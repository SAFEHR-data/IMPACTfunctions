# =============================================================================
# compute_pcas_final()
# -----------------------------------------------------------------------------
# PURPOSE
#   Turns raw PCA flowsheet rows into a tidy per-interval dataset with:
#     - medication (morphine / oxycodone / fentanyl only)
#     - inferred concentration (and mismatch flags)
#     - inferred bolus dose (per press) and background infusion (per hour)
#     - per-interval dose estimates from multiple sources (reservoir/admin/volume/model)
#     - a final "best estimate" dose per interval (and the basis used)
#
# HIGH-LEVEL FLOW
#   STEP 0: Map flowsheet questions -> medication and filter to {morphine, oxycodone, fentanyl}
#   STEP 1: Parse/classify flowsheet rows, collapse duplicates, pivot to wide rows per timestamp
#   STEP 2: Infer concentration from dose/volume and flag mismatches vs expected drug conc
#   STEP 2.5/2.6: Context-based recodes to fix obvious mislabeled meds due to conc patterns
#   STEP 3: Detect "new bag" events via reservoir refills
#   STEP 4: Detect pump resets via counter decreases
#   STEP 5: Compute incremental presses (n_given) between timestamps
#   STEP 6: Seed bolus dose at reset points (dose / presses)
#   STEP 7: Validate bolus dose using short forward windows (1–4 rows)
#   STEP 8: Fit bolus + background rate at each reset via small grid search
#   STEP 9: Segment-level background rate (reset-to-reset) from aggregate data
#   STEP 10: Per-interval dose components + reliability checks + final selection
#            then EWMA/±2SD gating to reject outlier candidates and choose dose_best_estimate
#
# =============================================================================

compute_pcas_final <- function(pcas_raw, phases_df = NULL, tz = "Europe/London") {
  
  suppressPackageStartupMessages({
    library(dplyr)
    library(tidyr)
    library(stringr)
    library(lubridate)
  })
  
  # ---------------------------------------------------------------------------
  # Tunable parameters controlling heuristics
  # ---------------------------------------------------------------------------
  near_full_threshold   <- 95    # reservoir volume (mL) treated as "near full"
  min_jump_ml           <- 20    # minimum positive jump in reservoir volume to call "new bag"
  priming_allowance_ml  <- 5     # allow up to 5mL of post-bag "priming" to be removed once
  zero_dose_eps_mg      <- 0.2   # dose treated as ~0 for mg drugs (morphine/oxycodone)
  zero_dose_eps_mcg     <- 4     # dose treated as ~0 for fentanyl (mcg)
  bg_ml_grid            <- c(0, 0.5, 1.0, 1.5, 2.0)  # candidate background rates (mL/hr) for grid search
  
  # ---------------------------------------------------------------------------
  # Small utilities used later (Steps 6–10)
  # ---------------------------------------------------------------------------
  `%||%` <- function(a, b) if (is.null(a)) b else a
  
  # Floors a number to a step size (e.g., 0.5 mg increments).
  # The +1e-9 avoids edge rounding issues where x is very close to an exact boundary.
  safe_floor_step <- function(x, step) {
    ifelse(is.na(x), NA_real_, floor((x + 1e-9) / step) * step)
  }
  
  # Floors the per-press bolus dose to plausible step sizes:
  #   fentanyl: 10 mcg increments
  #   morphine/oxycodone: 0.5 mg increments
  floor_step_by_med <- function(x, med) {
    m <- tolower(med %||% "")
    step <- ifelse(grepl("fentanyl", m), 10, 0.5)
    safe_floor_step(x, step)
  }
  
  # Upper bound for background mass rate:
  #   fentanyl: 40 mcg/hr
  #   others:   2 mg/hr
  upper_bg_rate_by_med <- function(med) {
    if (grepl("fentanyl", tolower(med))) 40 else 2
  }
  
  # Snaps a rate to nearest 0.5 mL/hr (because most pumps are set in half-ml steps).
  nearest_half_ml <- function(x) {
    ifelse(is.na(x), NA_real_, round(x / 0.5) * 0.5)
  }
  
  # Penalizes background rates away from the most common settings (0, 0.5, 1.0 mL/hr),
  # used as a soft tie-breaker in grid search.
  bg_pref_penalty <- function(mlhr) {
    prefs <- c(0, 0.5, 1.0)
    ifelse(is.na(mlhr), Inf,
           pmin(abs(mlhr - prefs[1]), abs(mlhr - prefs[2]), abs(mlhr - prefs[3])))
  }
  
  # Evidence test for whether 0.5mg bolus is plausible for mg-drugs:
  # If multiple intervals show dose/press ~0.5mg, allow 0.5 in candidate bolus set.
  has_half_mg_evidence <- function(press_vec, dose_vec) {
    ok <- which(is.finite(press_vec) & press_vec > 0 & is.finite(dose_vec) & dose_vec > 0)
    if (length(ok) < 2) return(FALSE)
    r <- dose_vec[ok] / press_vec[ok]
    sum(r >= 0.45 & r <= 0.55) >= 2 && mean(r >= 0.45 & r <= 0.55) >= 0.5
  }
  
  # ===========================================================================
  # STEP 0 (NEW): Derive medication from flowsheet text + filter out unwanted meds
  # ===========================================================================
  # Raw flowsheet includes many PCA/NCA items and other drug mixes (bupivacaine etc.).
  # We keep only:
  #   - morphine
  #   - oxycodone
  #   - fentanyl
  # And explicitly exclude:
  #   - diamorphine
  #   - bupivacaine / levobupivacaine (incl combos)
  #   - remifentanil
  #   - hydromorphone
  #
  # This must happen before STEP 1, because later grouping and inference assumes
  # medication is one of these three.
  pcas_raw <- pcas_raw %>%
    mutate(
      flowsheet_question_name = as.character(flowsheet_question_name),
      flowsheet_question      = as.character(flowsheet_question),
      
      # Build a single searchable text field from question name + question.
      fs_text = str_to_lower(paste0(
        coalesce(flowsheet_question_name, ""),
        " ",
        coalesce(flowsheet_question, "")
      )),
      
      # Hard exclude list (denylist) before mapping to the target drugs.
      excluded_drug = str_detect(
        fs_text,
        "diamorphine|bupivacaine|levobupivacaine|remifentanil|hydromorphone"
      ),
      
      # Map to the 3 drugs of interest (stored as lowercase strings here).
      medication = case_when(
        !excluded_drug & str_detect(fs_text, "morphine")  ~ "morphine",
        !excluded_drug & str_detect(fs_text, "oxycod")    ~ "oxycodone",
        !excluded_drug & str_detect(fs_text, "fent")      ~ "fentanyl",
        TRUE ~ NA_character_
      )
    ) %>%
    # Keep only the PCA meds of interest
    filter(!is.na(medication)) %>%
    # Remove temporary helper columns
    select(-fs_text, -excluded_drug)
  
  # ===========================================================================
  # STEP 1: Classify rows into PCA variables, parse times, collapse duplicates
  # ===========================================================================
  # Here we:
  #   1) Create a "var" label that says what each row represents
  #      (dose, volume, reservoir, attempts, given).
  #   2) Choose a timestamp for ordering (effective_time).
  #   3) Convert numeric_value to numeric and keep units.
  #   4) Collapse multiple entries at the same time/var into one value.
  pcas_long_keyed <- pcas_raw %>%
    mutate(
      admission_csn = as.character(admission_csn),
      mrn           = as.character(mrn),
      
      # Uppercase versions for pattern matching.
      qn  = str_to_upper(coalesce(flowsheet_question_name, "")),
      q   = str_to_upper(coalesce(flowsheet_question, "")),
      
      # Map each flowsheet question to a standardized variable name.
      var = case_when(
        str_detect(qn, "GIVEN VOLUME")        | str_detect(q, "GIVEN VOLUME")        ~ "volume_administered",
        str_detect(qn, "GIVEN AMOUNT")        | str_detect(q, "GIVEN AMOUNT")        ~ "dose_administered",
        str_detect(qn, "RESERVOIR VOLUME")    | str_detect(q, "RESERVOIR VOLUME")    ~ "reservoir_volume_remaining",
        str_detect(qn, "PCA DOSES GIVEN")     | str_detect(q, "PCA DOSES GIVEN")     ~ "n_given",
        str_detect(qn, "PCA DOSES ATTEMPTED") | str_detect(q, "PCA DOSES ATTEMPTED") ~ "n_attempts",
        TRUE ~ NA_character_
      ),
      
      # Parse timestamps robustly (works if already POSIXct or character).
      # effective_time prefers recorded_date; if missing, uses line_start.
      recorded_date = if (inherits(recorded_date, "POSIXt")) recorded_date else ymd_hms(recorded_date, tz = tz, quiet = TRUE),
      line_start    = if (inherits(line_start,    "POSIXt")) line_start    else ymd_hms(line_start,    tz = tz, quiet = TRUE),
      line_end      = if (inherits(line_end,      "POSIXt")) line_end      else ymd_hms(line_end,      tz = tz, quiet = TRUE),
      effective_time = coalesce(recorded_date, line_start),
      
      # Ensure numeric; keep unit only if non-empty.
      numeric_value = suppressWarnings(as.numeric(numeric_value)),
      unit_derived  = na_if(unit_derived, "")
    ) %>%
    # Only keep rows we could classify and that have a usable time.
    filter(!is.na(var)) %>%
    filter(!is.na(effective_time))
  
  # Helper: sum unique values (rounded) to avoid duplicate charting artifacts.
  # Used for dose_administered and volume_administered where multiple lines may exist.
  sum_distinct <- function(x, tol = 1e-6) {
    x <- x[is.finite(x)]
    if (!length(x)) return(NA_real_)
    ux <- unique(round(x, 6))
    if (length(ux) == 1) ux[1] else sum(ux)
  }
  
  # Collapse duplicates at the same (patient/admission/case/time/med/var).
  # Rules:
  #   - dose/volume: sum distinct values (often recorded as cumulative components)
  #   - reservoir: take the "latest" reading by (line_end or effective_time)
  #   - counters: take max (counters are cumulative; max is safest)
  pcas_collapsed <- pcas_long_keyed %>%
    group_by(mrn, admission_csn, case_id, effective_time, medication, var) %>%
    reframe(
      value = {
        vtype <- first(var)
        if (vtype %in% c("dose_administered", "volume_administered")) {
          sum_distinct(numeric_value)
        } else if (vtype == "reservoir_volume_remaining") {
          pick_time <- coalesce(line_end, effective_time)
          numeric_value[which.max(pick_time)]
        } else {
          if (all(is.na(numeric_value))) NA_real_ else max(numeric_value, na.rm = TRUE)
        }
      },
      # Choose one unit label for the collapsed row (last non-NA unit).
      units_var = {
        tmp <- unit_derived[!is.na(unit_derived)]
        if (length(tmp)) tail(tmp, 1) else NA_character_
      }
    )
  
  # Extract a single units value for each timestamp+med row after collapsing.
  units_by_group <- pcas_collapsed %>%
    transmute(mrn, admission_csn, effective_time, medication, units = units_var) %>%
    distinct(mrn, admission_csn, effective_time, medication, .keep_all = TRUE)
  
  # Pivot into one row per timestamp (date_time_recorded) and medication.
  # Result contains attempts, given, dose, volume, reservoir.
  pcas_wide_step1 <- pcas_collapsed %>%
    select(-units_var) %>%
    pivot_wider(
      names_from  = var,
      values_from = value
    ) %>%
    left_join(
      units_by_group,
      by = c("mrn", "admission_csn", "effective_time", "medication")
    ) %>%
    rename(
      date_time_recorded = effective_time
    ) %>%
    select(
      mrn, admission_csn, case_id, date_time_recorded, medication, units,
      n_attempts, n_given, dose_administered, volume_administered,
      reservoir_volume_remaining
    ) %>%
    # Require both counters present (original design requirement)
    filter(!is.na(n_attempts) & !is.na(n_given)) %>%
    distinct() %>%
    arrange(mrn, admission_csn, date_time_recorded)
  
  # ===========================================================================
  # STEP 2: Infer concentration + mismatch flag
  # ===========================================================================
  # We infer concentration from (dose_administered / volume_administered) after
  # converting the dose into the expected mass unit.
  #
  # expected_concentration:
  #   morphine  = 1 mg/mL
  #   oxycodone = 2 mg/mL
  #   fentanyl  = 20 mcg/mL
  #
  # concentration_mismatch_flag is TRUE when observed rounded concentration
  # disagrees with expected.
  pcas_wide_step2 <- pcas_wide_step1 %>%
    mutate(
      med_clean = str_to_lower(medication),
      
      expected_concentration = case_when(
        str_detect(med_clean, "morphine")  ~ 1,
        str_detect(med_clean, "oxycodone") ~ 2,
        str_detect(med_clean, "fentanyl")  ~ 20,
        TRUE ~ NA_real_
      ),
      
      PCA_concentration_unit = case_when(
        str_detect(med_clean, "fentanyl") ~ "mcg/mL",
        str_detect(med_clean, "morphine|oxycodone") ~ "mg/mL",
        TRUE ~ NA_character_
      ),
      
      # Guess dose units: prefer the supplied 'units' if it's mg/mcg.
      dose_unit_guess = case_when(
        !is.na(units) & units %in% c("mg", "mcg") ~ units,
        str_detect(med_clean, "fentanyl") ~ "mcg",
        str_detect(med_clean, "morphine|oxycodone") ~ "mg",
        TRUE ~ NA_character_
      ),
      
      # Convert dose to the expected mass unit if needed (mg<->mcg)
      dose_in_expected_unit = case_when(
        is.na(dose_administered) ~ NA_real_,
        PCA_concentration_unit == "mg/mL"  & dose_unit_guess == "mcg" ~ dose_administered / 1000,
        PCA_concentration_unit == "mcg/mL" & dose_unit_guess == "mg"  ~ dose_administered * 1000,
        TRUE ~ dose_administered
      ),
      
      # Observed concentration from dose/volume
      conc_obs = if_else(
        is.na(dose_in_expected_unit) | is.na(volume_administered) | volume_administered <= 0,
        NA_real_,
        dose_in_expected_unit / volume_administered
      ),
      
      # Round to nearest integer concentration (PCA concentrations are discrete)
      PCA_concentration = round(conc_obs),
      
      concentration_mismatch_flag = case_when(
        is.na(expected_concentration) | is.na(PCA_concentration) ~ NA,
        PCA_concentration != expected_concentration ~ TRUE,
        TRUE ~ FALSE
      )
    ) %>%
    select(
      mrn, admission_csn, case_id, date_time_recorded, medication, units,
      n_attempts, n_given, dose_administered, volume_administered, reservoir_volume_remaining,
      PCA_concentration, PCA_concentration_unit, expected_concentration, concentration_mismatch_flag
    )
  
  # ===========================================================================
  # STEP 2.5 + 2.6: Context recodes to fix mislabeled medication via patterns
  # ===========================================================================
  # These blocks try to correct medication labels when concentration implies a different
  # drug and there is evidence in the same admission that the "true" drug exists.
  #
  # 2.5: If mismatch + conc >= 15 and fentanyl appears elsewhere, recode to fentanyl.
  pcas_wide_step2a <- pcas_wide_step2 %>%
    group_by(mrn, admission_csn) %>%
    arrange(date_time_recorded, .by_group = TRUE) %>%
    mutate(
      med_lc   = str_to_lower(medication),
      is_fent  = str_detect(med_lc, "fentanyl"),
      fent_n         = sum(is_fent, na.rm = TRUE),
      fent_elsewhere = (fent_n - if_else(is_fent, 1L, 0L)) > 0,
      mis = coalesce(concentration_mismatch_flag, FALSE),
      to_fent_ctx = mis & is.finite(PCA_concentration) & PCA_concentration >= 15 & fent_elsewhere,
      
      # Keep originals for audit/debug
      medication_orig = medication,
      units_orig      = units,
      
      # Apply recode when context supports fentanyl
      medication             = if_else(to_fent_ctx, "Fentanyl", medication),
      units                  = if_else(to_fent_ctx, "mcg", units),
      PCA_concentration_unit = if_else(to_fent_ctx, "mcg/mL", PCA_concentration_unit)
    ) %>%
    ungroup() %>%
    select(-med_lc, -is_fent, -fent_n, -fent_elsewhere, -mis, -to_fent_ctx)
  
  # 2.6: If mismatch and conc==1 or 2, recode morphine<->oxycodone using neighbor rows
  #      and presence elsewhere in the admission.
  pcas_wide_step2b <- pcas_wide_step2a %>%
    group_by(mrn, admission_csn) %>%
    arrange(date_time_recorded, .by_group = TRUE) %>%
    mutate(
      med_lc    = str_to_lower(medication),
      is_morph  = str_detect(med_lc, "morphine"),
      is_oxy    = str_detect(med_lc, "oxycodone"),
      
      morph_n         = sum(is_morph, na.rm = TRUE),
      oxy_n           = sum(is_oxy,   na.rm = TRUE),
      morph_elsewhere = (morph_n - if_else(is_morph, 1L, 0L)) > 0,
      oxy_elsewhere   = (oxy_n   - if_else(is_oxy,   1L, 0L)) > 0,
      
      # Look at adjacent rows for medication continuity evidence
      prev_med       = lag(medication),
      next_med       = lead(medication),
      prev_is_morph  = str_detect(str_to_lower(coalesce(prev_med, "")), "morphine"),
      next_is_morph  = str_detect(str_to_lower(coalesce(next_med, "")), "morphine"),
      prev_is_oxy    = str_detect(str_to_lower(coalesce(prev_med, "")), "oxycodone"),
      next_is_oxy    = str_detect(str_to_lower(coalesce(next_med, "")), "oxycodone"),
      neighbor_morph = coalesce(prev_is_morph, FALSE) | coalesce(next_is_morph, FALSE),
      neighbor_oxy   = coalesce(prev_is_oxy,   FALSE) | coalesce(next_is_oxy,   FALSE),
      
      mis       = coalesce(concentration_mismatch_flag, FALSE),
      conc_calc = PCA_concentration,
      
      # Recode rules:
      #   - morphine labeled but conc==2 and oxy exists nearby/elsewhere -> Oxycodone
      #   - oxy labeled but conc==1 and morph exists nearby (or morph elsewhere without oxy elsewhere) -> Morphine
      to_oxy   = mis & is_morph & is.finite(conc_calc) & conc_calc == 2 & (neighbor_oxy | oxy_elsewhere),
      to_morph = mis & is_oxy   & is.finite(conc_calc) & conc_calc == 1 & (neighbor_morph | (morph_elsewhere & !oxy_elsewhere)),
      
      medication = case_when(
        to_oxy   ~ "Oxycodone",
        to_morph ~ "Morphine",
        TRUE     ~ medication
      ),
      
      # Update concentration units + expected conc after any recode
      PCA_concentration_unit = case_when(
        str_detect(str_to_lower(medication), "fentanyl") ~ "mcg/mL",
        str_detect(str_to_lower(medication), "morphine|oxycodone") ~ "mg/mL",
        TRUE ~ PCA_concentration_unit
      ),
      
      expected_concentration = case_when(
        str_detect(str_to_lower(medication), "morphine")  ~ 1,
        str_detect(str_to_lower(medication), "oxycodone") ~ 2,
        str_detect(str_to_lower(medication), "fentanyl")  ~ 20,
        TRUE ~ expected_concentration
      )
    ) %>%
    ungroup() %>%
    select(-med_lc, -is_morph, -is_oxy, -morph_n, -oxy_n,
           -morph_elsewhere, -oxy_elsewhere, -prev_med, -next_med,
           -prev_is_morph, -next_is_morph, -prev_is_oxy, -next_is_oxy,
           -neighbor_morph, -neighbor_oxy, -mis, -conc_calc, -to_oxy, -to_morph)
  
  # ===========================================================================
  # STEP 3: New bag detection (reservoir refill)
  # ===========================================================================
  # Identify likely "bag changes" when reservoir volume jumps upward by >= min_jump_ml
  # AND the resulting volume is near full.
  pcas_wide_step3 <- pcas_wide_step2b %>%
    group_by(mrn, admission_csn, medication) %>%
    arrange(date_time_recorded, .by_group = TRUE) %>%
    mutate(
      res_ml       = reservoir_volume_remaining,
      res_prev     = dplyr::lag(res_ml),
      res_delta    = res_ml - res_prev,
      first_in_grp = dplyr::row_number() == 1,
      new_bag = case_when(
        first_in_grp ~ TRUE,  # treat first row as "start of a bag"
        is.na(res_ml) | is.na(res_prev) ~ FALSE,
        res_delta >= min_jump_ml & res_ml >= near_full_threshold ~ TRUE,
        TRUE ~ FALSE
      )
    ) %>%
    ungroup() %>%
    select(-res_ml, -res_prev, -res_delta, -first_in_grp)
  
  # ===========================================================================
  # STEP 4: Pump reset detection via counter decreases
  # ===========================================================================
  # PCA counters (attempts/given) should be non-decreasing; when both drop, pump
  # was likely reset or a new run started (or counters re-zeroed).
  pcas_wide_step4 <- pcas_wide_step3 %>%
    group_by(mrn, admission_csn, medication) %>%
    arrange(date_time_recorded, .by_group = TRUE) %>%
    mutate(
      first_in_grp    = dplyr::row_number() == 1,
      n_attempts_prev = dplyr::lag(n_attempts),
      n_given_prev    = dplyr::lag(n_given),
      dn_attempts     = n_attempts - n_attempts_prev,
      dn_given        = n_given - n_given_prev,
      
      # reset only if BOTH decrease (stronger signal than either alone)
      both_decreased  = is.finite(dn_attempts) & is.finite(dn_given) &
        dn_attempts < 0 & dn_given < 0,
      
      pump_reset      = first_in_grp | both_decreased,
      pump_reset_reason = dplyr::case_when(
        first_in_grp   ~ "first_row",
        both_decreased ~ "both",
        TRUE           ~ NA_character_
      )
    ) %>%
    ungroup() %>%
    select(-first_in_grp, -n_attempts_prev, -n_given_prev, -dn_attempts, -dn_given, -both_decreased)
  
  # ===========================================================================
  # STEP 5: Incremental number of doses given (presses) per row
  # ===========================================================================
  # n_given is a cumulative counter. Convert to per-interval increments; after reset,
  # take the raw value as that interval’s increment.
  pcas_wide_step5 <- pcas_wide_step4 %>%
    group_by(mrn, admission_csn, medication) %>%
    arrange(date_time_recorded, .by_group = TRUE) %>%
    mutate(
      n_given_lag = dplyr::lag(n_given),
      incremental_n_given = case_when(
        pump_reset ~ as.numeric(n_given),
        is.na(n_given) ~ NA_real_,
        is.na(n_given_lag) ~ as.numeric(n_given),
        TRUE ~ as.numeric(n_given - n_given_lag)
      ),
      # guard: negative increments are invalid -> NA
      incremental_n_given = if_else(incremental_n_given < 0, NA_real_, incremental_n_given)
    ) %>%
    ungroup() %>%
    select(-n_given_lag) %>%
    relocate(incremental_n_given, .after = n_given)
  
  # ===========================================================================
  # STEP 6: Seed bolus dose at reset starts
  # ===========================================================================
  # For each reset row i, search forward to the first row j with n_given>0 and dose available,
  # then estimate bolus = dose_administered[j] / n_given[j].
  # Store it at row i (the reset row) as bolus_dose, plus a source offset.
  pcas_wide_step6 <- pcas_wide_step5 %>%
    group_by(mrn, admission_csn, medication) %>%
    arrange(date_time_recorded, .by_group = TRUE) %>%
    group_modify(~{
      df <- .x
      key <- .y
      n  <- nrow(df)
      df$bolus_dose <- NA_real_
      df$bolus_dose_source_offset <- NA_integer_
      med_name <- key$medication[[1]]
      for (i in seq_len(n)) {
        if (df$pump_reset_reason[i] %in% c("first_row","both")) {
          # find next row with positive press count
          j <- i
          while (j <= n && (is.na(df$n_given[j]) || df$n_given[j] <= 0)) j <- j + 1
          if (j <= n && !is.na(df$dose_administered[j]) && !is.na(df$n_given[j]) && df$n_given[j] > 0) {
            bd <- df$dose_administered[j] / df$n_given[j]
            # snap to plausible step size by drug
            df$bolus_dose[i] <- floor_step_by_med(bd, med_name)
            df$bolus_dose_source_offset[i] <- j - i
          }
        }
      }
      df
    }) %>%
    ungroup() %>%
    mutate(
      # bolus dose units depend on medication
      bolus_dose_unit = case_when(
        str_detect(str_to_lower(medication), "fentanyl") ~ "mcg",
        str_detect(str_to_lower(medication), "morphine|oxycodone") ~ "mg",
        TRUE ~ NA_character_
      )
    )
  
  # ===========================================================================
  # STEP 7: Validate bolus with short forward windows (1–4 rows)
  # ===========================================================================
  # For each reset, compute per-press dose over windows of 1..4 rows after reset.
  # Prefer reservoir-derived dose if reservoir drops exist; otherwise use admin totals.
  # Pick the most frequent rounded per-press value across windows.
  pcas_wide_step7 <- pcas_wide_step6 %>%
    group_by(mrn, admission_csn, medication) %>%
    arrange(date_time_recorded, .by_group = TRUE) %>%
    mutate(
      # recompute incremental presses (kept here because window logic uses it)
      n_given_lag = dplyr::lag(n_given),
      incremental_n_given = case_when(
        pump_reset ~ as.numeric(n_given),
        is.na(n_given) ~ NA_real_,
        is.na(n_given_lag) ~ as.numeric(n_given),
        TRUE ~ as.numeric(n_given - n_given_lag)
      ),
      incremental_n_given = if_else(incremental_n_given < 0, NA_real_, incremental_n_given)
    ) %>%
    group_modify(~{
      df  <- .x
      key <- .y
      n   <- nrow(df)
      df$bolus_dose_validated        <- df$bolus_dose
      df$bolus_dose_validated_window <- NA_integer_
      
      # reservoir drop between rows (positive drop means drug delivered)
      res   <- df$reservoir_volume_remaining
      drops <- c(NA_real_, res[-1] - res[-length(res)])
      drops <- -pmin(drops, 0)
      
      med_name <- key$medication[[1]]
      
      for (i in seq_len(n)) {
        if (df$pump_reset_reason[i] %in% c("first_row","both")) {
          wins_vals <- rep(NA_real_, 4)
          for (w in 1:4) {
            j_end <- min(i + w, n)
            if (j_end <= i) next
            rng <- seq.int(i + 1L, j_end)
            
            incr_sum <- sum(df$incremental_n_given[rng], na.rm = TRUE)
            
            # Reservoir-based dose over the window (drop_ml * concentration)
            dose_res <- sum(drops[rng] * df$PCA_concentration[rng], na.rm = TRUE)
            use_res  <- is.finite(dose_res) && dose_res > 0
            
            # Window dose totals: reservoir if available, else admin totals
            dose_sum <- if (use_res) dose_res else sum(df$dose_administered[rng], na.rm = TRUE)
            
            if (!is.na(incr_sum) && incr_sum > 0 && !is.na(dose_sum) && dose_sum > 0) {
              per_press <- dose_sum / incr_sum
              wins_vals[w] <- floor_step_by_med(per_press, med_name)
            }
          }
          
          # Choose the modal per-press value (ties broken by latest evidence)
          valid_idx <- which(!is.na(wins_vals))
          if (length(valid_idx) > 0) {
            tab <- table(wins_vals[valid_idx])
            top_vals <- as.numeric(names(tab)[tab == max(tab)])
            if (length(top_vals) > 1) {
              best_val <- top_vals[which.max(sapply(top_vals, function(v) max(which(wins_vals == v))))]
            } else best_val <- top_vals[1]
            df$bolus_dose_validated[i]        <- best_val
            df$bolus_dose_validated_window[i] <- max(which(wins_vals == best_val), na.rm = TRUE)
          }
        }
      }
      df
    }) %>%
    ungroup() %>%
    relocate(incremental_n_given, .after = n_given) %>%
    relocate(bolus_dose_validated, bolus_dose_validated_window, .after = bolus_dose)
  
  # ===========================================================================
  # STEP 8: Per-reset bolus/background fitting via grid search
  # ===========================================================================
  # Goal: for each reset row i, infer:
  #   - bolus_dose_final (mass per press)
  #   - background_rate_ml_hr (mL/hr)
  #   - background_rate_final (mass/hr)
  #
  # Uses up to 4 rows (or up to 2 hours / 4 presses) after reset and finds the
  # best (bolus, bg) pair minimizing absolute error against observed dose.
  #
  # Observed dose comes from:
  #   - reservoir drop * concentration if reservoir shows drops
  #   - otherwise incremental admin dose totals
  #
  # Includes extra logic:
  #   - priming removal (first interval after a new bag with “looks like priming” pattern)
  #   - “snap” to 1.0 or 1.5 mg in mg-drugs when ratios strongly suggest those
  #   - soft penalties to prefer common bg rates and continuity with previous reset bolus
  pcas_wide_step8 <- pcas_wide_step7 %>%
    group_by(mrn, admission_csn, medication) %>%
    arrange(date_time_recorded, .by_group = TRUE) %>%
    group_modify(~{
      df  <- .x
      key <- .y
      n   <- nrow(df)
      
      # Initialise outputs
      df$bolus_dose_final        <- df$bolus_dose_validated
      df$background_rate_final   <- NA_real_
      df$background_rate_ml_hr   <- NA_real_
      df$background_dose         <- NA_real_
      df$background_dose_unit    <- ifelse(grepl("fentanyl", tolower(key$medication[[1]])), "mcg/hr", "mg/hr")
      df$bolus_recalc_flag       <- FALSE
      df$bolus_recalc_basis      <- NA_character_
      df$bolus_recalc_window     <- NA_integer_
      
      # Precompute time deltas, reservoir drops, and incremental admin dose
      t        <- df$date_time_recorded
      dtH      <- c(NA_real_, as.numeric(diff(t), units = "hours"))
      res_prev <- dplyr::lag(df$reservoir_volume_remaining)
      drop_ml  <- ifelse(is.finite(res_prev) & is.finite(df$reservoir_volume_remaining),
                         pmax(res_prev - df$reservoir_volume_remaining, 0), NA_real_)
      dose_lag  <- dplyr::lag(df$dose_administered)
      incr_dose <- ifelse(df$pump_reset | is.na(dose_lag), df$dose_administered,
                          df$dose_administered - dose_lag)
      incr_dose <- ifelse(!is.na(incr_dose) & incr_dose < 0, df$dose_administered, incr_dose)
      
      med_name    <- key$medication[[1]]
      mg_drug     <- !grepl("fentanyl", tolower(med_name))
      bg_cap_mass <- upper_bg_rate_by_med(med_name)
      
      # Helper: end index for current reset segment (before next reset)
      next_reset_end <- function(i) {
        nxt <- which(df$pump_reset & seq_len(n) > i)
        if (length(nxt)) min(nxt) - 1L else n
      }
      
      for (i in seq_len(n)) {
        if (!(df$pump_reset_reason[i] %in% c("first_row","both"))) next
        
        j_limit <- next_reset_end(i)
        if (i >= j_limit) next
        
        # Choose a fitting window after reset:
        # extend forward until reaching any of: 4 rows, 2 hours, 4 presses.
        j_end <- i
        hours_sum <- 0; presses_sum <- 0; rows <- 0
        while (j_end < j_limit && rows < 4 && hours_sum < 2 && presses_sum < 4) {
          j_end <- j_end + 1L
          rng <- seq.int(i + 1L, j_end)
          hours_sum   <- sum(dtH[rng], na.rm = TRUE)
          presses_sum <- sum(df$incremental_n_given[rng], na.rm = TRUE)
          rows        <- length(rng)
        }
        if (j_end <= i) next
        rng <- seq.int(i + 1L, j_end)
        
        # Concentration reference for this window
        conc <- suppressWarnings(stats::median(df$PCA_concentration[rng], na.rm = TRUE))
        if (!is.finite(conc)) conc <- df$expected_concentration[i]
        if (!is.finite(conc) || conc <= 0) next
        
        # Observed dose per interval: reservoir-derived if any reservoir drops exist
        use_res   <- any(drop_ml[rng] > 0, na.rm = TRUE)
        dose_each <- if (use_res) drop_ml[rng] * df$PCA_concentration[rng] else incr_dose[rng]
        
        # Priming correction:
        # If first interval after new bag has no presses and minimal admin dose/volume,
        # treat up to priming_allowance_ml as priming, subtract it from reservoir-based dose.
        if (use_res) {
          new_bag_prev    <- dplyr::lag(df$new_bag)
          res_prev_inrng  <- dplyr::lag(df$reservoir_volume_remaining)
          prim_candidates <- which((!is.na(new_bag_prev[rng]) & new_bag_prev[rng]) &
                                     is.finite(res_prev_inrng[rng]) & res_prev_inrng[rng] >= near_full_threshold)
          if (length(prim_candidates) > 0) {
            k <- prim_candidates[1]
            prim_mg <- min(priming_allowance_ml * conc, dose_each[k], na.rm = TRUE)
            if (is.finite(prim_mg)) dose_each[k] <- pmax(0, dose_each[k] - prim_mg)
          }
        }
        
        press_each <- df$incremental_n_given[rng]
        hours_each <- dtH[rng]
        hours_sum   <- sum(hours_each, na.rm = TRUE)
        presses_sum <- sum(press_each, na.rm = TRUE)
        if (!is.finite(hours_sum) || hours_sum <= 0) next
        if (all(!is.finite(dose_each)) || sum(dose_each, na.rm = TRUE) <= 0) next
        
        # Background hints:
        # If interval has 0 presses but dose>0, infer background rate from that.
        bg_hint_ml <- NA_real_
        zero_idx <- which(is.finite(hours_each) & hours_each > 0 &
                            (is.na(press_each) | press_each == 0) &
                            is.finite(dose_each) & dose_each > 0)
        if (length(zero_idx) > 0) {
          bg_ml_est <- median((dose_each[zero_idx] / hours_each[zero_idx]) / conc, na.rm = TRUE)
          bg_hint_ml <- nearest_half_ml(bg_ml_est)
        }
        
        # If 0 presses and essentially 0 dose, cap background very low (discourage bg>0).
        dose_eps <- if (mg_drug) zero_dose_eps_mg else zero_dose_eps_mcg
        zero_nopress_nodose_idx <- which(
          is.finite(hours_each) & hours_each > 0 &
            (is.na(press_each) | press_each == 0) &
            (!is.finite(dose_each) | abs(dose_each) <= dose_eps)
        )
        bg_zero_cap <- if (length(zero_nopress_nodose_idx) > 0) { if (mg_drug) 0.10 else 2 } else { Inf }
        
        # Low-press hint:
        # If with ≤1 press intervals the residual implies ~0.5 mL/hr consistently, suggest bg=0.5.
        b_seed <- df$bolus_dose_validated[i]
        if (!is.finite(b_seed)) b_seed <- if (mg_drug) 1.0 else 20
        lp_idx <- which(is.finite(hours_each) & hours_each > 0 &
                          is.finite(press_each) & press_each <= 1 &
                          is.finite(dose_each))
        bg_ml_hint_lowpress <- NA_real_
        if (length(lp_idx) >= 2 && is.finite(conc) && conc > 0) {
          resid_ml_hr <- (dose_each[lp_idx] - b_seed * pmax(press_each[lp_idx], 0)) /
            (hours_each[lp_idx] * conc)
          resid_ml_hr <- resid_ml_hr[is.finite(resid_ml_hr) & resid_ml_hr >= 0]
          if (length(resid_ml_hr) >= 2) {
            med_lp  <- stats::median(resid_ml_hr, na.rm = TRUE)
            iqr_lp  <- stats::IQR(resid_ml_hr, na.rm = TRUE)
            snap_lp <- nearest_half_ml(med_lp)
            if (is.finite(snap_lp) && abs(snap_lp - 0.5) <= 0.15 && iqr_lp <= 0.3) {
              bg_ml_hint_lowpress <- 0.5
            }
          }
        }
        bg_ml_evidence <- dplyr::coalesce(bg_ml_hint_lowpress, bg_hint_ml)
        
        # Continuity prior: prefer bolus close to previous segment bolus
        prev_cands <- which(df$pump_reset & seq_len(n) < i)
        prev_idx   <- if (length(prev_cands) > 0) max(prev_cands) else NA_integer_
        prev_bolus <- if (!is.na(prev_idx)) df$bolus_dose_final[prev_idx] else NA_real_
        
        # Quick snap for mg drugs when press ratio strongly suggests 1.0 or 1.5 mg.
        if (mg_drug && presses_sum >= 3) {
          ok <- which(is.finite(press_each) & press_each > 0 & is.finite(dose_each) & dose_each > 0)
          if (length(ok) >= 2) {
            r_med <- stats::median(dose_each[ok] / press_each[ok], na.rm = TRUE)
            if (is.finite(r_med)) {
              snap_target <- if (abs(r_med - 1.5) <= 0.15) 1.5 else if (abs(r_med - 1.0) <= 0.15) 1.0 else NA_real_
              if (is.finite(snap_target)) {
                bg_mass <- pmax(0, (sum(dose_each, na.rm = TRUE) - snap_target * presses_sum) / hours_sum)
                bg_mass <- pmin(bg_mass, upper_bg_rate_by_med(med_name), snap_target)
                bg_mass <- pmin(bg_mass, bg_zero_cap)
                bg_ml   <- nearest_half_ml(bg_mass / conc)
                df$bolus_dose_final[i]      <- snap_target
                df$background_rate_ml_hr[i] <- bg_ml
                df$background_rate_final[i] <- bg_ml * conc
                df$background_dose[i]       <- df$background_rate_final[i]
                df$bolus_recalc_flag[i]     <- TRUE
                df$bolus_recalc_basis[i]    <- if (use_res) "reservoir" else "admin"
                df$bolus_recalc_window[i]   <- length(rng)
                next
              }
            }
          }
        }
        
        # Candidate bolus set:
        #   - mg drugs: typically {1, 1.5, 2} and optionally 0.5 if evidence exists
        #   - fentanyl: {10,20,30,40} mcg
        bolus_set <- if (mg_drug) {
          if (has_half_mg_evidence(press_each, dose_each)) c(0.5, 1.0, 1.5, 2.0) else c(1.0, 1.5, 2.0)
        } else c(10, 20, 30, 40)
        
        # Background rate candidates (mL/hr), optionally include hinted bg rate
        bg_ml_grid_use <- bg_ml_grid
        if (is.finite(bg_ml_evidence)) bg_ml_grid_use <- unique(c(bg_ml_evidence, bg_ml_grid_use))
        
        # Build evaluation grid of (bolus, bg_ml)
        ev <- expand.grid(bolus = bolus_set, bg_ml = bg_ml_grid_use, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
        ev$bg_mass     <- ev$bg_ml * conc
        
        # Apply caps to background:
        #   - per-drug cap (upper_bg_rate_by_med)
        #   - for mg drugs: also cap bg <= bolus (a conservative constraint)
        #   - bg_zero_cap if evidence of 0 bg
        ev$bg_mass_cap <- pmin(ev$bg_mass,
                               upper_bg_rate_by_med(med_name),
                               if (mg_drug) ev$bolus else Inf,
                               bg_zero_cap)
        
        # Weighting: downweight first interval if it looks like an outlier vs the median dose/press.
        w <- rep(1, length(dose_each))
        if (length(dose_each) >= 1 && is.finite(press_each[1]) && press_each[1] >= 5) {
          r1 <- dose_each[1] / press_each[1]
          r_ok <- which(is.finite(press_each) & press_each > 0 & is.finite(dose_each) & dose_each > 0)
          if (length(r_ok) >= 2) {
            r_med <- stats::median(dose_each[r_ok] / press_each[r_ok], na.rm = TRUE)
            if (is.finite(r1) && is.finite(r_med) && abs(r1 - r_med) > 0.4) w[1] <- 0.25
          }
        }
        
        # Fit error: L1 error between predicted and observed per-interval dose.
        ev$err <- vapply(seq_len(nrow(ev)), function(j) {
          b  <- ev$bolus[j]; bg <- ev$bg_mass_cap[j]
          fit_k <- b * press_each + bg * hours_each
          sum(w * abs(fit_k - dose_each), na.rm = TRUE)
        }, numeric(1))
        
        # Diagnostic: median implied per-press dose after subtracting background
        ev$pp_med <- vapply(seq_len(nrow(ev)), function(j) {
          bg <- ev$bg_mass_cap[j]
          pp <- (dose_each - bg * hours_each) / pmax(press_each, 1)
          ok <- is.finite(press_each) & press_each > 0 & is.finite(pp)
          if (!any(ok)) return(NA_real_)
          stats::median(pp[ok], na.rm = TRUE)
        }, numeric(1))
        
        # Diagnostic: mean absolute deviation of implied per-press vs candidate bolus
        ev$pp_mae <- vapply(seq_len(nrow(ev)), function(j) {
          b  <- ev$bolus[j]; bg <- ev$bg_mass_cap[j]
          pp <- (dose_each - bg * hours_each) / pmax(press_each, 1)
          ok <- is.finite(press_each) & press_each > 0 & is.finite(pp)
          if (!any(ok)) return(Inf)
          mean(abs(pp[ok] - b), na.rm = TRUE)
        }, numeric(1))
        
        # Soft tie-breakers / priors
        ev$bg_pref <- bg_pref_penalty(ev$bg_ml)
        ev$bg_hint_dist <- if (is.finite(bg_ml_evidence)) abs(ev$bg_ml - bg_ml_evidence) else Inf
        if (is.finite(prev_bolus)) {
          # allow +0.5 drift for mg drugs (common step)
          ev$bolus_prior_dist <- pmin(abs(ev$bolus - prev_bolus),
                                      abs(ev$bolus - (prev_bolus + 0.5)))
        } else ev$bolus_prior_dist <- Inf
        
        # mild penalty against bolus < 1mg in mg-drugs
        ev$bolus_pen <- if (mg_drug) ifelse(ev$bolus < 1, 1, 0) else 0
        
        # hard block example: discourage 2mg bolus if implied per-press is far lower
        hard_block <- if (mg_drug) ifelse(ev$bolus == 2 & is.finite(ev$pp_med) & ev$pp_med < 1.7, 1, 0) else 0
        ev$err <- ev$err + hard_block * 1e6
        
        # Choose the best candidate by ordered criteria
        ev <- ev[order(ev$err, ev$pp_mae, ev$bg_hint_dist, ev$bolus_prior_dist, ev$bg_pref, ev$bolus_pen, ev$bolus), ]
        choice <- ev[1, , drop = FALSE]
        
        # Save chosen parameters at the reset row i
        df$bolus_dose_final[i]      <- choice$bolus
        df$background_rate_ml_hr[i] <- as.numeric(choice$bg_ml)
        df$background_rate_final[i] <- as.numeric(choice$bg_mass_cap)
        df$background_dose[i]       <- as.numeric(choice$bg_mass_cap)
        df$bolus_recalc_flag[i]     <- TRUE
        df$bolus_recalc_basis[i]    <- if (use_res) "reservoir" else "admin"
        df$bolus_recalc_window[i]   <- length(rng)
      }
      df
    }) %>%
    ungroup() %>%
    relocate(bolus_dose_final, background_rate_final, background_rate_ml_hr, background_dose,
             background_dose_unit, bolus_recalc_flag, bolus_recalc_basis, bolus_recalc_window,
             .after = bolus_dose_validated_window)
  
  # ===========================================================================
  # STEP 9: Segment-level background (reset → next reset)
  # ===========================================================================
  # For each segment (defined by pump_reset), compute aggregate observed dose and
  # infer a background rate for the segment. This provides a "global" background
  # estimate that can override/assist the per-reset grid fit.
  pcas_wide_step9 <- pcas_wide_step8 %>%
    group_by(mrn, admission_csn, medication) %>%
    arrange(date_time_recorded, .by_group = TRUE) %>%
    mutate(
      segment_id        = cumsum(coalesce(pump_reset, FALSE)),
      row_in_segment    = dplyr::row_number(),
      interval_hours    = as.numeric(difftime(date_time_recorded, dplyr::lag(date_time_recorded), units = "hours")),
      res_prev          = dplyr::lag(reservoir_volume_remaining),
      res_drop          = pmax(res_prev - reservoir_volume_remaining, 0),
      dose_prev         = dplyr::lag(dose_administered),
      incremental_dose_admin = case_when(
        coalesce(pump_reset, FALSE) ~ dose_administered,
        is.na(dose_prev)            ~ dose_administered,
        TRUE                        ~ dose_administered - dose_prev
      ),
      incremental_dose_admin = if_else(!is.na(incremental_dose_admin) & incremental_dose_admin < 0,
                                       dose_administered, incremental_dose_admin)
    ) %>%
    group_by(mrn, admission_csn, medication, segment_id) %>%
    mutate(
      # Bolus used for segment inference (prefer final/validated/seeded bolus)
      bolus_for_segment = dplyr::first(coalesce(bolus_dose_final, bolus_dose_validated, bolus_dose)),
      
      # Aggregate quantities for the segment (excluding the first row in segment)
      seg_hours   = sum(interval_hours[row_in_segment > 1], na.rm = TRUE),
      seg_presses = sum(incremental_n_given[row_in_segment > 1], na.rm = TRUE),
      seg_dose_reservoir = sum((res_drop * PCA_concentration)[row_in_segment > 1], na.rm = TRUE),
      seg_dose_admin     = sum(incremental_dose_admin[row_in_segment > 1],          na.rm = TRUE),
      
      # Prefer reservoir dose if it's present and positive, else admin-based dose
      seg_use_reservoir  = is.finite(seg_dose_reservoir) & seg_dose_reservoir > 0,
      seg_obs_dose       = if_else(seg_use_reservoir, seg_dose_reservoir, seg_dose_admin),
      
      # Use median observed concentration across segment (fallback to expected)
      seg_conc_ref = {
        v <- suppressWarnings(stats::median(PCA_concentration[row_in_segment > 1], na.rm = TRUE))
        ifelse(is.finite(v) & !is.na(v), v, dplyr::first(expected_concentration))
      },
      
      # Expected bolus contribution from presses; remainder attributed to background
      seg_expected_bolus_dose = if_else(!is.na(bolus_for_segment), seg_presses * bolus_for_segment, NA_real_),
      
      seg_bg_mass_rate_raw = case_when(
        is.finite(seg_hours) & seg_hours > 0 & is.finite(seg_obs_dose) & seg_obs_dose > 0 & is.finite(seg_conc_ref) & seg_conc_ref > 0 ~
          if_else(seg_presses > 0,
                  pmax(0, (seg_obs_dose - seg_expected_bolus_dose) / seg_hours),
                  seg_obs_dose / seg_hours),
        TRUE ~ NA_real_
      ),
      
      # Convert background mass/hr -> mL/hr using concentration, then snap to 0.5mL/hr
      seg_bg_ml_hr        = seg_bg_mass_rate_raw / seg_conc_ref,
      seg_bg_ml_hr_round  = if_else(!is.na(seg_bg_ml_hr), round(seg_bg_ml_hr / 0.5) * 0.5, NA_real_),
      seg_bg_mass_rate    = seg_bg_ml_hr_round * seg_conc_ref,
      
      seg_bg_unit         = if_else(str_detect(str_to_lower(dplyr::first(medication)), "fentanyl"), "mcg/hr", "mg/hr"),
      seg_bg_applied      = !is.na(seg_bg_ml_hr_round) & seg_bg_ml_hr_round >= 0.5,
      seg_bg_reason       = if_else(seg_use_reservoir, "reservoir", "admin")
    ) %>%
    ungroup() %>%
    mutate(
      background_rate_seg_ml_hr = seg_bg_ml_hr_round,
      background_dose_seg       = seg_bg_mass_rate,
      background_dose_seg_unit  = seg_bg_unit,
      background_applied_seg    = seg_bg_applied,
      background_basis_seg      = seg_bg_reason
    ) %>%
    # Drop intermediate segment calc columns to keep the table compact
    select(-res_prev, -res_drop, -dose_prev,
           -seg_hours, -seg_presses, -seg_dose_reservoir, -seg_dose_admin, -seg_use_reservoir,
           -seg_obs_dose, -seg_conc_ref, -seg_expected_bolus_dose, -seg_bg_mass_rate_raw,
           -seg_bg_ml_hr, -seg_bg_ml_hr_round, -seg_bg_mass_rate, -seg_bg_unit, -seg_bg_applied, -seg_bg_reason)
  
  # ===========================================================================
  # STEP 10: Per-interval dose components + plausibility checks + selection
  # ===========================================================================
  # For each interval, compute candidate dose estimates:
  #   1) reservoir-derived mass (drop_ml * locked concentration) minus priming
  #   2) admin-derived mass (increment in dose_administered)
  #   3) volume-derived mass (increment in volume_administered * locked concentration)
  #   4) model-derived mass (bolus*presses + background*hours)
  #
  # Then:
  #   - apply per-bag feasibility limits (max mass + max mL)
  #   - flag reservoir unreliability (missing, increases, implausible long gaps, mismatch flags)
  #   - compute volume-model agreement mean (mv_mean)
  #   - pick a dose_best_estimate using a preference order,
  #     then run EWMA gating to reject outliers and choose final basis.
  #
  # NOTE: below block includes some explicit comments already in your script and retains them.
  pcas_wide_step10 <- pcas_wide_step9 %>%
    group_by(mrn, admission_csn, medication) %>%
    arrange(date_time_recorded, .by_group = TRUE) %>%
    mutate(
      interval_hours = as.numeric(difftime(date_time_recorded, dplyr::lag(date_time_recorded), units = "hours")),
      
      # Reservoir previous/current values for drops and missingness checks
      res_prev        = dplyr::lag(reservoir_volume_remaining),
      res_prev_missing  = is.na(res_prev),
      res_curr_missing  = is.na(reservoir_volume_remaining),
      reservoir_missing_prev_or_curr = res_prev_missing | res_curr_missing,
      
      # Reservoir drop is only meaningful if we have both prev and current.
      res_delta_ml    = if_else(!is.na(res_prev) & !is.na(reservoir_volume_remaining),
                                res_prev - reservoir_volume_remaining, NA_real_),
      res_drop_raw_ml = if_else(!is.na(res_delta_ml), pmax(res_delta_ml, 0), NA_real_),
      
      # Incremental admin dose (handles resets and negative diffs)
      dose_admin_prev  = dplyr::lag(dose_administered),
      dose_admin_incr  = if_else(pump_reset | is.na(dose_admin_prev),
                                 dose_administered,
                                 dose_administered - dose_admin_prev),
      dose_admin_incr  = if_else(!is.na(dose_admin_incr) & dose_admin_incr < 0,
                                 dose_administered, dose_admin_incr),
      
      # Incremental volume (mL) similarly
      vol_admin_prev    = dplyr::lag(volume_administered),
      vol_admin_incr_ml = if_else(pump_reset | is.na(vol_admin_prev),
                                  volume_administered,
                                  volume_administered - vol_admin_prev),
      vol_admin_incr_ml = if_else(!is.na(vol_admin_incr_ml) & vol_admin_incr_ml < 0,
                                  volume_administered, vol_admin_incr_ml),
      
      # Locked concentration by medication:
      # This deliberately ignores any inferred concentration variability to keep Step 10 stable.
      conc_locked = dplyr::case_when(
        stringr::str_detect(stringr::str_to_lower(medication), "fentanyl")  ~ 20,
        stringr::str_detect(stringr::str_to_lower(medication), "morphine")  ~ 1,
        stringr::str_detect(stringr::str_to_lower(medication), "oxycodone") ~ 2,
        TRUE ~ NA_real_
      ),
      is_fentanyl   = grepl("fentanyl", tolower(medication)),
      zero_dose_eps = if_else(is_fentanyl, zero_dose_eps_mcg, zero_dose_eps_mg),
      
      # Priming detection:
      # First interval after a new bag, with near-full reservoir and no presses/dose/volume,
      # treat some of the reservoir drop as priming and subtract it.
      is_after_new_bag = dplyr::lag(new_bag, default = FALSE),
      looks_like_priming =
        is_after_new_bag &
        !is.na(res_prev) & res_prev >= near_full_threshold &
        (is.na(incremental_n_given) | incremental_n_given == 0) &
        (is.na(dose_admin_incr)     | dose_admin_incr     <= zero_dose_eps) &
        (is.na(vol_admin_incr_ml)   | vol_admin_incr_ml   <= 0.5) &
        (!is.na(interval_hours)     & interval_hours <= 1.5),
      priming_ml      = dplyr::case_when(looks_like_priming ~ pmin(res_drop_raw_ml, priming_allowance_ml), TRUE ~ 0),
      res_drop_adj_ml = pmax(res_drop_raw_ml - priming_ml, 0),
      
      # Convert reservoir drop (mL) -> mass using locked concentration
      dose_reservoir_raw   = if_else(!is.na(res_drop_adj_ml) & !is.na(conc_locked),
                                     res_drop_adj_ml * conc_locked, NA_real_),
      # If reservoir is missing at either end, force reservoir dose to NA
      dose_reservoir_raw = dplyr::if_else(reservoir_missing_prev_or_curr, NA_real_, dose_reservoir_raw),
      
      # Admin mass and volume-derived mass
      dose_admin_mass_raw  = dose_admin_incr,
      dose_volume_mass_raw = if_else(!is.na(vol_admin_incr_ml) & !is.na(conc_locked),
                                     vol_admin_incr_ml * conc_locked, NA_real_),
      
      # Background mass/hr:
      # Prefer segment-level ml/hr if available, else per-reset bg ml/hr,
      # then convert to mass/hr using locked concentration.
      bg_ml_hr_active = dplyr::coalesce(background_rate_seg_ml_hr, background_rate_ml_hr),
      bg_mass_hr_active_locked = if_else(!is.na(bg_ml_hr_active) & !is.na(conc_locked),
                                         bg_ml_hr_active * conc_locked, NA_real_),
      bg_mass_hr_active = dplyr::coalesce(bg_mass_hr_active_locked, background_dose_seg, background_rate_final),
      
      # Model components:
      # bolus_active * presses + background_mass/hr * hours
      bolus_active = dplyr::coalesce(bolus_for_segment, bolus_dose_final, bolus_dose_validated),
      model_bolus_component      = if_else(!is.na(bolus_active) & !is.na(incremental_n_given),
                                           bolus_active * incremental_n_given, NA_real_),
      model_background_component = if_else(!is.na(bg_mass_hr_active) & !is.na(interval_hours),
                                           bg_mass_hr_active * interval_hours, NA_real_),
      dose_model_mass_raw = dplyr::coalesce(model_bolus_component, 0) +
        dplyr::coalesce(model_background_component, 0),
      dose_model_mass_raw = if_else(
        is.na(model_bolus_component) & is.na(model_background_component),
        NA_real_, dose_model_mass_raw
      ),
      
      # Per-bag feasibility limits:
      #   fentanyl bag ~2000 mcg, morphine ~100 mg, oxycodone ~200 mg.
      # Volume limit fixed at 100 mL/bag.
      bag_mass_limit = dplyr::case_when(
        is_fentanyl ~ 2000,
        stringr::str_detect(stringr::str_to_lower(medication), "morphine")  ~ 100,
        stringr::str_detect(stringr::str_to_lower(medication), "oxycodone") ~ 200,
        TRUE ~ NA_real_
      ),
      bag_vol_limit_ml = 100,
      
      # heuristic: allow multiple bags if the interval is long (≈1 bag per 8h gap)
      multi_bag_factor = dplyr::case_when(
        !is.finite(interval_hours) ~ 1,
        interval_hours <= 0        ~ 1,
        TRUE ~ ceiling(interval_hours / 8)
      ),
      
      allowed_mass_limit    = bag_mass_limit   * multi_bag_factor,
      allowed_vol_limit_ml  = bag_vol_limit_ml * multi_bag_factor,
      
      # Apply feasibility limits: if a candidate exceeds the plausible maximum,
      # set it to NA (treated as unusable).
      dose_reservoir = dplyr::case_when(
        reservoir_missing_prev_or_curr ~ NA_real_,
        !is.na(dose_reservoir_raw) &
          !is.na(allowed_mass_limit) &
          dose_reservoir_raw > 1.01 * allowed_mass_limit ~ NA_real_,
        TRUE ~ dose_reservoir_raw
      ),
      
      dose_admin_mass = dplyr::case_when(
        !is.na(dose_admin_mass_raw) &
          !is.na(allowed_mass_limit) &
          dose_admin_mass_raw > 1.01 * allowed_mass_limit ~ NA_real_,
        TRUE ~ dose_admin_mass_raw
      ),
      
      dose_volume_mass = dplyr::case_when(
        !is.na(dose_volume_mass_raw) &
          !is.na(allowed_mass_limit) &
          dose_volume_mass_raw > 1.01 * allowed_mass_limit ~ NA_real_,
        !is.na(vol_admin_incr_ml) &
          vol_admin_incr_ml > 1.01 * allowed_vol_limit_ml ~ NA_real_,
        TRUE ~ dose_volume_mass_raw
      ),
      
      # Apply feasibility limits to model-derived mass as well.
      dose_model_mass = dplyr::case_when(
        !is.na(dose_model_mass_raw) &
          !is.na(allowed_mass_limit) &
          dose_model_mass_raw > 1.01 * allowed_mass_limit ~ NA_real_,
        TRUE ~ dose_model_mass_raw
      ),
      
      # Reservoir reliability checks:
      #  - "zero now" (empty) can be real but also indicates end-of-bag noise
      #  - "increased" without a clear new bag is suspicious
      #  - "exceeds bag" and concentration mismatch also penalize reservoir
      res_is_zero_now = !is.na(reservoir_volume_remaining) &
        reservoir_volume_remaining <= 0.5,
      
      res_increased = !is.na(res_prev) & !is.na(reservoir_volume_remaining) &
        (reservoir_volume_remaining > res_prev + 0.5),
      
      not_obvious_bag = !(
        coalesce(new_bag, FALSE) |
          (
            reservoir_volume_remaining >= near_full_threshold &
              (reservoir_volume_remaining - coalesce(res_prev, 0)) >= min_jump_ml
          )
      ),
      
      reservoir_suspect = res_is_zero_now | (res_increased & not_obvious_bag),
      
      reservoir_exceeds_bag = !is.na(dose_reservoir_raw) &
        !is.na(bag_mass_limit) &
        (dose_reservoir_raw > 1.01 * bag_mass_limit),
      
      # Long-interval plausibility:
      # If over very long gaps, reservoir-implied rate is extremely low vs model,
      # treat reservoir as unreliable.
      res_rate = ifelse(
        is.finite(dose_reservoir) & is.finite(interval_hours) & interval_hours > 0,
        dose_reservoir / interval_hours,
        NA_real_
      ),
      
      model_rate = ifelse(
        is.finite(dose_model_mass) & is.finite(interval_hours) & interval_hours > 0,
        dose_model_mass / interval_hours,
        NA_real_
      ),
      
      abs_floor_rate_long = dplyr::case_when(
        is_fentanyl ~ 20,
        TRUE        ~ 0.5
      ),
      
      reservoir_implausibly_low_vs_model = ifelse(
        is.finite(res_rate) & is.finite(model_rate) &
          interval_hours > 12 &
          res_rate < 0.5 * model_rate,
        TRUE, FALSE
      ),
      
      reservoir_abs_too_low_long = ifelse(
        is.finite(res_rate) &
          interval_hours > 24 &
          res_rate < abs_floor_rate_long,
        TRUE, FALSE
      ),
      
      reservoir_plausibility_flag =
        reservoir_implausibly_low_vs_model |
        reservoir_abs_too_low_long,
      
      # Overall reservoir unreliability summary
      reservoir_unreliable = (
        reservoir_suspect |
          reservoir_exceeds_bag |
          reservoir_missing_prev_or_curr |
          coalesce(concentration_mismatch_flag, FALSE) |
          reservoir_plausibility_flag
      ),
      
      # Volume–Model agreement:
      # If volume-derived mass and model-derived mass agree within ±10%,
      # average them as an extra candidate (mv_mean).
      mv_agree = is.finite(dose_volume_mass) & is.finite(dose_model_mass) & {
        mv_mean_tmp <- (dose_volume_mass + dose_model_mass) / 2
        abs(dose_volume_mass - dose_model_mass) <= 0.10 * pmax(mv_mean_tmp, 1e-9)
      },
      mv_mean = dplyr::if_else(mv_agree,
                               (dose_volume_mass + dose_model_mass) / 2,
                               NA_real_),
      
      # If volume+model agree but both dwarf admin by >4x, treat that as suspicious and
      # prefer admin (often indicates cumulation/mis-entry in vol/model).
      mv_over_admin = mv_agree &
        is.finite(dose_admin_mass) & dose_admin_mass > 0 &
        (dose_volume_mass > 4 * dose_admin_mass) &
        (dose_model_mass  > 4 * dose_admin_mass),
      
      mv_mean_final = dplyr::if_else(mv_over_admin, NA_real_, mv_mean),
      
      # Preference helper: after reset or new bag, admin diffs are often the cleanest.
      prefer_admin_firstrow =
        dplyr::coalesce(new_bag, FALSE) |
        dplyr::coalesce(pump_reset, FALSE),
      
      # Initial selection before EWMA gating (this is later refined by the EWMA block)
      dose_best_estimate = dplyr::case_when(
        # 1) Right after a reset/new bag: trust admin if present
        prefer_admin_firstrow & !is.na(dose_admin_mass) ~ dose_admin_mass,
        
        # 2) If reservoir is unreliable: fall back to corroborated candidates
        reservoir_unreliable & mv_over_admin ~ dose_admin_mass,
        reservoir_unreliable ~ dplyr::coalesce(
          mv_mean_final, dose_admin_mass, dose_volume_mass, dose_model_mass, dose_reservoir
        ),
        
        # 3) Reservoir deemed fine: use reservoir first, then others
        !reservoir_unreliable & is.na(dose_reservoir) & mv_over_admin ~ dose_admin_mass,
        TRUE ~ dplyr::coalesce(
          dose_reservoir, mv_mean_final, dose_admin_mass, dose_volume_mass, dose_model_mass
        )
      )
    ) %>%
    ungroup()
  
  # ===========================================================================
  # EWMA/±2SD-gated final selection (post-processing on Step 10 candidates)
  # ===========================================================================
  # Purpose:
  #   Use a rolling EWMA model of dose/hour to reject implausible spikes (especially
  #   admin/volume spikes) unless corroborated.
  #
  # Core idea:
  #   - Maintain EWMA mean and EWMA second moment of *chosen* rate (dose/hour).
  #   - For each interval, compute candidate rates and discard those outside the band:
  #        mu ± max(2*sd, 0.25*|mu|, abs_floor_rate)
  #   - Then choose best estimate by an "inlier" logic:
  #        * if ≥2 inlier sources -> mean(inliers)
  #        * else fallback to the standard preference order (reservoir vs admin etc.)
  pcas_wide_step10 <- pcas_wide_step10 %>%
    dplyr::group_by(mrn, admission_csn, medication) %>%
    dplyr::arrange(date_time_recorded, .by_group = TRUE) %>%
    dplyr::group_modify(~{
      df  <- .x
      key <- .y
      n   <- nrow(df)
      if (n == 0) return(df)
      
      # Derive drug type from group key (avoid mutating df)
      med_key <- if ("medication" %in% names(key) && length(key$medication) && !is.na(key$medication[[1]])) {
        as.character(key$medication[[1]])
      } else NA_character_
      
      # EWMA smoothing: window ~5
      alpha <- 2 / (5 + 1)
      
      # Absolute rate floors (units depend on drug):
      #   fentanyl: 20 mcg/hr
      #   others:   0.5 mg/hr
      is_fent_group  <- isTRUE(grepl("fentanyl", tolower(med_key)))
      abs_floor_rate <- if (is_fent_group) 20 else 0.5
      rel_floor      <- 0.25
      
      if (!("dose_best_estimate" %in% names(df))) df$dose_best_estimate <- NA_real_
      if (!("dose_best_basis"    %in% names(df))) df$dose_best_basis    <- NA_character_
      
      # EWMA mean and EWMA second moment of chosen rate
      ema_x  <- NA_real_
      ema_x2 <- NA_real_
      has_prior <- function() is.finite(ema_x) && is.finite(ema_x2) && (ema_x2 - ema_x^2) >= 0
      
      # Fallback band state when EWMA isn't stable yet
      last_rate <- NA_real_
      
      to_hr <- function(m, ih) if (is.finite(m) && is.finite(ih) && ih > 0) m / ih else NA_real_
      
      for (i in seq_len(n)) {
        ih <- df$interval_hours[i]
        
        # Base candidates AFTER earlier caps/reliability decisions:
        # - reservoir candidate is dropped if reservoir_unreliable is TRUE
        res0 <- if (isTRUE(df$reservoir_unreliable[i])) NA_real_ else df$dose_reservoir[i]
        adm0 <- df$dose_admin_mass[i]
        vol0 <- df$dose_volume_mass[i]
        mod0 <- df$dose_model_mass[i]
        
        # Convert to rates (dose/hour) for gating comparisons
        res_r <- to_hr(res0, ih); adm_r <- to_hr(adm0, ih); vol_r <- to_hr(vol0, ih); mod_r <- to_hr(mod0, ih)
        
        # Pre-gate spike pruning:
        # If admin or volume rate is >3x the next-highest rate, drop that spike.
        rates  <- c(reservoir = res_r, admin = adm_r, volume = vol_r, model = mod_r)
        finite <- rates[is.finite(rates)]
        if (length(finite) >= 2) {
          ord <- sort(finite, decreasing = TRUE)
          if (ord[1] > 3 * ord[2] && names(ord)[1] %in% c("admin","volume")) {
            if (names(ord)[1] == "admin") { adm0 <- NA_real_; adm_r <- NA_real_ } else { vol0 <- NA_real_; vol_r <- NA_real_ }
            # optional second pass
            rates2  <- c(reservoir = res_r, admin = adm_r, volume = vol_r, model = mod_r)
            finite2 <- rates2[is.finite(rates2)]
            if (length(finite2) >= 2) {
              ord2 <- sort(finite2, decreasing = TRUE)
              if (ord2[1] > 3 * ord2[2] && names(ord2)[1] %in% c("admin","volume")) {
                if (names(ord2)[1] == "admin") { adm0 <- NA_real_; adm_r <- NA_real_ } else { vol0 <- NA_real_; vol_r <- NA_real_ }
              }
            }
          }
        }
        
        # If BOTH admin and volume are huge compared to reservoir/model, drop both.
        ref_rate <- suppressWarnings(max(c(res_r, mod_r), na.rm = TRUE))
        if (!is.finite(ref_rate)) ref_rate <- NA_real_
        if (is.finite(ref_rate) && is.finite(adm_r) && is.finite(vol_r)) {
          if (adm_r > 3 * ref_rate && vol_r > 3 * ref_rate) {
            adm0 <- NA_real_; adm_r <- NA_real_
            vol0 <- NA_real_; vol_r <- NA_real_
          }
        }
        
        # "agree_all" means multiple candidates are tightly consistent (±10%):
        all_vals <- c(res0, adm0, vol0, mod0)
        all_vals <- all_vals[is.finite(all_vals)]
        agree_all <- length(all_vals) >= 2 &&
          (max(all_vals) - min(all_vals)) <= 0.10 * max(mean(all_vals), 1e-9)
        
        # If admin+volume agree but dwarf reservoir/model, do not disable the gate.
        suspicious_av_pair <- is.finite(adm0) && is.finite(vol0) && (
          (is.finite(mod0) && adm0 > 3 * mod0 && vol0 > 3 * mod0) ||
            (is.finite(res0) && adm0 > 3 * res0 && vol0 > 3 * res0)
        )
        if (suspicious_av_pair) agree_all <- FALSE
        
        # Gate is active when:
        #   - not right after reset/new bag (prefer_admin_firstrow is FALSE)
        #   - not all candidates agree
        #   - we have a prior EWMA state
        gate_active <- !isTRUE(df$prefer_admin_firstrow[i]) &&
          !agree_all &&
          is.finite(ih) && ih > 0 &&
          has_prior()
        
        # ±2SD (or floor) band definition
        sd_mult   <- 2
        mu <- ema_x
        sd <- sqrt(pmax(ema_x2 - ema_x^2, 0))
        bw <- if (!is.finite(mu) || !is.finite(sd)) NA_real_ else
          max(sd_mult * sd, rel_floor * abs(mu), abs_floor_rate)
        
        band_ok <- function(xhr) {
          if (!gate_active) return(TRUE)
          if (!is.finite(xhr)) return(FALSE)
          # If band not formed, use last_rate if available.
          if (!is.finite(mu) || !is.finite(bw)) {
            if (is.finite(last_rate)) {
              bw_f <- max(rel_floor * abs(last_rate), abs_floor_rate)
              return(xhr >= (last_rate - bw_f) && xhr <= (last_rate + bw_f))
            } else {
              return(TRUE)
            }
          }
          (xhr >= (mu - bw)) && (xhr <= (mu + bw))
        }
        
        # Apply band to candidates
        res <- if (band_ok(res_r)) res0 else NA_real_
        adm <- if (band_ok(adm_r)) adm0 else NA_real_
        vol <- if (band_ok(vol_r)) vol0 else NA_real_
        mod <- if (band_ok(mod_r)) mod0 else NA_real_
        
        # If everything got gated out, fall back to pre-gate values (avoid empty row).
        if (all(!is.finite(c(res, adm, vol, mod)))) {
          res <- res0; adm <- adm0; vol <- vol0; mod <- mod0
        }
        
        # Recompute volume-model mean after gating
        mv_agree <- is.finite(vol) && is.finite(mod) &&
          abs(vol - mod) <= 0.10 * max((vol + mod)/2, 1e-9)
        mv_mean  <- if (mv_agree) (vol + mod)/2 else NA_real_
        mv_over_admin <- mv_agree && is.finite(adm) && adm > 0 &&
          (vol > 4 * adm) && (mod > 4 * adm)
        mv_mean_final <- if (mv_over_admin) NA_real_ else mv_mean
        
        # Inliers after gating
        inliers <- c(reservoir = res, admin = adm, volume = vol, model = mod)
        inliers <- inliers[is.finite(inliers)]
        have_2plus_inliers <- length(inliers) >= 2
        
        chosen <- NA_real_; basis <- NA_character_
        
        # Selection logic:
        #   - immediately after reset/new bag: admin first
        #   - if ≥2 inliers: mean(inliers)
        #   - else: fall back to reservoir vs admin/volume/model with reservoir_unreliable logic
        if (isTRUE(df$prefer_admin_firstrow[i]) && is.finite(df$dose_admin_mass[i])) {
          chosen <- df$dose_admin_mass[i]; basis <- "admin(first row)"
          
        } else if (have_2plus_inliers) {
          # special override: if only volume+model are inliers but both are huge vs admin, keep admin
          if (is.finite(adm) && is.finite(vol) && is.finite(mod) &&
              is.na(res) && (length(inliers) == 2) &&
              all(names(inliers) %in% c("volume","model")) &&
              (vol > 4*adm) && (mod > 4*adm)) {
            chosen <- adm; basis <- "admin(override: ≥2 inliers but vol+model >4× admin)"
          } else {
            chosen <- mean(inliers, na.rm = TRUE)
            basis  <- if (isTRUE(df$reservoir_unreliable[i])) "mean(≥2 inliers; reservoir unreliable)" else "mean(≥2 inliers)"
          }
          
        } else if (isTRUE(df$reservoir_unreliable[i])) {
          if (is.finite(mv_mean_final)) { chosen <- mv_mean_final; basis <- "mean(volume,model; reservoir unreliable)" }
          else if (is.finite(adm))      { chosen <- adm;          basis <- "admin(reservoir unreliable)" }
          else if (is.finite(vol))      { chosen <- vol;          basis <- "volume(reservoir unreliable)" }
          else if (is.finite(mod))      { chosen <- mod;          basis <- "model(reservoir unreliable)" }
          else if (is.finite(res))      { chosen <- res;          basis <- "reservoir(reservoir unreliable)" }
          
        } else {
          if (is.finite(res))               { chosen <- res;           basis <- ifelse(df$priming_ml[i] > 0, "reservoir(after priming)", "reservoir") }
          else if (is.finite(mv_mean_final)){ chosen <- mv_mean_final; basis <- "mean(volume,model)" }
          else if (is.finite(adm))          { chosen <- adm;           basis <- "admin" }
          else if (is.finite(vol))          { chosen <- vol;           basis <- "volume" }
          else if (is.finite(mod))          { chosen <- mod;           basis <- "model(bolus+background)" }
        }
        
        df$dose_best_estimate[i] <- chosen
        df$dose_best_basis[i]    <- basis
        
        # Update EWMA from the chosen rate (dose/hour)
        x <- if (is.finite(chosen) && is.finite(ih) && ih > 0) chosen / ih else NA_real_
        if (!is.finite(ema_x) || !is.finite(ema_x2)) {
          if (is.finite(x)) { ema_x <- x; ema_x2 <- x^2 }
        } else if (is.finite(x)) {
          ema_x  <- (1 - alpha) * ema_x  + alpha * x
          ema_x2 <- (1 - alpha) * ema_x2 + alpha * x^2
        }
        if (is.finite(x)) last_rate <- x
      }
      
      df
    }) %>%
    dplyr::ungroup()
  
  # ===========================================================================
  # FINAL SELECT: output table of the main fields used downstream
  # ===========================================================================
  out <- pcas_wide_step10 %>%
    select(
      mrn, admission_csn, medication,
      date_time_recorded, interval_hours, units,
      PCA_concentration, PCA_concentration_unit, concentration_mismatch_flag,
      new_bag, pump_reset_reason,
      bolus_dose_final,
      background_dose = background_dose,
      background_dose_unit = background_dose_unit,
      bolus_recalc_basis, bolus_dose_unit,
      dose_reservoir, dose_admin_mass, dose_volume_mass, dose_model_mass,
      dose_best_estimate, dose_best_basis
    ) %>%
    arrange(mrn, admission_csn, date_time_recorded)
  
  # ===========================================================================
  # Optional: patch the first interval_hours using phases_df post-op anchor
  # ===========================================================================
  # If phases_df is provided, we compute a post-op anchor per admission and use it
  # to fill interval_hours for the very first row in that admission when interval_hours
  # is missing or non-positive.
  if (!is.null(phases_df)) {
    
    postop_anchor_tbl <- phases_df %>%
      mutate(
        in_recovery         = if (inherits(in_recovery,         "POSIXt")) in_recovery         else ymd_hms(in_recovery,         tz = tz, quiet = TRUE),
        patient_out_of_room = if (inherits(patient_out_of_room, "POSIXt")) patient_out_of_room else ymd_hms(patient_out_of_room, tz = tz, quiet = TRUE),
        anaesthesia_end     = if (inherits(anaesthesia_end,     "POSIXt")) anaesthesia_end     else ymd_hms(anaesthesia_end,     tz = tz, quiet = TRUE),
        postop_anchor_raw   = coalesce(in_recovery, patient_out_of_room, anaesthesia_end),
        admission_csn       = as.character(admission_csn)
      ) %>%
      group_by(admission_csn) %>%
      summarise(
        postop_anchor = {
          if (all(is.na(postop_anchor_raw))) {
            as.POSIXct(NA)
          } else {
            min(postop_anchor_raw, na.rm = TRUE)
          }
        },
        .groups = "drop"
      )
    
    out <- out %>%
      left_join(postop_anchor_tbl, by = "admission_csn") %>%
      group_by(mrn, admission_csn) %>%
      arrange(date_time_recorded, .by_group = TRUE) %>%
      mutate(
        is_first_overall_in_adm = row_number() == 1L,
        interval_from_anchor = as.numeric(difftime(date_time_recorded, postop_anchor, units = "hours")),
        interval_from_anchor = if_else(is.finite(interval_from_anchor) & interval_from_anchor < 0, 0, interval_from_anchor),
        interval_hours = if_else(
          is_first_overall_in_adm &
            (is.na(interval_hours) | interval_hours <= 0) &
            is.finite(interval_from_anchor),
          interval_from_anchor,
          interval_hours
        )
      ) %>%
      ungroup() %>%
      select(-is_first_overall_in_adm, -interval_from_anchor, -postop_anchor)
  }
  
  out
}




# =============================================================================
# build_pcas_clean_flag()
# -----------------------------------------------------------------------------
# PURPOSE
#   Expands the interval-level PCA output (pcas_final) into hourly buckets and
#   flags each hour as having a valid MAR prescription window match.
#
#   Output is one row per:
#     (mrn, admission_csn, medication, time_hourly_start, time_hourly_end)
#   plus:
#     - dose_best_estimate_hourly (dose distributed evenly across the row’s window)
#     - valid_prescription (TRUE/FALSE) based on MAR timing windows
#     - matched_orders (comma-separated order_id list when matched)
#     - still_duplicated (TRUE/FALSE) if multiple meds remain in same hour bucket
#
# HIGH-LEVEL FLOW
#   0) Build MAR "validity windows" for PCA meds (IV only) from mar_df
#   1) Mark first PCA row per admission (to allow theatre-anchored long first interval)
#   2) Expand each PCA row into hourly buckets (using row_start_time → row_end_time)
#   3) Match hourly PCA buckets to MAR windows (mrn + medication)
#   4) Clean duplicate-hour buckets:
#        - drop explicit zero-dose duplicates
#        - if any row in the hour is valid, keep only valid rows in that hour
# =============================================================================

build_pcas_clean_flag <- function(pcas_final,
                                  mar_df,
                                  tz = "Europe/London",
                                  new_admin_gap_hours = 48) {
  
  suppressPackageStartupMessages({
    require(dplyr)
    require(tidyr)
    require(stringr)
    require(purrr)
    require(lubridate)
  })
  
  # ---------------------------------------------------------------------------
  # Sanity checks: ensure required columns exist in inputs
  # ---------------------------------------------------------------------------
  req_pcas_cols <- c(
    "mrn","admission_csn","medication","date_time_recorded",
    "dose_best_estimate","dose_best_basis","units",
    "concentration_mismatch_flag",
    "interval_hours"              # used for first-row anchoring logic
  )
  miss_pcas <- setdiff(req_pcas_cols, names(pcas_final))
  if (length(miss_pcas)) {
    stop("pcas_final missing columns: ", paste(miss_pcas, collapse=", "))
  }
  
  req_mar_cols <- c(
    "mrn","case_id","admission_csn","anaesthesia_csn",
    "operation_csn","order_id",
    "simple_generic_name","generic_name","medication_name",
    "administration_date_time","stop_time",
    "administration_route","pca_medication_flag"
  )
  
  miss_mar <- setdiff(req_mar_cols, names(mar_df))
  if (length(miss_mar)) {
    warning("mar_df is missing some optional columns; attempting to continue. Missing: ",
            paste(miss_mar, collapse=", "))
  }
  
  # ===========================================================================
  # 0) Build MAR validity windows
  # ===========================================================================
  # Idea:
  #   For each MAR PCA administration event (IV, flagged as PCA med), create a
  #   window [time_valid, time_valid_end] where we consider the PCA prescription
  #   "active/valid". We later match hourly PCA buckets to these windows.
  #
  # Implementation details:
  #   - medication_raw is coalesced across name fields
  #   - administration_instant uses administration_date_time (your table column)
  #   - discharge_date is not available => set to NA
  #   - time_valid is floored to hour of (administration_instant - 1 hour)
  #   - time_valid_end is min of:
  #         * next administration + 1 hour (ceiled)
  #         * stop_time + 1 hour (ceiled)
  #         * discharge_date (NA here)
  #     with NA handled safely via numeric min + Inf control
  mar_check <- mar_df %>%
    transmute(
      mrn,
      case_id,
      admission_csn,
      anaesthesia_csn,
      operation_csn,   # NOTE: your table uses operation_csn
      order_id,
      
      # Use whichever medication name field is populated
      medication_raw = dplyr::coalesce(simple_generic_name, generic_name, medication_name),
      
      # Parse admin + stop timestamps in local tz
      administration_instant = suppressWarnings(lubridate::as_datetime(administration_date_time, tz = tz)),
      stop_time = suppressWarnings(lubridate::as_datetime(stop_time, tz = tz)),
      
      # Not present in your post_op_mar; keep as NA for window end calc
      discharge_date = as.POSIXct(NA, tz = tz),
      
      administration_route,
      pca_medication_flag
    ) %>%
    # Keep only PCA meds, IV route, and rows with a timestamp
    filter(
      pca_medication_flag == "Yes",
      administration_route == "Intravenous",
      !is.na(administration_instant)
    ) %>%
    # Map MAR medication text -> standardized medication labels
    mutate(
      medication = case_when(
        str_detect(str_to_lower(medication_raw), "morphine") ~ "Morphine",
        str_detect(str_to_lower(medication_raw), "oxycod")   ~ "Oxycodone",
        str_detect(str_to_lower(medication_raw), "fent")     ~ "Fentanyl",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(medication)) %>%
    # Start of validity: 1 hour before admin, floored to the hour
    mutate(time_valid = floor_date(administration_instant - hours(1), "hour")) %>%
    arrange(mrn, case_id, admission_csn, anaesthesia_csn, administration_instant) %>%
    # Next admin time for the same patient/admission (not medication-specific)
    group_by(mrn, admission_csn) %>%
    mutate(next_any_admin = lead(administration_instant)) %>%
    ungroup() %>%
    # Candidate end times: ceil to hour and add 1 hour buffer
    mutate(
      cand_next_admin_end = ceiling_date(next_any_admin + hours(1), "hour"),
      cand_stop_end       = ceiling_date(stop_time + hours(1),     "hour"),
      cand_discharge_end  = ceiling_date(discharge_date,           "hour")
    ) %>%
    # Choose the minimum non-NA end time robustly:
    #   pmin(..., na.rm=TRUE) returns Inf if all are NA, which we convert to NA.
    mutate(
      time_valid_end_num = pmin(
        as.numeric(cand_next_admin_end),
        as.numeric(cand_stop_end),
        as.numeric(cand_discharge_end),
        na.rm = TRUE
      ),
      time_valid_end = as_datetime(
        ifelse(is.infinite(time_valid_end_num), NA_real_, time_valid_end_num),
        tz = tz
      )
    ) %>%
    select(
      mrn, case_id, admission_csn, anaesthesia_csn, operation_csn, order_id,
      medication, medication_raw, administration_instant,
      time_valid, time_valid_end
    )
  
  # ===========================================================================
  # 1) Tag first PCA row per admission (used to allow a long first interval)
  # ===========================================================================
  # compute_pcas_final may have theatre-anchored interval_hours for the first row.
  # We keep a flag and also carry the first interval_hours for potential use later.
  pcas_final_aug <- pcas_final %>%
    arrange(mrn, admission_csn, date_time_recorded) %>%
    group_by(mrn, admission_csn) %>%
    mutate(
      first_row_overall_in_adm       = (row_number() == 1L),
      first_interval_hours_in_adm    = interval_hours[1]
    ) %>%
    ungroup()
  
  # ===========================================================================
  # 2) Expand PCA rows to hourly buckets
  # ===========================================================================
  # For each PCA row, define a "row window":
  #   row_start_time  → row_end_time (hour-ceiled end)
  #
  # row_start_time rules:
  #   - If this row is the first PCA row in the admission, and interval_hours is
  #     sane (>0 and <=36), use a long backward anchor:
  #         date_time_recorded - interval_hours
  #   - If there's a large gap since previous PCA record (>new_admin_gap_hours),
  #     treat as a new admin episode: start 1 hour before current record
  #   - Otherwise: start at prev_time (previous PCA record time)
  #
  # Then floor row_start_time to an hour boundary.
  pcas_with_row_start <- pcas_final_aug %>%
    group_by(mrn, admission_csn, medication) %>%
    arrange(date_time_recorded, .by_group = TRUE) %>%
    mutate(
      prev_time = dplyr::lag(date_time_recorded),
      gap_hours = as.numeric(
        difftime(date_time_recorded, prev_time, units = "hours")
      ),
      new_admin = is.na(prev_time) | gap_hours > new_admin_gap_hours,
      
      # Only allow long anchor for the very first PCA row in admission,
      # and cap it at 36h to avoid bizarre backfills.
      use_long_anchor = first_row_overall_in_adm &
        is.finite(interval_hours) &
        interval_hours > 0 &
        interval_hours <= 36,
      
      row_start_time_raw = dplyr::case_when(
        use_long_anchor ~ date_time_recorded - lubridate::dhours(interval_hours),
        new_admin       ~ date_time_recorded - hours(1),
        TRUE            ~ prev_time
      ),
      
      # Align to exact hour
      row_start_time = lubridate::floor_date(row_start_time_raw, unit = "hour")
    ) %>%
    ungroup() %>%
    select(-row_start_time_raw, -use_long_anchor)
  
  # Define row_end_time as the next hour boundary at/after the record timestamp.
  # start_end_window_hours is the length of the PCA row window.
  # hourly_dose_best_estimate spreads the interval dose across its hour buckets.
  pcas_with_row_window <- pcas_with_row_start %>%
    mutate(
      row_end_time = lubridate::ceiling_date(
        date_time_recorded,
        unit = "hour",
        change_on_boundary = FALSE
      ),
      start_end_window_hours = as.numeric(
        difftime(row_end_time, row_start_time, units = "hours")
      ),
      hourly_dose_best_estimate = dplyr::case_when(
        is.na(dose_best_estimate)         ~ NA_real_,
        is.na(start_end_window_hours)     ~ NA_real_,
        start_end_window_hours <= 0       ~ NA_real_,
        TRUE                              ~ dose_best_estimate / start_end_window_hours
      )
    )
  
  # Expand each row window into an explicit list of hour start times
  # and unnest into one row per hour.
  pcas_hourly_expanded <- pcas_with_row_window %>%
    mutate(
      row_start_time = as_datetime(row_start_time, tz = tz),
      row_end_time   = as_datetime(row_end_time,   tz = tz),
      n_hours = ifelse(
        is.finite(start_end_window_hours) & start_end_window_hours > 0,
        as.integer(round(start_end_window_hours)),
        1L
      ),
      n_hours = pmax(1L, n_hours),
      hour_starts = purrr::map2(
        row_start_time, n_hours,
        ~ .x + hours(0:(.y - 1))
      )
    ) %>%
    tidyr::unnest(hour_starts) %>%
    transmute(
      mrn, admission_csn, medication, units,
      date_time_recorded,
      time_hourly_start = hour_starts,
      time_hourly_end   = hour_starts + hours(1),
      dose_best_estimate_hourly = hourly_dose_best_estimate,
      dose_best_basis,
      concentration_mismatch_flag
    ) %>%
    arrange(mrn, admission_csn, medication, time_hourly_start)
  
  # ===========================================================================
  # 3) MAR matching
  # ===========================================================================
  # Build a lightweight matching table and join by (mrn, medication).
  # A hit occurs when the hour bucket END falls within [time_valid, time_valid_end].
  mar_for_match <- mar_check %>%
    transmute(
      mrn, medication, order_id,
      time_valid     = as.POSIXct(time_valid,     tz = tz),
      time_valid_end = as.POSIXct(time_valid_end, tz = tz)
    )
  
  # Add a row id so we can collapse matches and join back safely
  pcas_with_id <- pcas_hourly_expanded %>%
    mutate(.row_id = dplyr::row_number())
  
  # For each hourly PCA row, determine if it matches any MAR window.
  hits_mrn <- pcas_with_id %>%
    left_join(mar_for_match, by = c("mrn", "medication")) %>%
    mutate(
      hit = !is.na(order_id) &
        !is.na(time_hourly_end) & !is.na(time_valid) & !is.na(time_valid_end) &
        time_hourly_end >= time_valid & time_hourly_end <= time_valid_end
    ) %>%
    group_by(.row_id) %>%
    summarise(
      valid_prescription = any(hit, na.rm = TRUE),
      matched_orders = {
        ids <- unique(order_id[hit])
        if (length(ids) == 0 || all(is.na(ids))) NA_character_ else paste(ids, collapse = ",")
      },
      .groups = "drop"
    )
  
  # Join validity flags back to the hourly table
  pcas_hourly_flagged <- pcas_with_id %>%
    left_join(hits_mrn, by = ".row_id") %>%
    mutate(
      valid_prescription = coalesce(valid_prescription, FALSE),
      matched_orders     = ifelse(valid_prescription, matched_orders, NA_character_)
    ) %>%
    select(-.row_id)
  
  # ===========================================================================
  # 4) Duplicate-hour cleaning
  # ===========================================================================
  # Duplicate-hour buckets happen when more than one medication lands in the same
  # (mrn/admission/hour_start/hour_end) bucket.
  #
  # Cleaning strategy:
  #   - Identify duplicate buckets
  #   - Drop rows with explicit 0 dose inside duplicates (keep NA doses)
  #   - If any row in a given MRN+hour is valid by MAR, keep only valid rows in that hour
  #   - Return cleaned dataset and still_duplicated flag
  pcas_hourly_work <- pcas_hourly_flagged %>%
    mutate(.row_id = dplyr::row_number())
  
  dupe_key <- c("mrn", "admission_csn", "time_hourly_start", "time_hourly_end")
  
  dupe_buckets <- pcas_hourly_work %>%
    group_by(across(all_of(dupe_key))) %>%
    filter(dplyr::n() > 1) %>%
    ungroup()
  
  non_dupe_rows <- anti_join(pcas_hourly_work, dupe_buckets, by = ".row_id")
  
  # Step 1: within duplicates, drop explicit zero-dose rows (but keep NA doses)
  dupes_step1_keep <- dupe_buckets %>%
    filter(is.na(dose_best_estimate_hourly) | dose_best_estimate_hourly != 0)
  
  dupes_step1_drop <- anti_join(dupe_buckets, dupes_step1_keep, by = ".row_id") %>%
    mutate(drop_reason = "zero-dose in duplicate")
  
  # Step 2: if any row is MAR-valid in an MRN+hour bucket (ignoring admission),
  # keep only valid rows in that bucket.
  alt_dupe_key <- c("mrn", "time_hourly_start", "time_hourly_end")
  
  dupes_step2_keep <- dupes_step1_keep %>%
    group_by(across(all_of(alt_dupe_key))) %>%
    mutate(any_valid = any(coalesce(valid_prescription, FALSE))) %>%
    filter(!any_valid | coalesce(valid_prescription, FALSE)) %>%
    ungroup() %>%
    select(-any_valid)
  
  dupes_step2_drop <- anti_join(dupes_step1_keep, dupes_step2_keep, by = ".row_id") %>%
    mutate(drop_reason = "invalid prescription in hour (another row valid)")
  
  # Combine non-duplicate rows + cleaned duplicate rows
  pcas_hourly_cleaned <- bind_rows(
    non_dupe_rows %>% select(-.row_id),
    dupes_step2_keep %>% select(-.row_id)
  ) %>%
    arrange(mrn, admission_csn, medication, time_hourly_start, time_hourly_end)
  
  # Flag remaining duplicates after cleaning
  pcas_hourly_cleaned_flag <- pcas_hourly_cleaned %>%
    group_by(across(all_of(dupe_key))) %>%
    mutate(still_duplicated = dplyr::n() > 1) %>%
    ungroup()
  
  # Final output
  pcas_clean_flag <- pcas_hourly_cleaned_flag
  return(pcas_clean_flag)
}



# =============================================================================
# resolve_crossover_duplicates()
# -----------------------------------------------------------------------------
# PURPOSE
#   Resolves remaining duplicate-hour buckets where ≥2 medications overlap in the
#   SAME hour for the same patient/admission. This is meant to handle "crossover"
#   periods when one PCA is being stopped and another started, or charting overlap.
#
#   The function:
#     - Finds hours where ≥2 meds exist
#     - Within each hour keeps only the top-2 meds (by dose then priority)
#     - Groups overlapping hours into "episodes" (contiguous, with small-gap bridging)
#     - Allows the first `allowance_hours` of overlap to remain
#     - Beyond the allowance band, chooses a winner med for the episode and drops
#       the losing med’s rows in the beyond-band hours (and optionally inside-band)
#
# KEY PARAMETERS
#   allowance_hours: total overlap hours allowed per episode before trimming
#   const_run_hours: if a med shows a long constant run in the beyond-band, penalize it
#   med_priority: tie-breaker ordering (highest priority wins ties)
#   bridge_hours: treat small gaps inside an episode as still same episode
#   drop_on_equal_allowance: if TRUE, trim when seg_len >= allowance_hours (else >)
#   drop_within_allowance: if TRUE, also drop the loser within the allowed band hours
# =============================================================================

resolve_crossover_duplicates <- function(
    pcas_clean_flag,
    allowance_hours         = 4L,                      # total duplicate HOURS allowed per episode
    const_run_hours         = 6L,                      # ">5 hours" == 6+
    med_priority            = c("Oxycodone","Fentanyl","Morphine"),
    bridge_hours            = 2L,                      # allow small gaps INSIDE a duplicate episode
    drop_on_equal_allowance = FALSE,                   # trim when seg_len == allowance_hours
    drop_within_allowance   = FALSE                    # also drop loser INSIDE the allowed hours
) {
  suppressPackageStartupMessages({
    library(dplyr); library(tidyr); library(lubridate); library(purrr); library(stringr); library(tibble)
  })
  
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
  
  # ---------------------------------------------------------------------------
  # Prep: add a row id, define a working dose value, and mark "active" hours
  # ---------------------------------------------------------------------------
  df <- pcas_clean_flag %>%
    mutate(
      .row_id   = dplyr::row_number(),
      dose_val  = dose_best_estimate_hourly,
      is_active = is.finite(dose_val) & dose_val > 0
    )
  
  # Determine ID keys present (at least one of mrn/admission_csn must exist)
  id_keys <- intersect(names(df), c("mrn","admission_csn"))
  if (length(id_keys) == 0L) stop("Need at least one of {mrn, admission_csn} in input.")
  dupe_key <- c(id_keys, "time_hourly_start", "time_hourly_end")
  
  # ---------------------------------------------------------------------------
  # Identify duplicate hours where ≥2 meds exist in the same hour bucket
  # ---------------------------------------------------------------------------
  dup_hours <- df %>%
    group_by(across(all_of(dupe_key))) %>%
    mutate(
      n_in_bucket    = dplyr::n(),
      n_meds_bucket  = dplyr::n_distinct(medication)
    ) %>%
    ungroup() %>%
    filter(n_in_bucket > 1, n_meds_bucket >= 2)
  
  # If none, just return input with still_duplicated recalculated
  if (nrow(dup_hours) == 0L) {
    return(
      df %>%
        select(-.row_id, -dose_val, -is_active) %>%
        group_by(across(all_of(dupe_key))) %>%
        mutate(still_duplicated = dplyr::n() > 1) %>%
        ungroup()
    )
  }
  
  # ---------------------------------------------------------------------------
  # Priority table: higher prio numeric means higher preference
  # (med_priority is ordered highest→lowest by your argument; we reverse seq to match)
  # ---------------------------------------------------------------------------
  prio_tbl <- tibble(medication = med_priority, prio = rev(seq_along(med_priority)))
  
  # ---------------------------------------------------------------------------
  # Within each duplicate hour, keep only the top-2 meds:
  #   - primary sort: dose_val (descending)
  #   - tie-break: priority (descending)
  # We store the med pair as a canonical string "A|B" (alphabetical) for grouping.
  # ---------------------------------------------------------------------------
  dup_top2 <- dup_hours %>%
    left_join(prio_tbl, by = "medication") %>%
    mutate(prio = coalesce(prio, 0L)) %>%
    group_by(across(all_of(dupe_key))) %>%
    arrange(desc(dose_val), desc(prio), .by_group = TRUE) %>%
    slice_head(n = 2) %>%
    ungroup() %>%
    group_by(across(all_of(dupe_key))) %>%
    mutate(pair_meds = paste(sort(unique(medication)), collapse = "|")) %>%
    ungroup()
  
  # One row per hour per patient/admission and med-pair
  pair_by_hour <- dup_top2 %>%
    distinct(across(all_of(c(id_keys, "time_hourly_start", "pair_meds")))) %>%
    arrange(across(all_of(c(id_keys, "time_hourly_start"))))
  
  # ---------------------------------------------------------------------------
  # Build duplicate "episodes" by joining contiguous duplicate hours for same pair
  # with gap-bridging:
  #   - if time gap between dup hours > bridge_hours, start a new segment
  # Each episode yields:
  #   - dupe_times: list of hour start times
  #   - seg_len: number of duplicated hours
  #   - allow_k: how many duplicated hours we allow before trimming
  #   - band_times: duplicated hours beyond the allowed band (to potentially drop)
  # ---------------------------------------------------------------------------
  episodes <- pair_by_hour %>%
    group_by(across(all_of(c(id_keys, "pair_meds")))) %>%
    arrange(time_hourly_start, .by_group = TRUE) %>%
    mutate(
      prev_t    = lag(time_hourly_start),
      gap_h     = as.numeric(difftime(time_hourly_start, prev_t, units = "hours")),
      break_seg = if_else(is.na(prev_t) | gap_h > bridge_hours, 1L, 0L),
      seg_id    = cumsum(break_seg)
    ) %>%
    group_by(across(all_of(c(id_keys, "pair_meds", "seg_id")))) %>%
    summarise(
      dupe_times = list(sort(unique(time_hourly_start))),
      seg_start  = min(dupe_times[[1]]),
      seg_end    = max(dupe_times[[1]]),
      seg_len    = length(dupe_times[[1]]),
      .groups    = "drop"
    ) %>%
    mutate(
      # allowed overlap hours in this episode (capped by allowance_hours)
      allow_k    = pmax(1L, pmin(seg_len, as.integer(allowance_hours))),
      
      # decide if trimming is needed (> allowance, or >= allowance depending on flag)
      needs_trim = if (drop_on_equal_allowance) seg_len >= allowance_hours else seg_len > allowance_hours,
      
      # cutpoint_time is the last allowed duplicate hour start (kth time)
      cutpoint_time = pmap(list(dupe_times, allow_k), function(ts, k) {
        if (length(ts) == 0 || is.na(k) || k < 1 || k > length(ts)) as.POSIXct(NA) else ts[[k]]
      }),
      
      # band_times are duplicate hour starts beyond allow_k (these are the ones trimmed)
      band_times = pmap(list(dupe_times, allow_k), function(ts, k) {
        if (length(ts) == 0 || is.na(k) || k >= length(ts)) as.POSIXct(character(0)) else ts[(k+1):length(ts)]
      }),
      
      # allowed_times inside the allowance band (only used if drop_within_allowance==TRUE)
      allowed_times = pmap(list(dupe_times, allow_k), function(ts, k) {
        if (!isTRUE(drop_within_allowance)) return(as.POSIXct(character(0)))
        if (length(ts) == 0 || is.na(k) || k < 1) as.POSIXct(character(0)) else ts[seq_len(min(length(ts), k))]
      })
    )
  
  # If no episodes, return with still_duplicated recomputed
  if (nrow(episodes) == 0L) {
    return(
      df %>%
        select(-.row_id, -dose_val, -is_active) %>%
        group_by(across(all_of(dupe_key))) %>%
        mutate(still_duplicated = dplyr::n() > 1) %>%
        ungroup()
    )
  }
  
  # ---------------------------------------------------------------------------
  # Decide winner per episode (beyond-band trimming only)
  # ---------------------------------------------------------------------------
  # For each episode:
  #   - Identify the two meds (med_a, med_b)
  #   - Look at doses in the beyond-band hours (band_times)
  #   - Penalize a med if it has a long constant run in beyond-band
  #   - Prefer the med that continues further AFTER the episode end (active hours)
  #   - If tie, use med_priority
  #   - If still tie, use total dose within entire episode window
  #
  # Output: a tibble listing which med to drop and at which hour starts.
  decide_one <- function(row_tbl) {
    meds <- strsplit(row_tbl$pair_meds %||% "", "\\|")[[1]]
    if (length(meds) < 2) return(NULL)
    med_a <- meds[1]; med_b <- meds[2]
    
    # subset df to this patient/admission
    sub <- df
    for (kk in id_keys) {
      vv <- row_tbl[[kk]][[1]]
      sub <- sub[!is.na(sub[[kk]]) & sub[[kk]] == vv, , drop = FALSE]
    }
    
    seg_start  <- row_tbl$seg_start[[1]]
    seg_end    <- row_tbl$seg_end[[1]]
    band_times <- row_tbl$band_times[[1]]
    
    # Only act when trimming needed and there are beyond-band hours
    if (!isTRUE(row_tbl$needs_trim[[1]]) || length(band_times) == 0) return(NULL)
    
    # Doses on the beyond-band duplicate hours only
    band_df <- sub %>%
      filter(medication %in% c(med_a, med_b),
             time_hourly_start %in% band_times) %>%
      select(medication, time_hourly_start, dose_val)
    
    a_band <- band_df$dose_val[band_df$medication == med_a]
    b_band <- band_df$dose_val[band_df$medication == med_b]
    
    # "constant run" penalty in beyond-band:
    # if a med has ≥const_run_hours of finite values AND all are essentially the same,
    # treat it as suspicious/undesirable for continuation.
    a_const <- (sum(is.finite(a_band)) >= const_run_hours) && (dplyr::n_distinct(a_band[is.finite(a_band)]) <= 1L)
    b_const <- (sum(is.finite(b_band)) >= const_run_hours) && (dplyr::n_distinct(b_band[is.finite(b_band)]) <= 1L)
    
    # Which med continues further beyond the episode end?
    last_future_time <- function(mm) {
      ss <- sub %>%
        filter(medication == mm, is_active, time_hourly_start > seg_end)
      if (nrow(ss) == 0) as.POSIXct(NA) else max(ss$time_hourly_start, na.rm = TRUE)
    }
    end_a <- last_future_time(med_a)
    end_b <- last_future_time(med_b)
    
    # Priority values (default 0 if not present)
    pa <- prio_tbl$prio[match(med_a, prio_tbl$medication)]; pa <- ifelse(is.na(pa), 0L, pa)
    pb <- prio_tbl$prio[match(med_b, prio_tbl$medication)]; pb <- ifelse(is.na(pb), 0L, pb)
    
    # Tie-breaker: total dose over the entire duplicate episode window
    win_win <- sub %>%
      filter(medication %in% c(med_a, med_b),
             time_hourly_start >= seg_start,
             time_hourly_start <= seg_end)
    suma <- sum(win_win$dose_val[win_win$medication == med_a], na.rm = TRUE)
    sumb <- sum(win_win$dose_val[win_win$medication == med_b], na.rm = TRUE)
    
    # Winner selection hierarchy:
    keep_med <-
      if (a_const && !b_const) med_b else
        if (b_const && !a_const) med_a else
          if (isTRUE(end_a > end_b)) med_a else
            if (isTRUE(end_b > end_a)) med_b else
              if (pa > pb) med_a else if (pb > pa) med_b else
                if (isTRUE(suma >= sumb)) med_a else med_b
    
    drop_med <- if (keep_med == med_a) med_b else med_a
    
    id_vals <- row_tbl %>% dplyr::select(all_of(id_keys)) %>% as.list()
    
    tibble(
      !!!id_vals,
      pair_meds = row_tbl$pair_meds[[1]],
      seg_start = seg_start,
      seg_end   = seg_end,
      keep_med  = keep_med,
      drop_med  = drop_med,
      # drop beyond-band hours for the losing med
      times_to_drop        = list(band_times),
      # optionally also drop inside-band hours for the losing med
      times_to_drop_inside = list(if (isTRUE(drop_within_allowance)) row_tbl$allowed_times[[1]] else as.POSIXct(character(0)))
    )
  }
  
  # Map decide_one over episodes and bind results
  decisions <- episodes %>%
    mutate(.idx = dplyr::row_number()) %>%
    split(.$.idx) %>%
    purrr::map(~decide_one(as_tibble(.x %>% select(-.idx)))) %>%
    purrr::compact() %>%
    dplyr::bind_rows()
  
  # If no trimming decisions, return input with still_duplicated recomputed
  if (nrow(decisions) == 0L) {
    return(
      df %>%
        select(-.row_id, -dose_val, -is_active) %>%
        group_by(across(all_of(dupe_key))) %>%
        mutate(still_duplicated = dplyr::n() > 1) %>%
        ungroup()
    )
  }
  
  # ---------------------------------------------------------------------------
  # Build drop list: (id_keys + medication + time_hourly_start) pairs to remove
  # ---------------------------------------------------------------------------
  drops_outer <- decisions %>%
    select(all_of(id_keys), medication = drop_med, times_to_drop) %>%
    tidyr::unnest(times_to_drop, keep_empty = FALSE) %>%
    rename(time_hourly_start = times_to_drop)
  
  drops_inner <- decisions %>%
    select(all_of(id_keys), medication = drop_med, times_to_drop_inside) %>%
    tidyr::unnest(times_to_drop_inside, keep_empty = FALSE) %>%
    rename(time_hourly_start = times_to_drop_inside)
  
  drops <- bind_rows(drops_outer, drops_inner)
  
  # If nothing to drop, return as-is with still_duplicated recomputed
  if (nrow(drops) == 0L) {
    return(
      df %>%
        select(-.row_id, -dose_val, -is_active) %>%
        group_by(across(all_of(dupe_key))) %>%
        mutate(still_duplicated = dplyr::n() > 1) %>%
        ungroup()
    )
  }
  
  # Join keys needed to identify rows for removal
  join_keys <- c(id_keys, "medication", "time_hourly_start")
  
  # Identify row_ids that match the drop specifications
  to_drop <- df %>%
    inner_join(drops, by = join_keys) %>%
    pull(.row_id)
  
  # Return filtered df with duplication flag recalculated
  df %>%
    filter(!.row_id %in% to_drop) %>%
    select(-.row_id, -dose_val, -is_active) %>%
    group_by(across(all_of(dupe_key))) %>%
    mutate(still_duplicated = dplyr::n() > 1) %>%
    ungroup()
}

# =============================================================================
# summarise_postop_bins()
# -----------------------------------------------------------------------------
# PURPOSE
#   Summarise deduplicated hourly PCA dosing into post-op time bins (e.g. 0–24h,
#   24–48h, ...), and compute OME (oral morphine equivalent) totals per bin.
#
# INPUTS
#   pcas_clean_flag_deduplicated:
#     Hourly PCA table (one row per hour per med) that includes:
#       - mrn, admission_csn, medication, units
#       - time_hourly_start, time_hourly_end
#       - dose_best_estimate_hourly (dose assigned to that hour)
#
#   phases:
#     Table containing perioperative timing fields used to define the post-op anchor:
#       - patient_out_of_room, in_recovery, anaesthesia_end
#
#   bins_hours:
#     Numeric vector defining the bin boundaries (in hours post-op).
#     Example: c(0,24,48,72,...) produces labels like "0-24h", "24-48h", etc.
#
#   ome_conv:
#     Lookup table mapping (medication, units) -> ome_per_unit conversion factor.
#     The function multiplies dose_sum * ome_per_unit to create dose_OME per bin.
#
# OUTPUT
#   A wide table with one row per (mrn, admission_csn, medication), containing:
#     - postop_anchor (the anchor timestamp used for binning)
#     - dose_* columns for each bin (sum of hourly doses within bin)
#     - ome_*  columns for each bin (sum converted to OME within bin)
#
# IMPORTANT DETAIL
#   Binning is done using hours_postop_end computed from time_hourly_end
#   (i.e., the END of the hour bucket). This makes each hourly dose belong to the
#   bin corresponding to the hour’s ending time after surgery.
# =============================================================================

summarise_postop_bins <- function(
    pcas_clean_flag_deduplicated,
    phases,
    tz = "Europe/London",
    bins_hours = c(0, 24, 48, 72, 96, 120, 146),
    ome_conv = tibble::tribble(
      ~medication, ~units, ~ome_per_unit,
      "Morphine",  "mg",   3.0,
      "Oxycodone", "mg",   4.5,
      "Fentanyl",  "mcg",  0.30
    )
) {
  suppressPackageStartupMessages({
    library(dplyr)
    library(tidyr)
    library(lubridate)
  })
  
  # ---------------------------------------------------------------------------
  # Helper: safe character key for joining on admission_csn
  # ---------------------------------------------------------------------------
  # admission_csn can be stored as numeric-like IDs; converting directly to
  # character sometimes yields scientific notation, which breaks joins.
  # norm_key() forces stable string formatting for numeric IDs.
  norm_key <- function(x) {
    if (is.numeric(x)) format(x, scientific = FALSE, trim = TRUE, digits = 22) else as.character(x)
  }
  
  # -------------------------------------------------------------------------
  # 1) Build a post-op anchor per admission
  # -------------------------------------------------------------------------
  # We build ONE anchor timestamp per admission (admission_csn_key).
  #
  # Priority within each phases row:
  #   patient_out_of_room -> in_recovery -> anaesthesia_end
  #
  # Then for each admission we take the earliest non-NA anchor across rows.
  anchor_tbl <- phases %>%
    mutate(
      admission_csn_key   = norm_key(admission_csn),
      patient_out_of_room = suppressWarnings(as.POSIXct(patient_out_of_room, tz = tz)),
      in_recovery         = suppressWarnings(as.POSIXct(in_recovery,         tz = tz)),
      anaesthesia_end     = suppressWarnings(as.POSIXct(anaesthesia_end,     tz = tz)),
      row_anchor          = coalesce(patient_out_of_room, in_recovery, anaesthesia_end)
    ) %>%
    group_by(admission_csn_key) %>%
    summarise(
      postop_anchor = {
        # "v" is all candidate anchors for this admission across rows.
        # If none exist, keep NA; otherwise pick the earliest timestamp.
        v <- row_anchor[!is.na(row_anchor)]
        if (length(v) == 0L) as.POSIXct(NA, tz = tz) else min(v)
      },
      .groups = "drop"
    )
  
  # -------------------------------------------------------------------------
  # 2) Join anchor onto PCA hourly rows and compute hours post-op
  # -------------------------------------------------------------------------
  # We compute hours_postop_end using time_hourly_end (end of the hour bucket),
  # then restrict rows to within the maximum requested bin boundary.
  max_bin <- max(bins_hours, na.rm = TRUE)
  
  df <- pcas_clean_flag_deduplicated %>%
    mutate(
      admission_csn_key  = norm_key(admission_csn),
      time_hourly_start  = suppressWarnings(as.POSIXct(time_hourly_start, tz = tz)),
      time_hourly_end    = suppressWarnings(as.POSIXct(time_hourly_end,   tz = tz))
    ) %>%
    left_join(anchor_tbl, by = "admission_csn_key") %>%
    mutate(
      # Hours since surgery anchor, using the END of the hour bucket.
      # Example: the hour 10:00–11:00 is counted at 11:00.
      hours_postop_end = as.numeric(difftime(time_hourly_end, postop_anchor, units = "hours"))
    ) %>%
    filter(
      # Keep only rows where we have a valid anchor and a valid time difference
      !is.na(postop_anchor),
      !is.na(hours_postop_end),
      # Exclude hour buckets that end before/at the anchor
      hours_postop_end > 0,
      # Restrict to requested maximum bin horizon
      hours_postop_end <= max_bin
    )
  
  # -------------------------------------------------------------------------
  # 3) Convert continuous hours into bins (factor labels like "0-24h")
  # -------------------------------------------------------------------------
  # bin_labels are built from consecutive boundaries:
  #   (bins_hours[1], bins_hours[2]) -> "0-24h", etc.
  bin_labels <- paste0(head(bins_hours, -1), "-", tail(bins_hours, -1), "h")
  
  df_binned <- df %>%
    mutate(
      bin = cut(
        hours_postop_end,
        breaks = bins_hours,
        include.lowest = FALSE,  # 0 is excluded because we filtered hours_postop_end > 0
        right = TRUE,            # intervals are (a, b] i.e. include the right endpoint
        labels = bin_labels
      )
    ) %>%
    # Drop anything that fell outside the breaks (safety)
    filter(!is.na(bin))
  
  # -------------------------------------------------------------------------
  # 4) Sum dose within each (patient/admission/med/bin) and compute OME
  # -------------------------------------------------------------------------
  # dose_sum: total dose across all hours in that bin
  # dose_OME: dose_sum multiplied by ome_per_unit lookup (if available)
  dose_with_ome <- df_binned %>%
    group_by(mrn, admission_csn, medication, units, admission_csn_key, postop_anchor, bin) %>%
    summarise(
      dose_sum = sum(dose_best_estimate_hourly, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(ome_conv, by = c("medication", "units")) %>%
    mutate(
      dose_OME = if_else(is.na(ome_per_unit), NA_real_, dose_sum * ome_per_unit)
    )
  
  # -------------------------------------------------------------------------
  # 5) Wide outputs: one row per (mrn, admission_csn, medication)
  # -------------------------------------------------------------------------
  # We generate:
  #   dose_0-24h, dose_24-48h, ...
  #   ome_0-24h,  ome_24-48h,  ...
  # and keep the postop_anchor used.
  dose_wide <- dose_with_ome %>%
    select(mrn, admission_csn, medication, admission_csn_key, postop_anchor, bin, dose_sum) %>%
    pivot_wider(names_from = bin, values_from = dose_sum, names_prefix = "dose_")
  
  ome_wide <- dose_with_ome %>%
    select(mrn, admission_csn, medication, admission_csn_key, postop_anchor, bin, dose_OME) %>%
    pivot_wider(names_from = bin, values_from = dose_OME, names_prefix = "ome_")
  
  out <- dose_wide %>%
    left_join(
      ome_wide,
      by = c("mrn", "admission_csn", "medication", "admission_csn_key", "postop_anchor")
    ) %>%
    arrange(mrn, admission_csn, medication)
  
  out
}


# =============================================================================
# summarise_postop_days()
# -----------------------------------------------------------------------------
# PURPOSE
#   Summarise deduplicated hourly PCA dosing into "post-op days" where:
#     - day_0 runs from the post-op anchor time → next 08:00
#     - day_1..day_7 run as 08:00 → 08:00 windows
#
#   Produces:
#     1) per-medication table with dose_day_* and ome_day_* columns
#     2) per-admission table with OME totals collapsed across medications
#
# INPUTS
#   pcas_clean_flag_deduplicated:
#     Hourly PCA table that includes (at minimum):
#       - mrn, admission_csn, medication, units
#       - time_hourly_start
#       - dose_best_estimate_hourly
#
#   phases:
#     Table containing anchor candidates:
#       - patient_out_of_room, in_recovery, anaesthesia_end
#
#   *_iv_factor:
#     Per-unit conversion factors used to compute OME from IV doses:
#       - Morphine mg  -> mg OME via morphine_iv_factor
#       - Oxycodone mg -> mg OME via oxycodone_iv_factor
#       - Fentanyl mcg -> mg OME via fentanyl_iv_factor
#
# OUTPUT
#   A list with:
#     $days_per_med:
#       One row per (mrn, admission_csn, medication), columns:
#         dose_day_0 ... dose_day_7
#         ome_day_0  ... ome_day_7
#         postop_anchor
#     $days_ome_by_admission:
#       One row per (mrn, admission_csn), columns:
#         ome_day_0 ... ome_day_7 (summed across meds)
#         ome_total_0to7
#         postop_anchor
#
# IMPORTANT DETAIL
#   Assignment to day windows uses time_hourly_start (start of the hour bucket),
#   and requires time_hourly_start >= start AND < end.
# =============================================================================

summarise_postop_days <- function(pcas_clean_flag_deduplicated,
                                  phases,
                                  tz = "Europe/London",
                                  oxycodone_iv_factor = 4.5,  # IV oxycodone → OME
                                  morphine_iv_factor  = 3.0,  # IV morphine  → OME
                                  fentanyl_iv_factor  = 0.3   # mcg fentanyl → mg OME
) {
  
  # ---------------------------------------------------------------------------
  # 1) Build one postop anchor per admission
  # ---------------------------------------------------------------------------
  # For each row in phases, define a candidate anchor:
  #   patient_out_of_room -> in_recovery -> anaesthesia_end
  # Then, for each admission, keep the earliest non-NA candidate.
  phases_key <- phases %>%
    mutate(
      admission_csn_key      = as.character(admission_csn),
      patient_out_of_room    = suppressWarnings(as.POSIXct(patient_out_of_room, tz = tz)),
      in_recovery            = suppressWarnings(as.POSIXct(in_recovery,       tz = tz)),
      anaesthesia_end        = suppressWarnings(as.POSIXct(anaesthesia_end,   tz = tz)),
      postop_anchor_raw      = coalesce(patient_out_of_room, in_recovery, anaesthesia_end)
    ) %>%
    filter(!is.na(postop_anchor_raw)) %>%
    group_by(admission_csn_key) %>%
    summarise(postop_anchor = min(postop_anchor_raw, na.rm = TRUE), .groups = "drop")
  
  # ---------------------------------------------------------------------------
  # 2) Attach postop anchor to PCA data and keep only anchored admissions
  # ---------------------------------------------------------------------------
  # We carry forward only rows that have a matched anchor.
  pcas <- pcas_clean_flag_deduplicated %>%
    mutate(
      admission_csn_key = as.character(admission_csn),
      time_hourly_start = as.POSIXct(time_hourly_start, tz = tz)
    ) %>%
    left_join(phases_key, by = "admission_csn_key") %>%
    filter(!is.na(postop_anchor))
  
  # If nothing remains after anchoring, return empty shaped outputs
  if (nrow(pcas) == 0) {
    warning("No rows with a valid postop anchor were found.")
    return(list(days_per_med = tibble(), days_ome_by_admission = tibble()))
  }
  
  # ---------------------------------------------------------------------------
  # 3) Helper: compute the next 08:00 after a given timestamp
  # ---------------------------------------------------------------------------
  # Vectorised helper:
  #   - If time is before today's 08:00 -> return today's 08:00
  #   - If time is at/after today's 08:00 -> return tomorrow's 08:00
  next_8am_vec <- function(t) {
    t_local <- with_tz(t, tz)
    d <- as_date(t_local, tz = tz)
    eight_today <- as.POSIXct(paste0(d, " 08:00:00"), tz = tz)
    out <- eight_today
    out[t_local >= eight_today] <- eight_today[t_local >= eight_today] + days(1)
    out
  }
  
  # ---------------------------------------------------------------------------
  # 4) Build day windows per admission
  # ---------------------------------------------------------------------------
  # day_0: postop_anchor -> next 08:00
  # day_1..day_7: consecutive 08:00 -> 08:00 blocks after day_0 end
  bounds <- pcas %>%
    distinct(mrn, admission_csn, admission_csn_key, postop_anchor) %>%
    mutate(day0_end = next_8am_vec(postop_anchor))
  
  day_index <- tibble(day = 0:7)
  
  day_windows <- bounds %>%
    tidyr::crossing(day_index) %>%
    mutate(
      start = if_else(day == 0L, postop_anchor, day0_end + days(day - 1L)),
      end   = if_else(day == 0L, day0_end,      day0_end + days(day)),
      day_label = paste0("day_", day)
    )
  
  # ---------------------------------------------------------------------------
  # 5) Assign each hourly PCA row to its day window
  # ---------------------------------------------------------------------------
  # Membership rule:
  #   time_hourly_start >= start AND time_hourly_start < end
  pcas_days <- pcas %>%
    inner_join(day_windows, by = c("mrn","admission_csn","admission_csn_key")) %>%
    filter(time_hourly_start >= start,
           time_hourly_start <  end)
  
  # ---------------------------------------------------------------------------
  # 6) Sum dose per medication per post-op day
  # ---------------------------------------------------------------------------
  # We sum hourly doses within each (mrn, admission, med, day).
  # units is taken from the first row in that group (expected constant per med).
  dose_by_day <- pcas_days %>%
    group_by(mrn, admission_csn, medication, day, day_label) %>%
    summarise(
      dose = sum(dose_best_estimate_hourly, na.rm = TRUE),
      units = dplyr::first(units),
      .groups = "drop"
    )
  
  # ---------------------------------------------------------------------------
  # 7) Convert to OME using per-med/unit rules
  # ---------------------------------------------------------------------------
  # This function applies conversion factors only when (med, units) matches
  # expected forms:
  #   Morphine + mg, Oxycodone + mg, Fentanyl + mcg
  compute_ome <- function(med, units, dose) {
    out <- rep(NA_real_, length(dose))
    is_morph <- med == "Morphine"
    is_oxy   <- med == "Oxycodone"
    is_fent  <- med == "Fentanyl"
    
    out[is_morph & units == "mg"]  <- dose[is_morph & units == "mg"]  * morphine_iv_factor
    out[is_oxy   & units == "mg"]  <- dose[is_oxy   & units == "mg"]  * oxycodone_iv_factor
    out[is_fent  & units == "mcg"] <- dose[is_fent  & units == "mcg"] * fentanyl_iv_factor
    out
  }
  
  dose_ome_by_day <- dose_by_day %>%
    mutate(ome = compute_ome(medication, units, dose))
  
  # ---------------------------------------------------------------------------
  # 8) Make wide outputs per medication: dose_day_* and ome_day_*
  # ---------------------------------------------------------------------------
  dose_wide <- dose_ome_by_day %>%
    select(mrn, admission_csn, medication, day_label, dose) %>%
    pivot_wider(names_from = day_label, values_from = dose, names_prefix = "dose_")
  
  ome_wide <- dose_ome_by_day %>%
    select(mrn, admission_csn, medication, day_label, ome) %>%
    pivot_wider(names_from = day_label, values_from = ome, names_prefix = "ome_")
  
  # Combine dose + ome for each med, and attach the postop_anchor for reference
  days_per_med <- dose_wide %>%
    full_join(ome_wide, by = c("mrn","admission_csn","medication")) %>%
    left_join(bounds %>% select(mrn, admission_csn, postop_anchor),
              by = c("mrn","admission_csn")) %>%
    arrange(mrn, admission_csn, medication)
  
  # ---------------------------------------------------------------------------
  # 9) Collapse to per-admission totals (sum OME across meds)
  # ---------------------------------------------------------------------------
  # across(starts_with("ome_day_")) sums each day across medications.
  # ome_total_0to7 sums those day totals across day_0..day_7.
  days_ome_by_admission <- days_per_med %>%
    group_by(mrn, admission_csn) %>%
    summarise(
      across(starts_with("ome_day_"), ~ sum(.x, na.rm = TRUE)),
      postop_anchor = suppressWarnings(min(postop_anchor, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    mutate(ome_total_0to7 = rowSums(across(starts_with("ome_day_")), na.rm = TRUE)) %>%
    relocate(postop_anchor, .after = admission_csn)
  
  # Return both levels of output
  list(
    days_per_med = days_per_med,
    days_ome_by_admission = days_ome_by_admission
  )
}

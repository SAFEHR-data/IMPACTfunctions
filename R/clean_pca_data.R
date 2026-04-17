

#' Clean PCA data
#'
#' @param PCA_dataframe
#' @param phases_dataframe
#' @param near_full_threshold
#' @param min_jump_ml
#' @param priming_allowance_ml
#' @param zero_dose_eps_mg
#' @param zero_dose_eps_mcg
#' @param bg_ml_grid
#' @param tz
#'
#' @return
#' @export
#' @importFrom stringr str_to_upper
#' @importFrom stringr str_detect
#' @importFrom tidyr pivot_wider
#'
#' @examples
clean_pca_data <- function(PCA_dataframe = PCAs,
                           phases_dataframe = Phases,
                           near_full_threshold = 95,    # reservoir volume (mL) treated as "near full"
                           min_jump_ml = 20,    # minimum positive jump in reservoir volume to call "new bag"
                           priming_allowance_ml = 5,     # allow up to 5mL of post-bag "priming" to be removed once
                           zero_dose_eps_mg = 0.2,   # dose treated as ~0 for mg drugs (morphine/oxycodone)
                           zero_dose_eps_mcg  = 4,     # dose treated as ~0 for fentanyl (mcg)
                           bg_ml_grid = c(0, 0.5, 1.0, 1.5, 2.0),  # candidate background rates (mL/hr) for grid search
                           tz = "Europe/London"

){


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
  PCA_dataframe <- PCA_dataframe %>%
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

  # Unable to proceed with arrow, calling into R

  PCA_dataframe <- collect(PCA_dataframe)

   pcas_long_keyed <- PCA_dataframe %>%
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

  ### UP TO HERE ready for Step 2


  }

pca_summarise_days <- function(pcas_clean_flag_deduplicated,
                                  phases = Phases,
                                  tz = "Europe/London",
                                  oxycodone_iv_factor = 4.5,  # IV oxycodone → OME
                                  morphine_iv_factor  = 3.0,  # IV morphine  → OME
                                  fentanyl_iv_factor  = 0.3,   # mcg fentanyl → mg OME
                               output = c("dose", "ome"),
                               verbose = TRUE) {

  # ---------------------------------------------------------------------------
  # 1) Build one postop anchor per admission
  # ---------------------------------------------------------------------------
  # For each row in phases, define a candidate anchor:
  #   patient_out_of_room -> in_recovery -> anaesthesia_end
  # Then, for each admission, keep the earliest non-NA candidate.

  phases <- phases %>% filter(admission_csn %in% pcas_clean_flag_deduplicated$admission_csn) %>% collect()

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

  if(output == "dose"){
    return(days_per_med)
  }else if (output == "ome"){
    return(days_per_med)
  }else{
    cli::cli_abort("Please specify a return format in {.arg output}")
  }

}

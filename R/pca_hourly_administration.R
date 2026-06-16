#' Transform and Match PCA Data to Hourly Administration Buckets
#'
#' @description
#' `pca_hourly_administration()` models raw Patient-Controlled Analgesia (PCA) data into
#' precise, hourly timeline intervals. It maps longitudinal infusion metrics back to discrete Medication
#' Administration Record (MAR) validation windows to flag whether an active prescription was verified
#' at the point of care.
#'
#' @details
#' The processing architecture executes the following sequential lifecycle steps:
#' \enumerate{
#'   \item \strong{MAR Window Infrastructure:} Builds clinical prescription coverage windows spanning
#'   one hour before structural administration until execution of a subsequent order, a documented
#'   infusion stop time, or patient discharge.
#'   \item \strong{Dynamic Boundary Anchoring:} Evaluates historical entry gaps. If an entry sequence
#'   exceeds `new_admin_gap_hours` or stands as the index observation, it dynamically projects interval
#'   boundaries using lookback parameters to safely bound backfills.
#'   \item \strong{Hourly Volumetric Amortization:} Interpolates localized interval windows into explicit,
#'   one-hour buckets, uniformly fractionating and distributing `dose_best_estimate` metrics over the
#'   resampled time step array.
#'   \item \strong{Cross-Namespace Verification:} Merges temporal buckets against the valid MAR arrays
#'   by verifying whether a given hour bucket's endpoint drops within an authenticated validation window.
#'   \item \strong{Conflict De-duplication:} Resolves multi-entry temporal collisions by stripping redundant
#'   zero-dose records and prioritizing MAR-authenticated logs over unverified records.
#' }
#'
#' @param pca_dataframe_cleaned A data frame containing structured, cleaned PCA observations. Must feature
#'   the columns: \code{mrn}, \code{admission_csn}, \code{simple_generic_name}, \code{date_time_recorded},
#'   \code{dose_best_estimate}, \code{dose_best_basis}, \code{units}, \code{concentration_mismatch_flag},
#'   and \code{interval_hours}.
#' @param mar_df A data frame containing raw or parsed Medication Administration Record (MAR) logs.
#'   Must feature variables tracking patient identifiers, medication naming strings, routes,
#'   and temporal status parameters.
#' @param tz A character string specifying the Olson time zone convention used to enforce
#'   uniform POSIXct parsing across clinical timestamps. Default is `"Europe/London"`.
#' @param new_admin_gap_hours A numeric threshold defining the duration boundary (in hours)
#'   beyond which a subsequent PCA log is treated as a completely distinct, new clinical
#'   administration episode. Default is `48`.
#'  @param resolve_crossover Runs the `pca_resolve_crossover()` function within, thus accepting the default values set by the package.
#'   See the default parameters with `?pca_resolve_crossover`.
#'   Use `resolve_crossover = FALSE` and then run the function separaltly on the resulting dataframe to change parametes.
#'
#' @return A data frame containing hourly resampled PCA administration entries with
#'   the following structures:
#'   \list{
#'     \item{\code{mrn}, \code{admission_csn}}{Standardized character string patient/encounter keys.}
#'     \item{\code{simple_generic_name}}{Standardized character mapping label (\code{"Morphine"}, \code{"Oxycodone"}, or \code{"Fentanyl"}).}
#'     \item{\code{units}}{The localized unit label assigned to the dosage metrics.}
#'     \item{\code{date_time_recorded}}{The original raw timestamp recorded in the EHR logs.}
#'     \item{\code{time_hourly_start}, \code{time_hourly_end}}{\code{POSIXct} boundaries explicitly boxing the localized hour interval.}
#'     \item{\code{dose_best_estimate_hourly}}{Amortized numeric dosage fraction mapping across the specific hour frame.}
#'     \item{\code{dose_best_basis}}{Character string recording the source math or rationale behind the baseline estimate.}
#'     \item{\code{concentration_mismatch_flag}}{Logical indicator identifying downstream concentration calculation conflicts.}
#'     \item{\code{valid_prescription}}{Logical flag indicating whether an active MAR order backed this specific hour frame.}
#'     \item{\code{matched_orders}}{Character string containing comma-separated MAR order identifiers validating the window.}
#'     \item{\code{still_duplicated}}{Logical indicator flagging if unresolved multi-medication record collisions remain.}
#'   }
#'
#' @export
#'
#' @importFrom dplyr filter mutate arrange group_by lead ungroup select case_when row_number lag across all_of n anti_join
#' @importFrom lubridate as_datetime floor_date hours ceiling_date dhours
#' @importFrom stringr str_to_lower str_detect
#' @importFrom purrr map2
#' @importFrom tidyr unnest
#'
#' @examples
#' \dontrun{
#' hourly_pcas <- pca_hourly_administration(
#'   pca_dataframe_cleaned = cleaned_pca_data,
#'   mar_df = post_op_mar,
#'   tz = "Europe/London",
#'   new_admin_gap_hours = 48
#' )
#' }
pca_hourly_administration <- function(pca_dataframe_cleaned = pca_dataframe_cleaned,
                                      mar_df,
                                      tz = "Europe/London",
                                      new_admin_gap_hours = 48,
                                      resolve_crossover = TRUE,
                                      verbose = TRUE) {

  if(verbose){cli::cli_h1("Creating PCA hourly administrations")}
  if(verbose){cli::cli_progress_step("Checking inputs")}

  # ---------------------------------------------------------------------------
  # Sanity checks: ensure required columns exist in inputs
  # ---------------------------------------------------------------------------
  req_pcas_cols <- c(
    "mrn","admission_csn","simple_generic_name","date_time_recorded",
    "dose_best_estimate","dose_best_basis","units",
    "concentration_mismatch_flag",
    "interval_hours"              # used for first-row anchoring logic
  )
  miss_pcas <- setdiff(req_pcas_cols, names(pca_dataframe_cleaned))
  if (length(miss_pcas)) {
    stop("pca_dataframe_cleaned missing columns: ", paste(miss_pcas, collapse=", "))
  }

  req_mar_cols <- c(
    "mrn","case_id","admission_csn","anaesthesia_csn",
    "operation_csn","mar_id",
    "simple_generic_name","generic_name","medication_name",
    "administration_date_time","stop_time",
    "administration_route"
  )

  miss_mar <- setdiff(req_mar_cols, names(mar_df))
  if (length(miss_mar)) {
    warning("mar_df is missing some optional columns; attempting to continue. Missing: ",
            paste(miss_mar, collapse=", "))
  }

  if(verbose){cli::cli_progress_step("Building hourly windows")}
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


  mar_subset <- mar_df %>% filter(administration_route == "Intravenous",
                                  pharmaceutical_class == "Analgesics",
                                  admission_csn %in% pca_dataframe_cleaned$admission_csn,
                                  !is.na(administration_date_time)
                                  ) %>% collect()

  mar_check <- mar_subset %>%
    mutate(
      mrn,
      case_id,
      admission_csn,
      anaesthesia_csn,
      operation_csn,   # NOTE: your table uses operation_csn
      mar_id,

      # Use whichever medication name field is populated
      medication_raw = dplyr::coalesce(simple_generic_name, generic_name, medication_name),

      # Parse admin + stop timestamps in local tz
      administration_instant = lubridate::as_datetime(administration_date_time, tz = tz),
      stop_time = lubridate::as_datetime(stop_time, tz = tz),

      # Not present in your post_op_mar; keep as NA for window end calc
      discharge_date = as.POSIXct(NA, tz = tz),

      administration_route,
    .keep="none") %>%
    # Keep only PCA meds, IV route, and rows with a timestamp %>%
    # Map MAR medication text -> standardized medication labels
    mutate(
      simple_generic_name = case_when(
        str_detect(str_to_lower(medication_raw), "morphine") ~ "morphine sulfate",
        str_detect(str_to_lower(medication_raw), "oxycod")   ~ "oxycodone hydrochloride",
        str_detect(str_to_lower(medication_raw), "fent")     ~ "fentanyl",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(simple_generic_name)) %>%
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
      mrn, case_id, admission_csn, anaesthesia_csn, operation_csn, mar_id,
      simple_generic_name, medication_raw, administration_instant,
      time_valid, time_valid_end
    )

  if(verbose){cli::cli_progress_step("Linking administrations to hourly windows")}
  # ===========================================================================
  # 1) Tag first PCA row per admission (used to allow a long first interval)
  # ===========================================================================
  # compute_clean_pca may have theatre-anchored interval_hours for the first row.
  # We keep a flag and also carry the first interval_hours for potential use later.
  clean_pca_aug <- pca_dataframe_cleaned %>%
    arrange(mrn, admission_csn, date_time_recorded) %>%
    group_by(mrn, admission_csn) %>%
    mutate(
      first_row_overall_in_adm       = (row_number() == 1L),
      first_interval_hours_in_adm    = interval_hours[1]
    ) %>%
    ungroup()

  if(verbose){cli::cli_progress_step("Filling hourly windows")}
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
  pcas_with_row_start <- clean_pca_aug %>%
    group_by(mrn, admission_csn, simple_generic_name) %>%
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
      mrn, admission_csn, simple_generic_name, units,
      date_time_recorded,
      time_hourly_start = hour_starts,
      time_hourly_end   = hour_starts + hours(1),
      dose_best_estimate_hourly = hourly_dose_best_estimate,
      dose_best_basis,
      concentration_mismatch_flag
    ) %>%
    arrange(mrn, admission_csn, simple_generic_name, time_hourly_start)

  if(verbose){cli::cli_progress_step("Matching flowsheet data to MAR")}
  # ===========================================================================
  # 3) MAR matching
  # ===========================================================================
  # Build a lightweight matching table and join by (mrn, simple_generic_name).
  # A hit occurs when the hour bucket END falls within [time_valid, time_valid_end].
  mar_for_match <- mar_check %>%
    transmute(
      mrn, simple_generic_name, mar_id,
      time_valid     = as.POSIXct(time_valid,     tz = tz),
      time_valid_end = as.POSIXct(time_valid_end, tz = tz)
    )

  # Add a row id so we can collapse matches and join back safely
  pcas_with_id <- pcas_hourly_expanded %>%
    mutate(.row_id = dplyr::row_number())

  # For each hourly PCA row, determine if it matches any MAR window.
  hits_mrn <- pcas_with_id %>%
    left_join(mar_for_match, by = c("mrn", "simple_generic_name")) %>%
    mutate(
      hit = !is.na(mar_id) &
        !is.na(time_hourly_end) & !is.na(time_valid) & !is.na(time_valid_end) &
        time_hourly_end >= time_valid & time_hourly_end <= time_valid_end
    ) %>%
    group_by(.row_id) %>%
    summarise(
      valid_prescription = any(hit, na.rm = TRUE),
      matched_orders = {
        ids <- unique(mar_id[hit])
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

  if(verbose){cli::cli_progress_step("Removing duplicates")}
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
    arrange(mrn, admission_csn, simple_generic_name, time_hourly_start, time_hourly_end)

  # Flag remaining duplicates after cleaning
  pcas_hourly_cleaned_flag <- pcas_hourly_cleaned %>%
    group_by(across(all_of(dupe_key))) %>%
    mutate(still_duplicated = dplyr::n() > 1) %>%
    ungroup()


  if(resolve_crossover){
    cli::cli_progress_step("Resolving crossovers with default parameters")
    pca_hourly <- pca_resolve_crossover(pca_hourly = pcas_hourly_cleaned_flag)
    if(verbose){cli::cli_progress_done("Returning hourly dataset")}
    return(pca_hourly)
  }else{
    if(verbose){cli::cli_progress_done("Returning hourly dataset")}
    cli::cli_alert_warning("CAUTION: Crossovers not resolved! Set `resolve_crossover = TRUE` or run {.fn pca_resolve_crossover} separatly")
    pca_hourly <- pcas_hourly_cleaned_flag
    return(pca_hourly)
    }







}


#' Aggregate and Summarise Hourly PCA Consumption into Post-Operative Days
#'
#' @description
#' `pca_summarise_days()` constructs an administrative post-operative clinical
#' anchoring point per hospital admission, discretizes continuous temporal
#' sequences into non-uniform post-operative day windows (anchored around a standard
#' *08:00 shift change*), and aggregates hourly Patient-Controlled Analgesia (PCA)
#' metrics. It handles raw dosage summation and standardizes therapeutic density via
#' parametric Oral Morphine Equivalent (OME) conversion factors.
#'
#' @details
#' The processing pipeline implements the following systematic logic:
#' \enumerate{
#'   \item \strong{Clinical Post-Op Anchoring:} Discovers a deterministic surgical
#'   baseline timestamp per encounter by evaluating phase records hierarchically:
#'   \code{patient_out_of_room} \rightarrow \code{in_recovery} \rightarrow \code{anaesthesia_end}.
#'   Encounters completely lacking milestone timestamps are pruned from downstream evaluation.
#'   \item \strong{Asymmetric Boundary Partitioning:} Maps each encounter to an
#'   asymmetric temporal matrix. Post-operative day 0 (\code{day_0}) represents a
#'   variable window extending from the exact anchor timestamp up to the immediate
#'   subsequent 08:00 local time boundary. Subsequent windows (\code{day_1} through
#'   \code{day_7}) represent fixed, consecutive 24-hour blocks spanning \eqn{[08:00, 08:00)}.
#'   \item \strong{Interval Assignment:} Maps hourly PCA records to their respective
#'   day windows using a left-closed, right-open assignment strategy:
#'   \code{time_hourly_start} \eqn{\ge \text{start}} AND \code{time_hourly_start} \eqn{< \text{end}}.
#'   \item \strong{Parametric Equianalgesic Standardization:} Aggregates individual
#'   hourly doses inside each day partition and executes OME conversion using inline
#'   functional parameters mapping specific medication-unit pairs (Morphine mg,
#'   Oxycodone mg, and Fentanyl mcg).
#'   \item \strong{Pivoting and Aggregation:} Reshapes independent metric arrays into
#'   wide longitudinal vectors prefixed with \code{dose_day_} and \code{ome_day_}.
#' }
#'
#' @param pca_hourly A data frame or tibble containing hourly resampled and
#'   crossover-resolved PCA entries. Required variables include: \code{mrn},
#'   \code{admission_csn}, \code{simple_generic_name}, \code{units}, \code{time_hourly_start},
#'   and \code{dose_best_estimate_hourly}.
#' @param phases A data frame or remote database table (\code{tbl_lazy}) containing
#'   administrative peroperative or post-anaesthetic milestone logs. Must feature variables
#'   tracking \code{admission_csn}, \code{patient_out_of_room}, \code{in_recovery}, and
#'   \code{anaesthesia_end}. Note: This table is explicitly evaluated locally via
#'   \code{dplyr::collect()}.
#' @param tz A character string specifying the Olson time zone convention used to enforce
#'   uniform POSIXct parsing and calendar calculations across clinical timestamps.
#'   Default is \code{"Europe/London"}.
#' @param oxycodone_iv_factor A numeric multiplier used to convert IV Oxycodone in mg
#'   to mg Oral Morphine Equivalent. Default is \code{4.5}.
#' @param morphine_iv_factor A numeric multiplier used to convert IV Morphine in mg
#'   to mg Oral Morphine Equivalent. Default is \code{3.0}.
#' @param fentanyl_iv_factor A numeric multiplier used to convert IV Fentanyl in mcg
#'   to mg Oral Morphine Equivalent. Default is \code{0.3}.
#' @param output A character string determining the matrix composition returned by the
#'   function. Must be either \code{"dose"} or \code{"ome"}. Note: Both options
#'   currently yield the wide per-medication data frame tracking both raw metrics and OMEs.
#' @param verbose A logical scalar enabling or disabling telemetry tracking and warnings
#'   during execution. Default is \code{TRUE}.
#'
#' @return A data frame formatted in a wide analytical schema—one row per unique
#'   \code{mrn}, \code{admission_csn}, and \code{simple_generic_name} combination—exposing
#'   the following metadata layout:
#'   \list{
#'     \item{\code{mrn}, \code{admission_csn}, \code{admission_csn_key}}{Standardized and normalized patient/encounter keys.}
#'     \item{\code{simple_generic_name}}{The specific analgesic categorical identifier string.}
#'     \item{\code{postop_anchor}}{The derived \code{POSIXct} baseline surgical timestamp utilized for lookbacks.}
#'     \item{\code{dose_day_0 ... dose_day_7}}{Dynamic columns tracking raw summed medication totals consumed inside each daily interval.}
#'     \item{\code{ome_day_0 ... ome_day_7}}{Dynamic columns tracking calculated Oral Morphine Equivalent metrics across identical daily boundaries.}
#'   }
#'
#'   If no valid post-operative anchors are detected across the input cohort, a
#'   warning is issued and an empty named \code{list} containing two structured
#'   tibbles (\code{days_per_med} and \code{days_ome_by_admission}) is returned.
#'
#' @export
#'
#' @importFrom dplyr mutate group_by summarise filter left_join inner_join select arrange ungroup first across collect
#' @importFrom lubridate with_tz as_date days
#' @importFrom tidyr crossing pivot_wider
#' @importFrom cli cli_abort
#'
#' @examples
#' \dontrun{
#' # Partition PCA administration metrics into clinical day windows
#' pca_summary_daily <- pca_summarise_days(
#'   pca_hourly = pca_hourly,
#'   phases = surgical_milestones,
#'   tz = "Europe/London",
#'   output = "dose"
#' )
#' }
pca_summarise_days <- function(pca_hourly,
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

  phases <- phases %>% filter(admission_csn %in% pca_hourly$admission_csn) %>% collect()

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
  pcas <- pca_hourly %>%
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
  # 6) Sum dose per simple_generic_name per post-op day
  # ---------------------------------------------------------------------------
  # We sum hourly doses within each (mrn, admission, med, day).
  # units is taken from the first row in that group (expected constant per med).
  dose_by_day <- pcas_days %>%
    group_by(mrn, admission_csn, simple_generic_name, day, day_label) %>%
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
    is_morph <- med == "morphine sulfate"
    is_oxy   <- med == "oxycodone hydrochloride"
    is_fent  <- med == "fentanyl"

    out[is_morph & units == "mg"]  <- dose[is_morph & units == "mg"]  * morphine_iv_factor
    out[is_oxy   & units == "mg"]  <- dose[is_oxy   & units == "mg"]  * oxycodone_iv_factor
    out[is_fent  & units == "mcg"] <- dose[is_fent  & units == "mcg"] * fentanyl_iv_factor
    out
  }

  dose_ome_by_day <- dose_by_day %>%
    mutate(ome = compute_ome(simple_generic_name, units, dose))

  # ---------------------------------------------------------------------------
  # 8) Make wide outputs per medication: dose_day_* and ome_day_*
  # ---------------------------------------------------------------------------
  dose_wide <- dose_ome_by_day %>%
    select(mrn, admission_csn, simple_generic_name, day_label, dose) %>%
    pivot_wider(names_from = day_label, values_from = dose, names_prefix = "dose_")

  ome_wide <- dose_ome_by_day %>%
    select(mrn, admission_csn, simple_generic_name, day_label, ome) %>%
    pivot_wider(names_from = day_label, values_from = ome, names_prefix = "ome_")

  # Combine dose + ome for each med, and attach the postop_anchor for reference
  days_per_med <- dose_wide %>%
    full_join(ome_wide, by = c("mrn","admission_csn","simple_generic_name")) %>%
    left_join(bounds %>% select(mrn, admission_csn, postop_anchor),
              by = c("mrn","admission_csn")) %>%
    arrange(mrn, admission_csn, simple_generic_name)

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

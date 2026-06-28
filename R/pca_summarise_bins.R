#' Aggregate and Summarise Hourly PCA Consumption Into Post-Operative Bins
#'
#' @description
#' `pca_summarise_bins()` calculates the clinical post-operative anchor point for each
#' hospital admission and pools de-duplicated hourly PCA consumption records into custom
#' time-horizon bins. It computes localized dosage sums alongside standardized Oral Morphine
#' Equivalent (OME) conversion metrics, pivoting the final outputs into an analytics-ready wide format.
#'
#' @details
#' The summarizing pipeline implements the following systematic logic:
#' \enumerate{
#'   \item \strong{Robust Key Coercion:} Normalizes numeric encounter keys via an internal
#'   formatting subroutine (`norm_key()`) to prevent scientific notation truncation and
#'   ensure string join stability.
#'   \item \strong{Clinical Post-Op Anchoring:} Establishes a singular, earliest surgical baseline
#'   timestamp per encounter by testing the availability of phase timestamps hierarchically:
#'   `patient_out_of_room` -> `in_recovery` -> `anaesthesia_end`.
#'   \item \strong{Volumetric Binning Array:} Computes elapsed hours relative to the anchor using
#'   the upper interval boundary (`time_hourly_end`). Continuous elapsed times are discretized
#'   into right-closed intervals `(a, b]` utilizing `base::cut()` labeled with intuitive strings (e.g., `"0-24h"`).
#'   \item \strong{Equianalgesic Standardization:} Sums individual hourly dose vectors within each
#'   bin boundary and standardizes therapeutic density by mapping medication-specific OME multipliers
#'   (e.g., standard mg or mcg equivalents).
#'   \item \strong{Pivot and Shape Transformation:} Reshapes independent metric arrays into a single row
#'   per patient-encounter-medication triplet, exposing wide dynamic vectors prefixed with
#'   \code{dose_} and \code{ome_}.
#' }
#'
#' @param pca_hourly A data frame or tibble containing hourly resampled and crossover-resolved
#'   PCA entries. Required columns include: \code{mrn}, \code{admission_csn}, \code{simple_generic_name}, \code{units},
#'   \code{time_hourly_start}, \code{time_hourly_end}, and \code{dose_best_estimate_hourly}.
#' @param phases A data frame containing administrative peroperative or post-anaesthetic milestone logs.
#'   Must feature variables tracking \code{admission_csn}, \code{patient_out_of_room}, \code{in_recovery},
#'   and \code{anaesthesia_end}.
#' @param tz A character string specifying the Olson time zone convention used to enforce
#'   uniform POSIXct parsing across clinical timestamps. Default is `"Europe/London"`.
#' @param bins_hours A numeric vector mapping the localized hour thresholds utilized to segment
#'   post-operative tracking arrays. Default is `c(0, 24, 48, 72, 96, 120, 146)`.
#' @param ome_conv A data frame or tibble establishing equianalgesic multipliers relative to core
#'   clinical dosing scales. Must contain mapping columns: \code{simple_generic_name}, \code{units}, and \code{ome_per_unit}.
#'   Default contains standard institutional conversion metrics for Morphine, Oxycodone, and Fentanyl.
#'
#' @return A data frame formatted in a wide schema—one row per unique \code{mrn}, \code{admission_csn},
#'   and \code{simple_generic_name} combination—exposing the following metadata layout:
#' * `mrn`, `admission_csn`, `admission_csn_key`: Standardized and normalized patient/encounter keys.
#' * `simple_generic_name`: The specific analgesic categorical identifier string.
#' * `postop_anchor`: The derived `POSIXct` baseline surgical timestamp utilized for lookbacks.
#' * `dose_*.h`: Dynamic columns tracking raw summed medication totals consumed inside each interval break.
#' * `ome_*.h`: Dynamic columns tracking calculated Oral Morphine Equivalent metrics across identical boundaries.
#'
#' @export
#'
#' @importFrom dplyr mutate group_by summarise filter left_join select arrange ungroup
#' @importFrom tibble tribble
#' @importFrom tidyr pivot_wider
#'
#' @examples
#' \dontrun{
#' # Aggregate cleaned PCA data across 24-hour surgical intervals
#' binned_pca_summary <- pca_summarise_bins(
#'   pca_hourly = pca_hourly,
#'   phases = surgical_milestones,
#'   tz = "Europe/London",
#'   bins_hours = c(0, 24, 48, 72, 96)
#' )
#' }
pca_summarise_bins <- function(
    pca_hourly,
    phases = Phases,
    tz = "Europe/London",
    bins_hours = c(0, 24, 48, 72, 96, 120, 146),
    ome_conv = tibble::tribble(
      ~simple_generic_name, ~units, ~ome_per_unit,
      "morphine sulfate",  "mg",   3.0,
      "oxycodone hydrochloride", "mg",   4.5,
      "fentanyl",  "mcg",  0.30
    )
) {

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

  phases <- phases %>% filter(admission_csn %in% pca_hourly$admission_csn) %>% collect()

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

  df <- pca_hourly %>%
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
    group_by(mrn, admission_csn, simple_generic_name, units, admission_csn_key, postop_anchor, bin) %>%
    summarise(
      dose_sum = sum(dose_best_estimate_hourly, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(ome_conv, by = c("simple_generic_name", "units")) %>%
    mutate(
      dose_OME = if_else(is.na(ome_per_unit), NA_real_, dose_sum * ome_per_unit)
    )

  # -------------------------------------------------------------------------
  # 5) Wide outputs: one row per (mrn, admission_csn, simple_generic_name)
  # -------------------------------------------------------------------------
  # We generate:
  #   dose_0-24h, dose_24-48h, ...
  #   ome_0-24h,  ome_24-48h,  ...
  # and keep the postop_anchor used.
  dose_wide <- dose_with_ome %>%
    select(mrn, admission_csn, simple_generic_name, admission_csn_key, postop_anchor, bin, dose_sum) %>%
    pivot_wider(names_from = bin, values_from = dose_sum, names_prefix = "dose_")

  ome_wide <- dose_with_ome %>%
    select(mrn, admission_csn, simple_generic_name, admission_csn_key, postop_anchor, bin, dose_OME) %>%
    pivot_wider(names_from = bin, values_from = dose_OME, names_prefix = "ome_")

  out <- dose_wide %>%
    left_join(
      ome_wide,
      by = c("mrn", "admission_csn", "simple_generic_name", "admission_csn_key", "postop_anchor")
    ) %>%
    arrange(mrn, admission_csn, simple_generic_name)

  return(out)
}

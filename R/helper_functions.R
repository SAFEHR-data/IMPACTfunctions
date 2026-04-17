`%||%` <- function(a, b) if (is.null(a)) b else a

#' HELPER FUNCTION - safe_floor_step
#'
#' Parent function - `clean_pca_data()`
#'
#' Floors a number to a step size (e.g., 0.5 mg increments).
# The +1e-9 avoids edge rounding issues where x is very close to an exact boundary.
#'
#' @param x Value
#' @param step Step size
#'
safe_floor_step <- function(x, step) {
  ifelse(is.na(x), NA_real_, floor((x + 1e-9) / step) * step)
}

#' HELPER FUNCTION - floor_step_by_med
#'
#' Parent function - `clean_pca_data()`
#'
#' Floors the per-press bolus dose to plausible step sizes:
#'   fentanyl: 10 mcg increments
#'   morphine/oxycodone: 0.5 mg increments
#'
#' @param x value
#' @param med medication
#'
floor_step_by_med <- function(x, med) {
  m <- tolower(med %||% "")
  step <- ifelse(grepl("fentanyl", m), 10, 0.5)
  safe_floor_step(x, step)
}

#' HELPER FUNCTION - upper_bg_rate_by_med
#'
#' Parent function - `clean_pca_data()`
#'
#' Upper bound for background mass rate:
#'    fentanyl: 40 mcg/hr
#'    others:   2 mg/hr
#'
#' @param med medication
#'
upper_bg_rate_by_med <- function(med) {
  if (grepl("fentanyl", tolower(med))) 40 else 2
}

#' HELPER FUNCTION - nearest_half_ml
#'
#' Parent function - `clean_pca_data()`
#'
#' Snaps a rate to nearest 0.5 mL/hr (because most pumps are set in half-ml steps).
#' @param x value
#'
nearest_half_ml <- function(x) {
  ifelse(is.na(x), NA_real_, round(x / 0.5) * 0.5)
}

#' HELPER FUNCTION - bg_pref_penalty
#'
#' Parent function - `clean_pca_data()`
#'
#' Penalizes background rates away from the most common settings (0, 0.5, 1.0 mL/hr), used as a soft tie-breaker in grid search.
#'
#' @param mlhr ratein ml/hr
bg_pref_penalty <- function(mlhr) {
  prefs <- c(0, 0.5, 1.0)
  ifelse(is.na(mlhr), Inf,
         pmin(abs(mlhr - prefs[1]), abs(mlhr - prefs[2]), abs(mlhr - prefs[3])))
}


#' HELPER FUNCTION - has_half_mg_evidence
#'
#' Parent function - `clean_pca_data()`
#'
#' Evidence test for whether 0.5mg bolus is plausible for mg-drugs: If multiple intervals show dose/press ~0.5mg, allow 0.5 in candidate bolus set.
#'
#' @param press_vec vector of presses
#' @param dose_vec vector of doses
has_half_mg_evidence <- function(press_vec, dose_vec) {
  ok <- which(is.finite(press_vec) & press_vec > 0 & is.finite(dose_vec) & dose_vec > 0)
  if (length(ok) < 2) return(FALSE)
  r <- dose_vec[ok] / press_vec[ok]
  sum(r >= 0.45 & r <= 0.55) >= 2 && mean(r >= 0.45 & r <= 0.55) >= 0.5
}

#' HELPER FUNCTION - sum_distinct
#'
#' Parent function - `clean_pca_data()`
#'
#' Helper: sum unique values (rounded) to avoid duplicate charting artifacts. Used for dose_administered and volume_administered where multiple lines may exist.
#'
#' @param x vector of presses
#' @param tol vector of doses
sum_distinct <- function(x, tol = 1e-6) {
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  ux <- unique(round(x, 6))
  if (length(ux) == 1) ux[1] else sum(ux)
}

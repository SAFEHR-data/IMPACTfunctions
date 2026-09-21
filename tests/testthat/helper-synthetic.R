# Loaded once per test session (testthat convention: files matching
# helper-*.R are sourced before any test file runs). These objects are the
# shared fixtures used across the test suite so we only pay the cost of
# generating/reading the synthetic dataset once.
#
# The synthetic dataset generator is seeded and version-stamped (see
# `generate_synthetic_dataset()` / `load_synthetic_dataset()`), so these
# fixtures are stable for a given package version but will change if the
# generator or the package version changes.

test_synthetic <- load_synthetic_dataset()

test_procedures   <- test_synthetic$Procedures
test_demographics <- test_synthetic$Demographics
test_hospitalmar  <- test_synthetic$HospitalMar
test_measurements <- test_synthetic$Measurements

#' Ensure a test starts and ends with no `eligibility_criteria` in the global
#' environment.
#'
#' `cohort_enforce_*()` functions record their filtering via
#' `eligibility_criteria_add()`, which writes to a variable named
#' `eligibility_criteria` in the global environment. Left in place between
#' tests, this both leaks state across `test_that()` blocks and triggers the
#' "applied the same eligibility criteria twice" warning from
#' `eligibility_criteria_check()`. Call this at the top of any test that uses
#' `add_to_eligability_criteria = TRUE`.
local_reset_eligibility_criteria <- function(env = parent.frame()) {
  clear <- function() {
    if (exists("eligibility_criteria", envir = .GlobalEnv, inherits = FALSE)) {
      rm("eligibility_criteria", envir = .GlobalEnv)
    }
  }
  clear()
  withr::defer(clear(), envir = env)
}

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

# A clean study cohort (no missing/malformed admissions), used by tests for
# functions that operate on a cohort rather than the raw Procedures table
# (e.g. extract_medication(), extract_flowsheet_data()).
test_dataset <- cohort_complete_admissions(test_procedures, add_to_eligability_criteria = FALSE)

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

#' Make the global `HospitalMar` binding match `value`, and restore it afterwards.
#'
#' `preproccess_MAR()` (and, through it, `extract_medication()` whenever
#' `apply_preproccess_MAR = TRUE`, the default) requires its `MAR` argument to
#' be `identical()` to a global object named `HospitalMar` - see
#' `if(identical(MAR, HospitalMar))` in R/preproccess_MAR.R. Call this before
#' testing either function, passing the same object you're passing as `MAR`.
local_set_global_hospitalmar <- function(value = test_hospitalmar, env = parent.frame()) {
  had_prior <- exists("HospitalMar", envir = .GlobalEnv, inherits = FALSE)
  prior_value <- if (had_prior) get("HospitalMar", envir = .GlobalEnv, inherits = FALSE) else NULL

  assign("HospitalMar", value, envir = .GlobalEnv)

  withr::defer({
    if (had_prior) {
      assign("HospitalMar", prior_value, envir = .GlobalEnv)
    } else if (exists("HospitalMar", envir = .GlobalEnv, inherits = FALSE)) {
      rm("HospitalMar", envir = .GlobalEnv)
    }
  }, envir = env)
}

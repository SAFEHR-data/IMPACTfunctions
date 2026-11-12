# Support for opt-in tests against a real (non-synthetic) IMPACT dataset.
#
# Real patient data must never be committed to this repository or used in CI.
# Tests gated by `skip_if_no_local_data()` are for a developer to run on their
# own machine against a known dataset they have access to, by setting:
#
#   IMPACTFUNCTIONS_LOCAL_DATA_PATH=/path/to/a/real/IMPACT/extract
#
# e.g. in a project-level `.Renviron` (gitignored - see .gitignore). On CI,
# and for any other developer who hasn't set this, tests calling
# `skip_if_no_local_data()` are skipped, not failed.

#' Skip the current test unless a real local dataset is configured.
#'
#' @return The local dataset folder path (invisibly skips otherwise).
skip_if_no_local_data <- function() {
  path <- Sys.getenv("IMPACTFUNCTIONS_LOCAL_DATA_PATH", unset = NA)
  if (is.na(path) || !dir.exists(path)) {
    testthat::skip("Set IMPACTFUNCTIONS_LOCAL_DATA_PATH to a real IMPACT extract folder to run local tests")
  }
  path
}

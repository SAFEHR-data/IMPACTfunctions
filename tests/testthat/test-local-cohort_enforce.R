# Opt-in tests for cohort_enforce_*() against a real, local IMPACT dataset.
#
# These never run in CI or for a developer who hasn't configured
# IMPACTFUNCTIONS_LOCAL_DATA_PATH (see helper-local-data.R) - they are a
# harness for a developer with access to a known real dataset to sanity-check
# the same logic tested in test-cohort_enforce.R against real data.
#
# `load_dataset()` assigns its tables (Procedures, Demographics, ...) into
# the global environment rather than returning them, so we read them back out
# of globalenv() and clean up after ourselves.
#
# Configure, e.g. in a project-level .Renviron (gitignored):
#   IMPACTFUNCTIONS_LOCAL_DATA_PATH=/path/to/a/real/IMPACT/extract
#   IMPACTFUNCTIONS_LOCAL_DATA_PREFIX=SCB_Periop_   # optional, matches load_dataset(prefix=)
#   IMPACTFUNCTIONS_LOCAL_DATA_SUFFIX=_20260122.parquet   # optional, matches load_dataset(suffix=)

local_load_real_dataset <- function(env = parent.frame()) {
  folder_path <- skip_if_no_local_data()
  prefix <- Sys.getenv("IMPACTFUNCTIONS_LOCAL_DATA_PREFIX", unset = "")
  suffix <- Sys.getenv("IMPACTFUNCTIONS_LOCAL_DATA_SUFFIX", unset = ".parquet")

  before <- ls(envir = .GlobalEnv)
  load_dataset(folder_path = folder_path, prefix = prefix, suffix = suffix, dictionary_check = FALSE)
  added <- setdiff(ls(envir = .GlobalEnv), before)

  withr::defer(rm(list = added, envir = .GlobalEnv), envir = env)

  mget(added, envir = .GlobalEnv)
}

test_that("cohort_enforce_discharged() keeps only rows with a discharge_date on real data", {
  real <- local_load_real_dataset()
  real_procedures <- real$Procedures

  expected <- real_procedures %>% collect() %>% filter(!is.na(discharge_date))

  result <- cohort_enforce_discharged(real_procedures, add_to_eligability_criteria = FALSE) %>% collect()

  expect_setequal(result$case_id, expected$case_id)
  expect_false(any(is.na(result$discharge_date)))
})

# Extend this file with further cohort_enforce_*() checks (and, over time,
# other families of tests) mirroring test-cohort_enforce.R, plus any
# domain-specific invariants a developer knows should hold for their real
# dataset (e.g. plausible date ranges, expected table sizes).

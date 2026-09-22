# Tests for cohort_complete_admissions() in R/cohort_complete_admissions.R.
#
# This function only orchestrates the already-tested cohort_enforce_*()
# functions (see test-cohort_enforce.R) in a fixed order, toggled by its
# enforce_* arguments - so these tests check the *composition* (does turning
# each flag on/off correctly include/exclude its corresponding enforce_ step)
# rather than re-deriving each filter's own oracle again.

test_that("cohort_complete_admissions() with all defaults matches manually chaining the enforce_ steps in order", {
  expected <- test_procedures %>%
    cohort_enforce_discharged(add_to_eligability_criteria = FALSE) %>%
    cohort_enforce_dates_complete(add_to_eligability_criteria = FALSE) %>%
    cohort_enforce_dates_ordered(add_to_eligability_criteria = FALSE) %>%
    cohort_enforce_complete_anaesthetic_chart(add_to_eligability_criteria = FALSE) %>%
    cohort_enforce_procedure_coded(add_to_eligability_criteria = FALSE) %>%
    collect()

  result <- cohort_complete_admissions(test_procedures, add_to_eligability_criteria = FALSE) %>% collect()

  expect_setequal(result$case_id, expected$case_id)
})

test_that("cohort_complete_admissions() with every enforce_ flag off returns the input unchanged", {
  result <- cohort_complete_admissions(
    test_procedures,
    enforce_discharged = FALSE, enforce_dates_complete = FALSE, enforce_date_order = FALSE,
    enforce_complete_anaesthetic_chart = FALSE, enforce_coded = FALSE,
    add_to_eligability_criteria = FALSE
  ) %>% collect()

  expect_setequal(result$case_id, test_procedures %>% collect() %>% pull(case_id))
})

test_that("cohort_complete_admissions() with a single enforce_ flag on matches calling that enforce_ function alone", {
  result <- cohort_complete_admissions(
    test_procedures,
    enforce_discharged = FALSE, enforce_dates_complete = FALSE, enforce_date_order = TRUE,
    enforce_complete_anaesthetic_chart = FALSE, enforce_coded = FALSE,
    add_to_eligability_criteria = FALSE
  ) %>% collect()

  expected <- test_procedures %>% cohort_enforce_dates_ordered(add_to_eligability_criteria = FALSE) %>% collect()

  expect_setequal(result$case_id, expected$case_id)
})

test_that("cohort_complete_admissions() passes dates_completed_cols through to cohort_enforce_dates_complete()", {
  result <- cohort_complete_admissions(
    test_procedures,
    dates_completed_cols = "admission_date",
    enforce_date_order = FALSE, enforce_complete_anaesthetic_chart = FALSE, enforce_coded = FALSE,
    add_to_eligability_criteria = FALSE
  ) %>% collect()

  expected <- test_procedures %>%
    cohort_enforce_discharged(add_to_eligability_criteria = FALSE) %>%
    cohort_enforce_dates_complete(dates_completed_cols = "admission_date", add_to_eligability_criteria = FALSE) %>%
    collect()

  expect_setequal(result$case_id, expected$case_id)
})

test_that("cohort_complete_admissions() KNOWN ISSUE: enforce_single_procedure = TRUE errors on the standard arrow-backed input", {
  # cohort_enforce_single_procedure() (R/cohort_enforce.R) does
  # group_by(admission_csn) %>% slice_min(operation_date) %>% ungroup(), and
  # arrow's dplyr backend doesn't support slice_min() on grouped data -
  # "Slicing grouped data not supported in Arrow". Every other cohort_enforce_*()
  # function operates lazily on the arrow-backed Procedures table (the normal
  # case throughout this package, e.g. from load_dataset()/load_synthetic_dataset()),
  # so this flag currently can't be used on real usage without first collect()-ing
  # to a plain data frame, which the function doesn't do for you.
  expect_error(
    cohort_complete_admissions(test_procedures, enforce_single_procedure = TRUE, add_to_eligability_criteria = FALSE, verbose = FALSE),
    "Slicing grouped data not supported in Arrow"
  )
})

test_that("cohort_enforce_single_procedure() keeps only the earliest procedure per admission_csn, once collected to a data frame", {
  # The selection logic itself is correct - it's only arrow-backed lazy
  # queries that it can't run against (see KNOWN ISSUE test above).
  fixture_df <- data.frame(
    case_id = 1:3,
    admission_csn = c(100, 100, 200),
    operation_date = as.POSIXct(c("2024-01-01 09:00", "2024-01-02 09:00", "2024-01-01 09:00"), tz = "UTC")
  )

  result <- cohort_enforce_single_procedure(fixture_df, add_to_eligability_criteria = FALSE, verbose = FALSE)

  expect_setequal(result$case_id, c(1, 3)) # case_id 2 is the later procedure on the same admission_csn
})

test_that("cohort_complete_admissions() enforce_single_procedure defaults to off", {
  result_default <- cohort_complete_admissions(test_procedures, add_to_eligability_criteria = FALSE) %>% collect()
  result_explicit_off <- cohort_complete_admissions(test_procedures, enforce_single_procedure = FALSE, add_to_eligability_criteria = FALSE) %>% collect()

  expect_setequal(result_default$case_id, result_explicit_off$case_id)
})

test_that("cohort_complete_admissions() records one eligibility_criteria row per enabled enforce_ step", {
  local_reset_eligibility_criteria()
  cohort_complete_admissions(test_procedures, add_to_eligability_criteria = TRUE, verbose = FALSE)

  # 5 steps enabled by default (enforce_single_procedure defaults to off)
  expect_equal(nrow(eligibility_criteria), 5)
  expect_setequal(
    eligibility_criteria$function_name,
    c(
      "cohort_enforce_discharged()", "cohort_enforce_dates_complete()", "cohort_enforce_dates_ordered()",
      "cohort_enforce_complete_anaesthetic_chart()", "cohort_enforce_procedure_coded()"
    )
  )
})

test_that("cohort_complete_admissions() verbose = TRUE reports the starting count", {
  expect_message(
    cohort_complete_admissions(test_procedures, add_to_eligability_criteria = FALSE, verbose = TRUE),
    regexp = "Starting with a dataset"
  )
})

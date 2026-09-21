# Tests for the cohort_enforce_*() family in R/cohort_enforce.R.
#
# Each test computes its own "expected" result independently with plain
# dplyr::filter() on the same synthetic fixtures (see helper-synthetic.R),
# rather than hardcoding row counts. This keeps the tests correct regardless
# of the exact values the synthetic dataset generator produces, and means
# they keep working unchanged if the synthetic dataset is later replaced with
# a higher-fidelity one, as long as column semantics stay the same.

# cohort_enforce_complete_anaesthetic_chart() -----------------------------

test_that("cohort_enforce_complete_anaesthetic_chart() keeps only rows with an anaesthesia_csn", {
  expected <- test_procedures %>% collect() %>% filter(!is.na(anaesthesia_csn))

  result <- cohort_enforce_complete_anaesthetic_chart(test_procedures, add_to_eligability_criteria = FALSE) %>%
    collect()

  expect_setequal(result$case_id, expected$case_id)
  expect_false(any(is.na(result$anaesthesia_csn)))
})

test_that("cohort_enforce_complete_anaesthetic_chart() records eligibility criteria correctly", {
  local_reset_eligibility_criteria()
  n_prior <- nrow(test_procedures)
  n_missing <- test_procedures %>% filter(is.na(anaesthesia_csn)) %>% collect() %>% nrow()

  cohort_enforce_complete_anaesthetic_chart(test_procedures, add_to_eligability_criteria = TRUE)

  row <- tail(eligibility_criteria, 1)
  expect_equal(row$function_name, "cohort_enforce_complete_anaesthetic_chart()")
  expect_equal(row$n_prior, n_prior)
  expect_equal(row$n_filtered, n_missing)
})

test_that("cohort_enforce_complete_anaesthetic_chart() diagnostics = TRUE partitions included/excluded", {
  out <- cohort_enforce_complete_anaesthetic_chart(test_procedures, diagnostics = TRUE, add_to_eligability_criteria = FALSE)

  included_ids <- out$included %>% collect() %>% pull(case_id)
  excluded_ids <- out$excluded %>% collect() %>% pull(case_id)

  expect_length(intersect(included_ids, excluded_ids), 0)
  expect_setequal(c(included_ids, excluded_ids), test_procedures %>% collect() %>% pull(case_id))
})

test_that("cohort_enforce_complete_anaesthetic_chart() verbose = TRUE reports what was filtered", {
  expect_message(
    cohort_enforce_complete_anaesthetic_chart(test_procedures, verbose = TRUE, add_to_eligability_criteria = FALSE),
    regexp = "Filtering out"
  )
})

# cohort_enforce_procedure_coded() -----------------------------------------

test_that("cohort_enforce_procedure_coded() keeps only fully coded procedures", {
  expected <- test_procedures %>%
    collect() %>%
    filter(!is.na(primary_coded_procedure) & !is.na(combined_coded_opcs_procedures))

  result <- cohort_enforce_procedure_coded(test_procedures, add_to_eligability_criteria = FALSE) %>% collect()

  expect_setequal(result$case_id, expected$case_id)
})

test_that("cohort_enforce_procedure_coded() records eligibility criteria correctly", {
  local_reset_eligibility_criteria()
  n_prior <- nrow(test_procedures)
  n_missing <- test_procedures %>%
    filter(is.na(primary_coded_procedure) | is.na(combined_coded_opcs_procedures)) %>%
    collect() %>%
    nrow()

  cohort_enforce_procedure_coded(test_procedures, add_to_eligability_criteria = TRUE)

  row <- tail(eligibility_criteria, 1)
  expect_equal(row$function_name, "cohort_enforce_procedure_coded()")
  expect_equal(row$n_prior, n_prior)
  expect_equal(row$n_filtered, n_missing)
})

test_that("cohort_enforce_procedure_coded() diagnostics = TRUE partitions included/excluded", {
  out <- cohort_enforce_procedure_coded(test_procedures, diagnostics = TRUE, add_to_eligability_criteria = FALSE)

  included_ids <- out$included %>% collect() %>% pull(case_id)
  excluded_ids <- out$excluded %>% collect() %>% pull(case_id)

  expect_length(intersect(included_ids, excluded_ids), 0)
  expect_setequal(c(included_ids, excluded_ids), test_procedures %>% collect() %>% pull(case_id))
})

# cohort_enforce_dates_complete() -------------------------------------------

test_that("cohort_enforce_dates_complete() keeps only rows with all default date columns present", {
  cols <- c("admission_date", "operation_date", "discharge_date")
  expected <- test_procedures %>% collect() %>% filter(if_all(all_of(cols), ~ !is.na(.x)))

  result <- cohort_enforce_dates_complete(test_procedures, add_to_eligability_criteria = FALSE) %>% collect()

  expect_setequal(result$case_id, expected$case_id)
})

test_that("cohort_enforce_dates_complete() respects a custom dates_completed_cols", {
  expected <- test_procedures %>% collect() %>% filter(!is.na(admission_date))

  result <- cohort_enforce_dates_complete(
    test_procedures,
    dates_completed_cols = "admission_date",
    add_to_eligability_criteria = FALSE
  ) %>% collect()

  expect_setequal(result$case_id, expected$case_id)
  # Restricting to a single column should never remove more rows than the default of three
  default_result <- cohort_enforce_dates_complete(test_procedures, add_to_eligability_criteria = FALSE) %>% collect()
  expect_gte(nrow(result), nrow(default_result))
})

test_that("cohort_enforce_dates_complete() records eligibility criteria correctly", {
  local_reset_eligibility_criteria()
  cols <- c("admission_date", "operation_date", "discharge_date")
  n_prior <- nrow(test_procedures)
  n_missing <- test_procedures %>%
    collect() %>%
    filter(!if_all(all_of(cols), ~ !is.na(.x))) %>%
    nrow()

  cohort_enforce_dates_complete(test_procedures, add_to_eligability_criteria = TRUE)

  row <- tail(eligibility_criteria, 1)
  expect_equal(row$function_name, "cohort_enforce_dates_complete()")
  expect_equal(row$n_prior, n_prior)
  expect_equal(row$n_filtered, n_missing)
})

# cohort_enforce_dates_ordered() ---------------------------------------------

test_that("cohort_enforce_dates_ordered() keeps only admission < operation < discharge", {
  expected <- test_procedures %>%
    collect() %>%
    filter(admission_date < operation_date, operation_date < discharge_date)

  result <- cohort_enforce_dates_ordered(test_procedures, add_to_eligability_criteria = FALSE) %>% collect()

  expect_setequal(result$case_id, expected$case_id)
})

test_that("cohort_enforce_dates_ordered() diagnostics = TRUE partitions included/excluded", {
  out <- cohort_enforce_dates_ordered(test_procedures, diagnostics = TRUE, add_to_eligability_criteria = FALSE)

  included_ids <- out$included %>% collect() %>% pull(case_id)
  excluded_ids <- out$excluded %>% collect() %>% pull(case_id)

  expect_length(intersect(included_ids, excluded_ids), 0)
  expect_setequal(c(included_ids, excluded_ids), test_procedures %>% collect() %>% pull(case_id))
})

# cohort_enforce_discharged() ------------------------------------------------

test_that("cohort_enforce_discharged() keeps only rows with a discharge_date", {
  expected <- test_procedures %>% collect() %>% filter(!is.na(discharge_date))

  result <- cohort_enforce_discharged(test_procedures, add_to_eligability_criteria = FALSE) %>% collect()

  expect_setequal(result$case_id, expected$case_id)
  expect_false(any(is.na(result$discharge_date)))
})

test_that("cohort_enforce_discharged() records eligibility criteria correctly", {
  local_reset_eligibility_criteria()
  n_prior <- nrow(test_procedures)
  n_missing <- test_procedures %>% filter(is.na(discharge_date)) %>% collect() %>% nrow()

  cohort_enforce_discharged(test_procedures, add_to_eligability_criteria = TRUE)

  row <- tail(eligibility_criteria, 1)
  expect_equal(row$function_name, "cohort_enforce_discharged()")
  expect_equal(row$n_prior, n_prior)
  expect_equal(row$n_filtered, n_missing)
})

test_that("cohort_enforce_discharged() diagnostics = TRUE partitions included/excluded", {
  out <- cohort_enforce_discharged(test_procedures, diagnostics = TRUE, add_to_eligability_criteria = FALSE)

  included_ids <- out$included %>% collect() %>% pull(case_id)
  excluded_ids <- out$excluded %>% collect() %>% pull(case_id)

  expect_length(intersect(included_ids, excluded_ids), 0)
  expect_setequal(c(included_ids, excluded_ids), test_procedures %>% collect() %>% pull(case_id))
})

# cohort_enforce_opt_out() ---------------------------------------------------

test_that("cohort_enforce_opt_out() removes opt-out and restricted patients by default", {
  opted_out_mrns <- test_demographics %>%
    collect() %>%
    filter(opt_out_flag == "1" | restricted_patient_flag == "1") %>%
    pull(mrn)

  result <- cohort_enforce_opt_out(
    test_procedures,
    demographics_table = test_demographics,
    add_to_eligability_criteria = FALSE
  ) %>% collect()

  expect_true(all(!result$mrn %in% opted_out_mrns))
  expect_true(any(test_procedures %>% collect() %>% pull(mrn) %in% opted_out_mrns)) # sanity: fixture actually has some
})

test_that("cohort_enforce_opt_out() with remove_restricted = FALSE only removes opt_out_flag patients", {
  opted_out_mrns <- test_demographics %>% collect() %>% filter(opt_out_flag == "1") %>% pull(mrn)
  restricted_only_mrns <- test_demographics %>%
    collect() %>%
    filter(restricted_patient_flag == "1" & opt_out_flag != "1") %>%
    pull(mrn)
  skip_if(length(restricted_only_mrns) == 0, "No restricted-only patients in this synthetic fixture")

  result <- cohort_enforce_opt_out(
    test_procedures,
    demographics_table = test_demographics,
    remove_restricted = FALSE,
    add_to_eligability_criteria = FALSE
  ) %>% collect()

  expect_true(all(!result$mrn %in% opted_out_mrns))
  expect_true(any(result$mrn %in% restricted_only_mrns))
})

test_that("cohort_enforce_opt_out() records eligibility criteria correctly", {
  local_reset_eligibility_criteria()
  n_prior <- nrow(test_procedures)
  opted_out_mrns <- test_demographics %>%
    collect() %>%
    filter(opt_out_flag == "1" | restricted_patient_flag == "1") %>%
    pull(mrn)
  n_filtered <- test_procedures %>% filter(mrn %in% opted_out_mrns) %>% collect() %>% nrow()

  cohort_enforce_opt_out(test_procedures, demographics_table = test_demographics, add_to_eligability_criteria = TRUE)

  row <- tail(eligibility_criteria, 1)
  expect_equal(row$function_name, "cohort_enforce_opt_out()")
  expect_equal(row$n_prior, n_prior)
  expect_equal(row$n_filtered, n_filtered)
})

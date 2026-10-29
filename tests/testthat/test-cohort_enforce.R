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

# cohort_enforce_single_procedure() ------------------------------------------
# Regression tests for #60: grouped slice_min() is not supported by Arrow, so
# these run on Arrow tables (as returned by load_dataset()/load_synthetic_dataset()).

single_procedure_fixture <- function() {
  arrow::arrow_table(
    case_id = c("a1", "a2", "a3", "b1", "c1", "c2", "c3"),
    admission_csn = c("A", "A", "A", "B", "C", "C", "C"),
    # A: three procedures; B: single procedure; C: two-way tie on earliest date
    operation_date = as.Date(c("2024-01-03", "2024-01-01", "2024-01-05",
                               "2024-02-01",
                               "2024-03-01", "2024-03-01", "2024-03-09"))
  )
}

test_that("cohort_enforce_single_procedure() works on an Arrow table without erroring", {
  expect_no_error(
    cohort_enforce_single_procedure(single_procedure_fixture(), add_to_eligability_criteria = FALSE) %>% collect()
  )
})

test_that("cohort_enforce_single_procedure() keeps only the earliest procedure per admission_csn", {
  result <- cohort_enforce_single_procedure(single_procedure_fixture(), add_to_eligability_criteria = FALSE) %>%
    collect()

  # A -> a2 (earliest), B -> b1 (only one), C -> c1 and c2 (tied, as slice_min() keeps ties)
  expect_setequal(result$case_id, c("a2", "b1", "c1", "c2"))
  expect_false(any(c("a1", "a3", "c3") %in% result$case_id))
})

test_that("cohort_enforce_single_procedure() leaves the table unchanged when every admission has one procedure", {
  single <- arrow::arrow_table(
    case_id = c("x", "y"),
    admission_csn = c("X", "Y"),
    operation_date = as.Date(c("2024-01-01", "2024-01-02"))
  )

  result <- cohort_enforce_single_procedure(single, add_to_eligability_criteria = FALSE) %>% collect()

  expect_setequal(result$case_id, c("x", "y"))
})

test_that("cohort_enforce_single_procedure() on the synthetic procedures matches a plain dplyr reference", {
  expected <- test_procedures %>%
    collect() %>%
    group_by(admission_csn) %>%
    filter(operation_date == min(operation_date)) %>%
    ungroup()

  result <- cohort_enforce_single_procedure(test_procedures, add_to_eligability_criteria = FALSE) %>% collect()

  expect_setequal(result$case_id, expected$case_id)
})

test_that("cohort_enforce_single_procedure() records eligibility criteria correctly", {
  local_reset_eligibility_criteria()
  fixture <- single_procedure_fixture()

  cohort_enforce_single_procedure(fixture, add_to_eligability_criteria = TRUE)

  row <- tail(eligibility_criteria, 1)
  expect_equal(row$function_name, "cohort_enforce_single_procedure()")
  expect_equal(row$n_prior, 7)
  expect_equal(row$n_filtered, 3)
})

test_that("cohort_enforce_single_procedure() verbose = TRUE reports what was filtered", {
  expect_message(
    cohort_enforce_single_procedure(single_procedure_fixture(), verbose = TRUE, add_to_eligability_criteria = FALSE),
    regexp = "Filtering out"
  )
})

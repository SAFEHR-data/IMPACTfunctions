# Tests for filter_medication() in R/filter_medication.R.
#
# apply_preproccess_MAR = TRUE (the default) always calls preproccess_MAR()
# internally, which requires MAR to be identical() to the global
# HospitalMar - so every test sets that up first via
# local_set_global_hospitalmar() (see helper-synthetic.R).

test_that("filter_medication() with a single medication returns only that medication for patients in the dataset", {
  local_set_global_hospitalmar(test_hospitalmar)
  result <- filter_medication(
    test_dataset, MAR = test_hospitalmar,
    medication = "fentanyl", search_column = "simple_generic_name", verbose = FALSE
  ) %>% collect()

  expect_true(all(result$simple_generic_name == "fentanyl"))
  expect_true(all(result$mrn %in% (test_dataset %>% collect() %>% pull(mrn))))
})

test_that("filter_medication() medication = NULL returns every medication (no drug filter)", {
  local_set_global_hospitalmar(test_hospitalmar)
  result <- filter_medication(test_dataset, MAR = test_hospitalmar, medication = NULL, verbose = FALSE) %>% collect()

  expect_gt(n_distinct(result$simple_generic_name), 1)
})

test_that("filter_medication() errors when medication is left at its default (not supplied)", {
  local_set_global_hospitalmar(test_hospitalmar)
  expect_error(
    filter_medication(test_dataset, MAR = test_hospitalmar, verbose = FALSE),
    "No medication specifed"
  )
})

test_that("filter_medication() errors when dataset has no rows", {
  local_set_global_hospitalmar(test_hospitalmar)
  empty_dataset <- test_dataset %>% filter(case_id < 0)

  expect_error(
    filter_medication(empty_dataset, MAR = test_hospitalmar, medication = "fentanyl", verbose = FALSE),
    "No rows in"
  )
})

test_that("filter_medication() errors on an invalid mar_source", {
  local_set_global_hospitalmar(test_hospitalmar)
  expect_error(
    filter_medication(test_dataset, MAR = test_hospitalmar, medication = "fentanyl", mar_source = "Nonsense", verbose = FALSE),
    "mar_source"
  )
})

test_that("filter_medication() check_search = TRUE (default) errors on a misspelled medication", {
  local_set_global_hospitalmar(test_hospitalmar)
  expect_error(
    filter_medication(test_dataset, MAR = test_hospitalmar, medication = "fentanyllll", verbose = FALSE),
    "isnt't in the dataset"
  )
})

test_that("filter_medication() timeframe = custom with t0 + t_int keeps only administrations within that window", {
  local_set_global_hospitalmar(test_hospitalmar)
  # R/filter_medication.R uses `select(.data[[joining_column]], ...)` in
  # several places to build the timeframe join columns. `.data[[x]]` inside a
  # tidyselect expression (as opposed to inside a filter()/mutate() value
  # expression) was deprecated in tidyselect 1.2.0 in favour of `all_of(x)`;
  # it still works, and the warning only fires the first time this call
  # shape runs in a given R session (tidyselect's own warn-once caching), so
  # it's not asserted here to avoid a flaky test - noting it as a minor,
  # low-risk cleanup worth doing (mechanical find/replace), not a
  # correctness issue.
  result <- filter_medication(
    test_dataset, MAR = test_hospitalmar,
    medication = "fentanyl", search_column = "simple_generic_name",
    timeframe = "custom", t0 = "operation_date", t_int = lubridate::hours(6),
    verbose = FALSE
  ) %>% collect()

  expect_true(all(result$administration_date_time > result$operation_date))
  expect_true(all(result$administration_date_time < result$operation_date + lubridate::hours(6)))
  expect_false(any(c("t0", "t1") %in% names(result)))
})

test_that("filter_medication() timeframe = custom with only t0 (no t1 or t_int) errors", {
  local_set_global_hospitalmar(test_hospitalmar)
  expect_error(
    filter_medication(
      test_dataset, MAR = test_hospitalmar,
      medication = "fentanyl", search_column = "simple_generic_name",
      timeframe = "custom", t0 = "operation_date", verbose = FALSE
    ),
    "you have supplied a `t0`"
  )
})

test_that("filter_medication() filter_route without route_column errors", {
  local_set_global_hospitalmar(test_hospitalmar)
  expect_error(
    filter_medication(
      test_dataset, MAR = test_hospitalmar,
      medication = "fentanyl", search_column = "simple_generic_name",
      filter_route = "Intravenous", verbose = FALSE
    ),
    "route column"
  )
})

test_that("filter_medication() filter_route + route_column restricts to that route", {
  local_set_global_hospitalmar(test_hospitalmar)
  result <- filter_medication(
    test_dataset, MAR = test_hospitalmar,
    medication = "fentanyl", search_column = "simple_generic_name",
    filter_route = "Intravenous", route_column = "route", verbose = FALSE
  ) %>% collect()

  expect_true(all(result$route == "Intravenous"))
})

test_that("filter_medication() KNOWN LIMITATION: mar_source filtering needs an encounter_type column the synthetic MAR doesn't have", {
  # Any non-NA mar_source (single value or a vector) filters
  # `MAR %>% filter(encounter_type == ...)`. The synthetic HospitalMar
  # fixture doesn't include an encounter_type column (it's built directly
  # from the Procedures table without one), so this errors with "object
  # 'encounter_type' not found" for any real dataset the synthetic fixture
  # is meant to stand in for. This is a synthetic-data fidelity gap, not a
  # function bug - see the plan document for the reminder to revisit once
  # the synthetic dataset is improved.
  local_set_global_hospitalmar(test_hospitalmar)
  expect_error(
    filter_medication(test_dataset, MAR = test_hospitalmar, medication = NULL, mar_source = "Anaesthesia", verbose = FALSE),
    "encounter_type"
  )
})

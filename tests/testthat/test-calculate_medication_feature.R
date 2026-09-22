# Tests for calculate_medication_feature() in R/calculate_medication_feature.R.

local_fentanyl_mar <- function() {
  local_set_global_hospitalmar(test_hospitalmar, env = parent.frame())
  extract_medication(test_dataset, MAR = test_hospitalmar, medication = "fentanyl", search_column = "simple_generic_name", verbose = FALSE)
}

test_that("calculate_medication_feature() feature = \"binary\" flags admission_csns with a matching MAR entry", {
  fentanyl <- local_fentanyl_mar()
  result <- calculate_medication_feature(
    test_dataset, MAR = fentanyl, format = "table", feature = "binary", feature_name = "fentanyl_given",
    override_dose_check = TRUE, override_route_check = TRUE, verbose = FALSE
  )

  fentanyl_csns <- fentanyl %>% collect() %>% pull(admission_csn)
  dataset_csns <- test_dataset %>% collect() %>% pull(admission_csn)
  expected_true <- sum(dataset_csns %in% fentanyl_csns)

  expect_equal(sum(result$fentanyl_given), expected_true)
})

test_that("calculate_medication_feature() feature = \"count\"/\"sum_dose\"/\"sum_ome\" match an independent group_by oracle", {
  fentanyl <- local_fentanyl_mar()
  fentanyl_df <- fentanyl %>% collect()

  count_result <- calculate_medication_feature(
    test_dataset, MAR = fentanyl, format = "table", feature = "count", feature_name = "n",
    override_dose_check = TRUE, override_route_check = TRUE, verbose = FALSE
  )
  count_expected <- fentanyl_df %>% count(admission_csn, name = "n")
  expect_equal(
    count_result %>% arrange(admission_csn) %>% pull(n),
    count_expected %>% arrange(admission_csn) %>% pull(n)
  )

  dose_result <- calculate_medication_feature(
    test_dataset, MAR = fentanyl, format = "table", feature = "sum_dose", feature_name = "dose",
    override_dose_check = TRUE, override_route_check = TRUE, verbose = FALSE
  )
  dose_expected <- fentanyl_df %>% group_by(admission_csn) %>% summarise(dose = sum(dose, na.rm = FALSE), .groups = "drop")
  expect_equal(
    dose_result %>% arrange(admission_csn) %>% pull(dose),
    dose_expected %>% arrange(admission_csn) %>% pull(dose)
  )

  ome_result <- calculate_medication_feature(
    test_dataset, MAR = fentanyl, format = "table", feature = "sum_ome", feature_name = "ome",
    override_dose_check = TRUE, override_route_check = TRUE, verbose = FALSE
  )
  ome_expected <- fentanyl_df %>% group_by(admission_csn) %>% summarise(ome = sum(ome, na.rm = TRUE), .groups = "drop")
  expect_equal(
    ome_result %>% arrange(admission_csn) %>% pull(ome),
    ome_expected %>% arrange(admission_csn) %>% pull(ome)
  )
})

test_that("calculate_medication_feature() format = \"joined\" left-joins the feature onto the full dataset", {
  fentanyl <- local_fentanyl_mar()
  result <- calculate_medication_feature(
    test_dataset, MAR = fentanyl, format = "joined", feature = "count", feature_name = "fentanyl_count",
    override_dose_check = TRUE, override_route_check = TRUE, verbose = FALSE
  ) %>% collect()

  expect_equal(nrow(result), nrow(test_dataset %>% collect()))
  expect_true("fentanyl_count" %in% names(result))
})

test_that("calculate_medication_feature() errors on an unrecognised feature", {
  fentanyl <- local_fentanyl_mar()
  expect_error(
    calculate_medication_feature(
      test_dataset, MAR = fentanyl, format = "table", feature = "bogus", feature_name = "x",
      override_dose_check = TRUE, override_route_check = TRUE, verbose = FALSE
    ),
    "not recognised"
  )
})

test_that("calculate_medication_feature() errors on mismatched dose_units unless override_dose_check = TRUE", {
  local_set_global_hospitalmar(test_hospitalmar)
  mixed <- extract_medication(test_dataset, MAR = test_hospitalmar, medication = NULL, verbose = FALSE)

  expect_error(
    calculate_medication_feature(test_dataset, MAR = mixed, format = "table", feature = "sum_dose", feature_name = "x", verbose = FALSE),
    "Not all dose units"
  )
  expect_no_error(
    calculate_medication_feature(test_dataset, MAR = mixed, format = "table", feature = "count", feature_name = "x", override_dose_check = TRUE, override_route_check = TRUE, verbose = FALSE)
  )
})

test_that("calculate_medication_feature() errors on mismatched routes unless override_route_check = TRUE", {
  fentanyl <- local_fentanyl_mar()
  expect_error(
    calculate_medication_feature(test_dataset, MAR = fentanyl, format = "table", feature = "count", feature_name = "x", override_dose_check = TRUE, verbose = FALSE),
    "Not all medication routes"
  )
})

test_that("calculate_medication_feature() errors on a duplicated link_column in dataset", {
  fentanyl <- local_fentanyl_mar()
  dup_dataset <- bind_rows(test_dataset %>% collect() %>% head(2), test_dataset %>% collect() %>% head(1))

  expect_error(
    calculate_medication_feature(dup_dataset, MAR = fentanyl, format = "table", feature = "count", feature_name = "x", override_dose_check = TRUE, override_route_check = TRUE, verbose = FALSE),
    "Duplication error"
  )
})

test_that("calculate_medication_feature() errors on an unrecognised link_column", {
  fentanyl <- local_fentanyl_mar()
  expect_error(
    calculate_medication_feature(test_dataset, MAR = fentanyl, format = "table", feature = "count", feature_name = "x", link_column = "bogus_csn", override_dose_check = TRUE, override_route_check = TRUE, verbose = FALSE),
    "not one of"
  )
})

test_that("calculate_medication_feature() KNOWN ISSUE: the binary \"no positive result\" warning fires whenever not every row matched, not when none did", {
  # `if(!all(result$match_status)){ cli_alert_warning("No patients have a
  # positive result...") }` fires whenever ANY row is FALSE - which is the
  # normal case (most cohorts won't have 100% of patients on a given
  # medication) - not when NONE matched, as the message implies. Verified:
  # 8952/9677 rows were TRUE and the warning still fired. Documenting
  # current behaviour; the condition likely should be `!any(...)`.
  fentanyl <- local_fentanyl_mar()
  expect_message(
    calculate_medication_feature(
      test_dataset, MAR = fentanyl, format = "table", feature = "binary", feature_name = "x",
      override_dose_check = TRUE, override_route_check = TRUE, verbose = FALSE
    ),
    "No patients have a positive result"
  )
})

test_that("calculate_medication_feature() KNOWN ISSUE: the \"no result\" warning for count/sum_dose/sum_ome checks nrow(result) == 1, not == 0", {
  # Same pattern as the binary case above: the warning's message says "No
  # patients have a summed dose result", but the condition
  # `nrow(result) == 1` actually fires whenever there's exactly one distinct
  # admission_csn group in the MAR subset - even when that one group has a
  # perfectly real, non-zero result. Documenting current behaviour; the
  # condition likely should be `nrow(result) == 0`.
  local_set_global_hospitalmar(test_hospitalmar)
  fentanyl <- extract_medication(test_dataset, MAR = test_hospitalmar, medication = "fentanyl", search_column = "simple_generic_name", verbose = FALSE)
  single_csn <- fentanyl %>% filter(admission_csn == (fentanyl %>% collect() %>% pull(admission_csn) %>% head(1)))

  args <- list(
    test_dataset, MAR = single_csn, format = "table", feature = "count", feature_name = "x",
    override_dose_check = TRUE, override_route_check = TRUE, verbose = FALSE
  )
  expect_message(do.call(calculate_medication_feature, args), "No patients have a summed dose result")

  result <- suppressMessages(do.call(calculate_medication_feature, args))
  expect_equal(nrow(result), 1)
  expect_gt(result$x[1], 0) # a real, non-zero count - not "no result"
})

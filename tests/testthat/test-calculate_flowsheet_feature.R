# Tests for calculate_flowsheet_feature() in R/calculate_flowsheet_feature.R.

local_pulse_flowsheet <- function() {
  filter_flowsheet_data(test_dataset, flowsheet = test_measurements, search_string = "Pulse", timeframe = "whole_flowsheet", verbose = FALSE)
}

test_that("calculate_flowsheet_feature() feature = \"binary\" flags admission_csns with a matching flowsheet entry", {
  pulse <- local_pulse_flowsheet()
  result <- calculate_flowsheet_feature(test_dataset, flowsheet = pulse, format = "table", feature = "binary", feature_name = "has_pulse", verbose = FALSE)

  pulse_csns <- pulse %>% collect() %>% pull(admission_csn)
  dataset_csns <- test_dataset %>% collect() %>% pull(admission_csn)
  expect_equal(sum(result$has_pulse), sum(dataset_csns %in% pulse_csns))
})

test_that("calculate_flowsheet_feature() feature = \"sum_values\"/\"mean_values\"/\"count_of_values\" match an independent group_by oracle", {
  pulse <- local_pulse_flowsheet()
  pulse_df <- pulse %>% collect()

  sum_result <- calculate_flowsheet_feature(test_dataset, flowsheet = pulse, format = "table", feature = "sum_values", feature_name = "v", verbose = FALSE)
  sum_expected <- pulse_df %>% group_by(admission_csn) %>% summarise(v = sum(value, na.rm = TRUE), .groups = "drop")
  expect_equal(sum_result %>% arrange(admission_csn) %>% pull(v), sum_expected %>% arrange(admission_csn) %>% pull(v))

  mean_result <- calculate_flowsheet_feature(test_dataset, flowsheet = pulse, format = "table", feature = "mean_values", feature_name = "v", verbose = FALSE)
  mean_expected <- pulse_df %>% group_by(admission_csn) %>% summarise(v = mean(value, na.rm = TRUE), .groups = "drop")
  expect_equal(mean_result %>% arrange(admission_csn) %>% pull(v), mean_expected %>% arrange(admission_csn) %>% pull(v))

  count_result <- calculate_flowsheet_feature(test_dataset, flowsheet = pulse, format = "table", feature = "count_of_values", feature_name = "v", verbose = FALSE)
  count_expected <- pulse_df %>% count(admission_csn, name = "v")
  expect_equal(count_result %>% arrange(admission_csn) %>% pull(v), count_expected %>% arrange(admission_csn) %>% pull(v))
})

test_that("calculate_flowsheet_feature() feature = \"list_values\" returns a list-column of each admission_csn's values", {
  pulse <- local_pulse_flowsheet()
  pulse_df <- pulse %>% collect()
  result <- calculate_flowsheet_feature(test_dataset, flowsheet = pulse, format = "table", feature = "list_values", feature_name = "values", verbose = FALSE)

  expect_type(result$values, "list")
  one_csn <- result$admission_csn[1]
  expect_setequal(result$values[result$admission_csn == one_csn][[1]], pulse_df$value[pulse_df$admission_csn == one_csn])
})

test_that("calculate_flowsheet_feature() format = \"joined\" left-joins the feature onto the full dataset", {
  pulse <- local_pulse_flowsheet()
  result <- calculate_flowsheet_feature(test_dataset, flowsheet = pulse, format = "joined", feature = "count_of_values", feature_name = "pulse_count", verbose = FALSE) %>% collect()

  expect_equal(nrow(result), nrow(test_dataset %>% collect()))
  expect_true("pulse_count" %in% names(result))
})

test_that("calculate_flowsheet_feature() errors on an unrecognised feature", {
  pulse <- local_pulse_flowsheet()
  expect_error(
    calculate_flowsheet_feature(test_dataset, flowsheet = pulse, format = "table", feature = "bogus", feature_name = "x", verbose = FALSE),
    "must be one of"
  )
})

test_that("calculate_flowsheet_feature() errors on an unrecognised format", {
  pulse <- local_pulse_flowsheet()
  expect_error(
    calculate_flowsheet_feature(test_dataset, flowsheet = pulse, format = "bogus", feature = "count_of_values", feature_name = "x", verbose = FALSE),
    "must be one of"
  )
})

test_that("calculate_flowsheet_feature() errors on a duplicated link_column in dataset", {
  pulse <- local_pulse_flowsheet()
  dup_dataset <- bind_rows(test_dataset %>% collect() %>% head(2), test_dataset %>% collect() %>% head(1))

  expect_error(
    calculate_flowsheet_feature(dup_dataset, flowsheet = pulse, format = "table", feature = "count_of_values", feature_name = "x", verbose = FALSE),
    "Duplication error"
  )
})

test_that("calculate_flowsheet_feature() errors on an unrecognised link_column", {
  pulse <- local_pulse_flowsheet()
  expect_error(
    calculate_flowsheet_feature(test_dataset, flowsheet = pulse, format = "table", feature = "count_of_values", feature_name = "x", link_column = "bogus_csn", verbose = FALSE),
    "must be one of"
  )
})

test_that("calculate_flowsheet_feature() KNOWN ISSUE: the multiple-flowsheet consistency check never actually triggers", {
  # `flowsheets_present <- unique(flowsheet %>% select(flowsheet_row_epic_id) %>% collect())`
  # is a one-column data frame. `length(flowsheets_present)` counts its
  # *columns* (always 1), not its distinct rows - so
  # `if (length(flowsheets_present) == 1)` is always TRUE, and the intended
  # "abort/override when more than one flowsheet is mixed together" check
  # (which should presumably use nrow(), not length()) is dead code. Mixing
  # two incompatible flowsheets (e.g. Pulse and BP, different scales/units)
  # into one sum/mean currently proceeds silently with no error and no need
  # for override_flowsheet_check = TRUE. Verified there are 2 distinct
  # flowsheet_row_epic_id values in the combined data below, yet no error is
  # raised. This looks like a real bug worth a GitHub issue given it can
  # silently produce meaningless aggregated results.
  multi <- filter_flowsheet_data(test_dataset, flowsheet = test_measurements, search_string = c("Pulse", "BP"), timeframe = "whole_flowsheet", verbose = FALSE)
  distinct_flowsheets <- multi %>% select(flowsheet_row_epic_id) %>% distinct() %>% collect()
  expect_gt(nrow(distinct_flowsheets), 1) # sanity: genuinely more than one flowsheet in the mix

  expect_no_error(
    calculate_flowsheet_feature(test_dataset, flowsheet = multi, format = "table", feature = "sum_values", feature_name = "x", verbose = FALSE)
  )
})

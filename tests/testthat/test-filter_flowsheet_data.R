# Tests for filter_flowsheet_data() in R/filter_flowsheet_data.R.

test_that("filter_flowsheet_data() KNOWN ISSUE: calling without an explicit timeframe crashes", {
  # When `timeframe` is left at its default (not supplied), the function's
  # `if (missing(timeframe)) { timeframe <- "whole_flowsheet" } else if
  # (timeframe == "whole_flowsheet") { timeframe_df <- NA } else if ...`
  # chain means the branch that sets `timeframe_df <- NA` is only reached
  # via the *second* condition - which is skipped once the first
  # (`missing(timeframe)`) branch has already matched. So `timeframe_df` is
  # never assigned, and the function crashes later with "object
  # 'timeframe_df' not found" - on the exact call shape shown in the
  # function's own @examples (no `timeframe` argument at all). Passing
  # `timeframe = "whole_flowsheet"` explicitly avoids this and is used in
  # every other test in this file. This looks like a real regression worth
  # a GitHub issue given it breaks the documented default usage.
  expect_error(
    filter_flowsheet_data(test_dataset, flowsheet = test_measurements, search_string = "Pulse", verbose = FALSE),
    "timeframe_df"
  )
})

test_that("filter_flowsheet_data() whole_flowsheet returns all entries for the requested question, for patients in the dataset", {
  expected <- test_measurements %>%
    collect() %>%
    filter(flowsheet_question_name == "Pulse", admission_csn %in% (test_dataset %>% collect() %>% pull(admission_csn)))

  result <- filter_flowsheet_data(test_dataset, flowsheet = test_measurements, search_string = "Pulse", timeframe = "whole_flowsheet", verbose = FALSE) %>%
    collect()

  expect_equal(nrow(result), nrow(expected))
  expect_true(all(result$flowsheet_question_name == "Pulse"))
})

test_that("filter_flowsheet_data() search_string = NULL returns every question", {
  result <- filter_flowsheet_data(test_dataset, flowsheet = test_measurements, search_string = NULL, timeframe = "whole_flowsheet", verbose = FALSE) %>%
    collect()

  expect_gt(n_distinct(result$flowsheet_question_name), 1)
})

test_that("filter_flowsheet_data() errors when flowsheet_question_name is left at its default (not supplied)", {
  expect_error(
    filter_flowsheet_data(test_dataset, flowsheet = test_measurements, timeframe = "whole_flowsheet", verbose = FALSE),
    "No value specified"
  )
})

test_that("filter_flowsheet_data() treats a vector of question names as an OR match", {
  result <- filter_flowsheet_data(test_dataset, flowsheet = test_measurements, search_string = c("Pulse", "BP"), timeframe = "whole_flowsheet", verbose = FALSE) %>%
    collect()

  expect_setequal(result$flowsheet_question_name, c("Pulse", "BP"))
})

test_that("filter_flowsheet_data() check_search = TRUE (default) errors on a misspelled question name", {
  expect_error(
    filter_flowsheet_data(test_dataset, flowsheet = test_measurements, search_string = "Pulsexyz", timeframe = "whole_flowsheet", verbose = FALSE),
    "not found"
  )
})

test_that("filter_flowsheet_data() errors when dataset has no rows", {
  empty_dataset <- test_dataset %>% filter(case_id < 0)
  expect_error(
    filter_flowsheet_data(empty_dataset, flowsheet = test_measurements, search_string = "Pulse", timeframe = "whole_flowsheet", verbose = FALSE),
    "No rows in"
  )
})

test_that("filter_flowsheet_data() timeframe = pre_op only keeps entries between admission and operation", {
  dataset_df <- test_dataset %>% collect()
  result <- filter_flowsheet_data(test_dataset, flowsheet = test_measurements, search_string = "Pulse", timeframe = "pre_op", verbose = FALSE) %>%
    collect() %>%
    left_join(dataset_df %>% select(admission_csn, admission_date, operation_date), by = "admission_csn")

  expect_true(all(result$recorded_date > result$admission_date))
  expect_true(all(result$recorded_date < result$operation_date))
})

test_that("filter_flowsheet_data() timeframe = custom with t0 + t_int keeps only entries within that window", {
  dataset_df <- test_dataset %>% collect()
  result <- filter_flowsheet_data(
    test_dataset, flowsheet = test_measurements, search_string = "Pulse",
    timeframe = "custom", t0 = "operation_date", t_int = lubridate::hours(6), verbose = FALSE
  ) %>%
    collect() %>%
    left_join(dataset_df %>% select(admission_csn, operation_date), by = "admission_csn")

  expect_true(all(result$recorded_date > result$operation_date))
  expect_true(all(result$recorded_date < result$operation_date + lubridate::hours(6)))
})

test_that("filter_flowsheet_data() timeframe = custom with only t0 falls through to a generic, unhelpful error", {
  # filter_medication() has an explicit check for "t0 supplied but neither
  # t1 nor t_int" with a clear message. filter_flowsheet_data() has no such
  # check, so the same (fully reachable, ordinary) mistake instead falls
  # through to the function's "you should not have been able to trigger
  # this - please open a github issue" catch-all, which is misleading here.
  # Documenting current behaviour; consider adding the same explicit check
  # filter_medication() has.
  expect_error(
    filter_flowsheet_data(test_dataset, flowsheet = test_measurements, search_string = "Pulse", timeframe = "custom", t0 = "operation_date", verbose = FALSE),
    "Unexpected state"
  )
})

test_that("filter_flowsheet_data() warns (but doesn't error) when dataset has a duplicated timeframe_csn", {
  dup_dataset <- bind_rows(test_dataset %>% collect() %>% head(2), test_dataset %>% collect() %>% head(1))

  expect_message(
    filter_flowsheet_data(dup_dataset, flowsheet = test_measurements, search_string = "Pulse", timeframe = "whole_flowsheet", verbose = FALSE),
    "duplication"
  )
})

test_that("filter_flowsheet_data() timeframe_csn lets the timeframe join use a different column (e.g. anaesthesia_csn)", {
  dataset_df <- test_dataset %>% collect()
  result <- filter_flowsheet_data(
    test_dataset, flowsheet = test_measurements, search_string = "Pulse",
    timeframe = "pre_op", timeframe_csn = "anaesthesia_csn", verbose = FALSE
  ) %>%
    collect() %>%
    left_join(dataset_df %>% select(anaesthesia_csn, admission_date, operation_date), by = "anaesthesia_csn")

  expect_true(all(result$recorded_date > result$admission_date))
  expect_true(all(result$recorded_date < result$operation_date))
})

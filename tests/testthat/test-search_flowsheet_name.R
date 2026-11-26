# Tests for search_flowsheet_name() in R/search_flowsheet_name.R.

test_that("search_flowsheet_name() soft search matches case-insensitively on flowsheet_question_name", {
  summary_all <- test_measurements %>%
    group_by(flowsheet_question_name) %>%
    summarise(rows = n(), .groups = "drop") %>%
    collect()

  expected <- summary_all %>% filter(grepl("pulse", flowsheet_question_name, ignore.case = TRUE))

  result <- search_flowsheet_name("pulse", test_measurements, verbose = FALSE)

  expect_setequal(result$flowsheet_question_name, expected$flowsheet_question_name)
})

test_that("search_flowsheet_name() soft search is case-insensitive regardless of query casing", {
  lower <- search_flowsheet_name("pulse", test_measurements, verbose = FALSE)
  upper <- search_flowsheet_name("PULSE", test_measurements, verbose = FALSE)

  expect_equal(nrow(lower), nrow(upper))
})

test_that("search_flowsheet_name() treats a vector of strings as an OR search", {
  result <- search_flowsheet_name(c("pulse", "bp"), test_measurements, verbose = FALSE)

  expect_true("Pulse" %in% result$flowsheet_question_name)
  expect_true("BP" %in% result$flowsheet_question_name)
})

test_that("search_flowsheet_name() strict_search = TRUE without search_column errors", {
  expect_error(
    search_flowsheet_name("Pulse", test_measurements, strict_search = TRUE, verbose = FALSE),
    "Strict search requires"
  )
})

test_that("search_flowsheet_name() strict_search = TRUE does an exact, case-sensitive match in search_column", {
  exact <- search_flowsheet_name("Pulse", test_measurements, search_column = "flowsheet_question_name", strict_search = TRUE, verbose = FALSE)
  expect_true(all(exact$flowsheet_question_name == "Pulse"))

  wrong_case <- search_flowsheet_name("pulse", test_measurements, search_column = "flowsheet_question_name", strict_search = TRUE, verbose = FALSE)
  expect_equal(nrow(wrong_case), 0)
})

test_that("search_flowsheet_name() returns a 0-row tibble (not an error) when nothing matches", {
  result <- search_flowsheet_name("nonexistent_xyz", test_measurements, verbose = FALSE)
  expect_equal(nrow(result), 0)
})

test_that("search_flowsheet_name() returns both flowsheet_question_name and flowsheet_row_epic_id", {
  result <- search_flowsheet_name("pulse", test_measurements, verbose = FALSE)
  expect_true(all(c("flowsheet_question_name", "flowsheet_row_epic_id", "rows") %in% names(result)))
})

test_that("search_flowsheet_name() soft search also matches on flowsheet_row_epic_id", {
  pulse_id <- search_flowsheet_name("Pulse", test_measurements, search_column = "flowsheet_question_name",
                                    strict_search = TRUE, verbose = FALSE)$flowsheet_row_epic_id[1]

  result <- search_flowsheet_name(pulse_id, test_measurements, verbose = FALSE)
  expect_true("Pulse" %in% result$flowsheet_question_name)
})

test_that("search_flowsheet_name() strict_search = TRUE does an exact match on flowsheet_row_epic_id", {
  pulse_id <- search_flowsheet_name("Pulse", test_measurements, search_column = "flowsheet_question_name",
                                    strict_search = TRUE, verbose = FALSE)$flowsheet_row_epic_id

  exact <- search_flowsheet_name(pulse_id, test_measurements, search_column = "flowsheet_row_epic_id",
                                 strict_search = TRUE, verbose = FALSE)
  expect_true(nrow(exact) > 0)
  expect_true(all(exact$flowsheet_row_epic_id %in% pulse_id))
  expect_true(all(exact$flowsheet_question_name == "Pulse"))

  # A partial ID must not match under strict search
  partial <- search_flowsheet_name(substr(pulse_id[1], 1, 4), test_measurements,
                                   search_column = "flowsheet_row_epic_id", strict_search = TRUE, verbose = FALSE)
  expect_equal(nrow(partial), 0)
})

test_that("search_flowsheet_name() strict search on the wrong column returns no rows", {
  result <- search_flowsheet_name("Pulse", test_measurements, search_column = "flowsheet_row_epic_id",
                                  strict_search = TRUE, verbose = FALSE)
  expect_equal(nrow(result), 0)
})

test_that("search_flowsheet_name() errors on an invalid search_column", {
  expect_error(
    search_flowsheet_name("Pulse", test_measurements, search_column = "display_name", verbose = FALSE),
    "is not valid"
  )
})

test_that("search_flowsheet_name() strict search errors when given more than one search_column", {
  expect_error(
    search_flowsheet_name("Pulse", test_measurements,
                          search_column = c("flowsheet_question_name", "flowsheet_row_epic_id"),
                          strict_search = TRUE, verbose = FALSE),
    "single"
  )
})

test_that("search_flowsheet_name() FEATURE: soft search with search_column set still returns matches from both columns", {
  restricted <- search_flowsheet_name("pulse", test_measurements, search_column = "flowsheet_row_epic_id", verbose = FALSE)
  unrestricted <- search_flowsheet_name("pulse", test_measurements, verbose = FALSE)

  expect_identical(restricted, unrestricted)
})

test_that("search_flowsheet_name() warns when the term doesn't match within the specified search_column", {
  expect_message(
    search_flowsheet_name("pulse", test_measurements, search_column = "flowsheet_row_epic_id", verbose = FALSE),
    "No match for .*pulse.* found in the specified"
  )
})

test_that("search_flowsheet_name() does not warn when the term matches within the specified search_column", {
  expect_no_message(
    search_flowsheet_name("pulse", test_measurements, search_column = "flowsheet_question_name", verbose = FALSE),
    message = "No match for"
  )
})

test_that("search_flowsheet_name() does not warn when search_column is left at its default", {
  expect_no_message(
    search_flowsheet_name("pulse", test_measurements, verbose = FALSE),
    message = "No match for"
  )
})

test_that("filter_flowsheet_data() check_search works with search_column = flowsheet_row_epic_id", {
  pulse_id <- search_flowsheet_name("Pulse", test_measurements, search_column = "flowsheet_question_name",
                                    strict_search = TRUE, verbose = FALSE)$flowsheet_row_epic_id

  result <- filter_flowsheet_data(test_dataset, flowsheet = test_measurements, search_string = pulse_id,
                                  search_column = "flowsheet_row_epic_id", check_search = TRUE,
                                  timeframe = "whole_flowsheet", verbose = FALSE) %>%
    collect()
  expect_true(nrow(result) > 0)
  expect_true(all(result$flowsheet_question_name == "Pulse"))
})

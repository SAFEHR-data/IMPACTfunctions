# Tests for search_flowsheet_name() in R/search_flowsheet_name.R.

test_that("search_flowsheet_name() soft search matches case-insensitively on flowsheet_question_name", {
  summary_all <- test_measurements %>%
    group_by(flowsheet_question_name) %>%
    summarise(rows = n(), .groups = "drop") %>%
    collect()

  expected <- summary_all %>% filter(grepl("pulse", flowsheet_question_name, ignore.case = TRUE))

  result <- search_flowsheet_name(test_measurements, "pulse", verbose = FALSE)

  expect_setequal(result$flowsheet_question_name, expected$flowsheet_question_name)
})

test_that("search_flowsheet_name() soft search is case-insensitive regardless of query casing", {
  lower <- search_flowsheet_name(test_measurements, "pulse", verbose = FALSE)
  upper <- search_flowsheet_name(test_measurements, "PULSE", verbose = FALSE)

  expect_equal(nrow(lower), nrow(upper))
})

test_that("search_flowsheet_name() treats a vector of strings as an OR search", {
  result <- search_flowsheet_name(test_measurements, c("pulse", "bp"), verbose = FALSE)

  expect_true("Pulse" %in% result$flowsheet_question_name)
  expect_true("BP" %in% result$flowsheet_question_name)
})

test_that("search_flowsheet_name() strict_search = TRUE without search_column errors", {
  expect_error(
    search_flowsheet_name(test_measurements, "Pulse", strict_search = TRUE, verbose = FALSE),
    "Strict search requires"
  )
})

test_that("search_flowsheet_name() strict_search = TRUE does an exact, case-sensitive match in search_column", {
  exact <- search_flowsheet_name(test_measurements, "Pulse", search_column = "flowsheet_question_name", strict_search = TRUE, verbose = FALSE)
  expect_true(all(exact$flowsheet_question_name == "Pulse"))

  wrong_case <- search_flowsheet_name(test_measurements, "pulse", search_column = "flowsheet_question_name", strict_search = TRUE, verbose = FALSE)
  expect_equal(nrow(wrong_case), 0)
})

test_that("search_flowsheet_name() returns a 0-row tibble (not an error) when nothing matches", {
  result <- search_flowsheet_name(test_measurements, "nonexistent_xyz", verbose = FALSE)
  expect_equal(nrow(result), 0)
})

test_that("search_flowsheet_name() search_column = \"display_name\" (real IMPACT data only, not the synthetic fixture)", {
  # The real flowsheet has both flowsheet_question_name and display_name
  # columns, and @param search_column documents display_name as a valid hard
  # -search option. The synthetic Measurements fixture doesn't include a
  # display_name column yet, so this can't be exercised until the synthetic
  # dataset's fidelity is improved to add it - see the plan document's
  # reminders. Skipping rather than asserting the current (unrepresentative)
  # error against the synthetic fixture.
  skip("display_name column not yet present in the synthetic Measurements fixture")

  result <- search_flowsheet_name(test_measurements, "pulse", search_column = "display_name", verbose = FALSE)
  expect_true(all(!is.na(result$display_name)))
})

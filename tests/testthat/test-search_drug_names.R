# Tests for search_drug_names() in R/search_drug_names.R.

test_that("search_drug_names() soft search matches case-insensitively across all 5 name/class columns", {
  summary_all <- test_hospitalmar %>%
    group_by(medication_name, generic_name, simple_generic_name, pharmaceutical_class, pharmaceutical_subclass) %>%
    summarise(rows = n(), .groups = "drop") %>%
    collect()

  expected <- summary_all %>% filter(
    grepl("fentanyl", medication_name, ignore.case = TRUE) |
      grepl("fentanyl", generic_name, ignore.case = TRUE) |
      grepl("fentanyl", simple_generic_name, ignore.case = TRUE) |
      grepl("fentanyl", pharmaceutical_class, ignore.case = TRUE) |
      grepl("fentanyl", pharmaceutical_subclass, ignore.case = TRUE)
  )

  result <- search_drug_names(test_hospitalmar, "fentanyl", verbose = FALSE)

  expect_setequal(result$medication_name, expected$medication_name)
  expect_equal(nrow(result), nrow(expected))
})

test_that("search_drug_names() soft search is case-insensitive regardless of query casing", {
  lower <- search_drug_names(test_hospitalmar, "fentanyl", verbose = FALSE)
  upper <- search_drug_names(test_hospitalmar, "FENTANYL", verbose = FALSE)

  expect_equal(nrow(lower), nrow(upper))
})

test_that("search_drug_names() treats a vector of medications as an OR search", {
  result <- search_drug_names(test_hospitalmar, c("fentanyl", "paracetamol"), verbose = FALSE)

  expect_true(all(grepl("fentanyl|paracetamol", result$simple_generic_name, ignore.case = TRUE)))
  expect_true(any(grepl("fentanyl", result$simple_generic_name, ignore.case = TRUE)))
  expect_true(any(grepl("paracetamol", result$simple_generic_name, ignore.case = TRUE)))
})

test_that("search_drug_names() strict_search = TRUE without search_column errors", {
  expect_error(
    search_drug_names(test_hospitalmar, "fentanyl", strict_search = TRUE, verbose = FALSE),
    "strict search"
  )
})

test_that("search_drug_names() strict_search = TRUE does an exact, case-sensitive match in search_column", {
  exact <- search_drug_names(test_hospitalmar, "fentanyl", search_column = "simple_generic_name", strict_search = TRUE, verbose = FALSE)
  expect_true(all(exact$simple_generic_name == "fentanyl"))

  # Exact match is case-sensitive: the synthetic data stores it lower-case
  wrong_case <- search_drug_names(test_hospitalmar, "FENTANYL", search_column = "simple_generic_name", strict_search = TRUE, verbose = FALSE)
  expect_equal(nrow(wrong_case), 0)
})

test_that("search_drug_names() returns a 0-row tibble (not an error) when nothing matches", {
  result <- search_drug_names(test_hospitalmar, "nonexistent_drug_xyz", verbose = FALSE)
  expect_equal(nrow(result), 0)
})

test_that("search_drug_names() FEATURE: soft search with search_column set still returns matches from every column", {
  # By design, search_column is only *enforced* when strict_search = TRUE.
  # For a soft search it's informational only - the function still searches
  # every column, so a typo in search_column (or a term that happens to
  # match a different column) is still surfaced to help the caller debug
  # their search, rather than silently hiding results.
  restricted <- search_drug_names(test_hospitalmar, "fentanyl", search_column = "pharmaceutical_class", verbose = FALSE)
  unrestricted <- search_drug_names(test_hospitalmar, "fentanyl", verbose = FALSE)

  expect_identical(restricted, unrestricted)
})

test_that("search_drug_names() warns when the term doesn't match within the specified search_column", {
  # "fentanyl" matches simple_generic_name/medication_name/generic_name but
  # never pharmaceutical_class, so specifying pharmaceutical_class should
  # warn that the term wasn't found there, while still returning the full
  # cross-column result set (see FEATURE test above).
  expect_message(
    search_drug_names(test_hospitalmar, "fentanyl", search_column = "pharmaceutical_class", verbose = FALSE),
    "No match for .*fentanyl.* found in the specified"
  )
})

test_that("search_drug_names() does not warn when the term does match within the specified search_column", {
  expect_no_message(
    search_drug_names(test_hospitalmar, "fentanyl", search_column = "simple_generic_name", verbose = FALSE),
    message = "No match for"
  )
})

test_that("search_drug_names() does not check for a search_column match warning when search_column is left at its default", {
  expect_no_message(
    search_drug_names(test_hospitalmar, "fentanyl", verbose = FALSE),
    message = "No match for"
  )
})

test_that("search_drug_names() errors on an invalid search_column", {
  expect_error(
    search_drug_names(test_hospitalmar, "fentanyl", search_column = "not_a_column", verbose = FALSE),
    "is not valid"
  )
  expect_error(
    search_drug_names(test_hospitalmar, "fentanyl", search_column = "not_a_column", strict_search = TRUE, verbose = FALSE),
    "is not valid"
  )
})

test_that("search_drug_names() strict search errors when given more than one search_column", {
  expect_error(
    search_drug_names(test_hospitalmar, "fentanyl",
                      search_column = c("generic_name", "simple_generic_name"),
                      strict_search = TRUE, verbose = FALSE),
    "single"
  )
})

test_that("search_drug_names() strict search works on the class columns", {
  cls <- search_drug_names(test_hospitalmar, "fentanyl", search_column = "simple_generic_name",
                           strict_search = TRUE, verbose = FALSE)$pharmaceutical_class[1]
  result <- search_drug_names(test_hospitalmar, cls, search_column = "pharmaceutical_class",
                              strict_search = TRUE, verbose = FALSE)
  expect_true(nrow(result) > 0)
  expect_true(all(result$pharmaceutical_class == cls))
})

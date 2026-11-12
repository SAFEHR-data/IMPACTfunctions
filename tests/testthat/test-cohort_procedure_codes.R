# Tests for cohort_procedure_codes() in R/cohort_procedure_codes.R.
#
# cohort_procedure_codes() matches OPCS codes by plain substring search
# against the concatenated `combined_coded_opcs_procedures` string (it does
# not split the string into individual codes first), so a small hand-built
# fixture is used here rather than the large synthetic dataset - it makes
# the exact matching behaviour (including the documented "a 3-digit code
# also matches its 4/5-digit extensions" substring behaviour) easy to set
# up and review, and lets us include an uncoded row on demand, which never
# occurs in the synthetic Procedures table.

opcs_fixture <- arrow::arrow_table(
  case_id = 1:6,
  primary_coded_procedure = c("F34.1", "W46.2", "A01.1", NA, "F35.3", "Z99.9"),
  combined_coded_opcs_procedures = c(
    "F34.1 | A02.1",
    "W46.2 | B10.1",
    "A01.1 | C20.2",
    NA,
    "F35.3 | D10.1",
    "Z99.9 | E10.1"
  )
)

test_that("cohort_procedure_codes() include_OPCS matches a 3-digit code against its longer extension", {
  result <- cohort_procedure_codes(opcs_fixture, include_OPCS = "F34", add_to_eligability_criteria = FALSE, verbose = FALSE) %>%
    collect()

  expect_setequal(result$case_id, 1) # "F34.1" contains "F34" as a substring; "F35.3" does not
})

test_that("cohort_procedure_codes() include_OPCS treats multiple codes as an OR match", {
  result <- cohort_procedure_codes(opcs_fixture, include_OPCS = c("F34", "W46"), add_to_eligability_criteria = FALSE, verbose = FALSE) %>%
    collect()

  expect_setequal(result$case_id, c(1, 2))
})

test_that("cohort_procedure_codes() exclude_OPCS removes matching rows and keeps the rest", {
  result <- cohort_procedure_codes(
    opcs_fixture,
    include_OPCS = NA, exclude_OPCS = "A01",
    enforce_coded = FALSE, add_to_eligability_criteria = FALSE, verbose = FALSE
  ) %>% collect()

  expect_false(3 %in% result$case_id) # "A01.1" matched and removed
  expect_true(4 %in% result$case_id) # uncoded row untouched since enforce_coded = FALSE
})

test_that("cohort_procedure_codes() enforce_coded = TRUE removes uncoded rows before matching", {
  result <- cohort_procedure_codes(
    opcs_fixture,
    include_OPCS = "Z99", enforce_coded = TRUE,
    add_to_eligability_criteria = FALSE, verbose = FALSE
  ) %>% collect()

  expect_setequal(result$case_id, 6)
})

test_that("cohort_procedure_codes() attaches cohort_name to the result (#58)", {
  result <- cohort_procedure_codes(
    opcs_fixture,
    cohort_name = "Test cohort", include_OPCS = "F34",
    add_to_eligability_criteria = FALSE, verbose = FALSE
  ) %>% collect()

  expect_true("cohort_name" %in% names(result))
  expect_equal(unique(result$cohort_name), "Test cohort")
})

test_that("cohort_procedure_codes() adds no cohort_name column when cohort_name is NA", {
  result <- cohort_procedure_codes(
    opcs_fixture, include_OPCS = "F34",
    add_to_eligability_criteria = FALSE, verbose = FALSE
  ) %>% collect()

  expect_false("cohort_name" %in% names(result))
})

test_that("cohort_procedure_codes() errors when neither include_OPCS nor exclude_OPCS is provided", {
  expect_error(
    cohort_procedure_codes(opcs_fixture, add_to_eligability_criteria = FALSE, verbose = FALSE),
    "No procedures codes provided"
  )
})

test_that("cohort_procedure_codes() errors on an invalid OPCS code format", {
  expect_error(
    cohort_procedure_codes(opcs_fixture, include_OPCS = "f34", add_to_eligability_criteria = FALSE, verbose = FALSE),
    "UPPER CASE letter"
  )
})

test_that("cohort_procedure_codes() records an Inclusion row in eligibility_criteria", {
  local_reset_eligibility_criteria()
  cohort_procedure_codes(opcs_fixture, include_OPCS = "F34", enforce_coded = FALSE, add_to_eligability_criteria = TRUE, verbose = FALSE)

  row <- tail(eligibility_criteria, 1)
  expect_equal(row$function_name, "cohort_procedure_codes()")
  expect_equal(row$type, "Inclusion")
})

test_that("cohort_procedure_codes() add_to_eligability_criteria = FALSE also applies to the uncoded-removal step (#59)", {
  local_reset_eligibility_criteria()
  cohort_procedure_codes(opcs_fixture, include_OPCS = "Z99", enforce_coded = TRUE, add_to_eligability_criteria = FALSE, verbose = FALSE)

  expect_false(exists("eligibility_criteria", envir = .GlobalEnv, inherits = FALSE))
})

test_that("cohort_procedure_codes() records the uncoded-removal step when add_to_eligability_criteria = TRUE", {
  local_reset_eligibility_criteria()
  cohort_procedure_codes(opcs_fixture, include_OPCS = "Z99", enforce_coded = TRUE, add_to_eligability_criteria = TRUE, verbose = FALSE)

  expect_equal(eligibility_criteria$function_name, c("cohort_enforce_procedure_coded()", "cohort_procedure_codes()"))
  expect_equal(eligibility_criteria$type, c("Exclusion", "Inclusion"))
})

test_that("cohort_procedure_codes() Exclusion row text lists the exclude_OPCS codes, not include_OPCS", {
  local_reset_eligibility_criteria()
  cohort_procedure_codes(
    opcs_fixture,
    include_OPCS = c("F34", "A01"), exclude_OPCS = "A01",
    enforce_coded = FALSE, add_to_eligability_criteria = TRUE, verbose = FALSE
  )

  inclusion <- eligibility_criteria[eligibility_criteria$type == "Inclusion", ]
  exclusion <- eligibility_criteria[eligibility_criteria$type == "Exclusion", ]
  expect_match(inclusion$text, "F34, A01", fixed = TRUE)
  expect_match(exclusion$text, "codes: A01.", fixed = TRUE)
  expect_no_match(exclusion$text, "F34", fixed = TRUE)
})

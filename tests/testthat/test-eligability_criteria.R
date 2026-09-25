# Tests for eligibility_criteria_check() and eligibility_criteria_add() in
# R/eligability_criteria.R.
#
# Both write to a variable named `eligibility_criteria` in the global
# environment via `<<-` (see cohort_enforce_*() tests for the same pattern),
# so every test here calls local_reset_eligibility_criteria() first (see
# helper-synthetic.R) to start and end with a clean global state.

test_that("eligibility_criteria_check() initialises an empty data frame with the expected columns and types", {
  local_reset_eligibility_criteria()
  expect_false(exists("eligibility_criteria", envir = .GlobalEnv, inherits = FALSE))

  eligibility_criteria_check()

  expect_true(exists("eligibility_criteria", envir = .GlobalEnv, inherits = FALSE))
  expect_equal(nrow(eligibility_criteria), 0)
  expect_named(eligibility_criteria, c("type", "text", "function_name", "function_filter", "n_prior", "n_filtered"))
  expect_type(eligibility_criteria$type, "character")
  expect_type(eligibility_criteria$n_prior, "integer")
  expect_type(eligibility_criteria$n_filtered, "integer")
})

test_that("eligibility_criteria_check() leaves an existing table untouched", {
  local_reset_eligibility_criteria()
  eligibility_criteria_add(type = "Exclusion", text = "t", function_name = "fn_a", function_filter = "f", n_prior = 10, n_filtered = 1)

  eligibility_criteria_check()

  expect_equal(nrow(eligibility_criteria), 1)
  expect_equal(eligibility_criteria$function_name, "fn_a")
})

test_that("eligibility_criteria_check() warns only once at least 2 duplicate function_name rows already exist", {
  local_reset_eligibility_criteria()

  # 0 rows -> no duplicates possible
  expect_no_message(eligibility_criteria_check())

  assign("eligibility_criteria", data.frame(
    type = "Exclusion", text = "t", function_name = "fn_a", function_filter = "f", n_prior = 10L, n_filtered = 1L
  ), envir = .GlobalEnv)
  # 1 row -> nothing to duplicate against yet
  expect_no_message(eligibility_criteria_check())

  assign("eligibility_criteria", rbind(eligibility_criteria, eligibility_criteria), envir = .GlobalEnv)
  # 2 identical rows -> now duplicated
  expect_message(eligibility_criteria_check(), "applied the same eligability criteria twice")
})

test_that("eligibility_criteria_add() creates the table (via eligibility_criteria_check()) if none exists", {
  local_reset_eligibility_criteria()
  expect_false(exists("eligibility_criteria", envir = .GlobalEnv, inherits = FALSE))

  eligibility_criteria_add(type = "Inclusion", text = "some text", function_name = "fn_a", function_filter = "filter code", n_prior = 100, n_filtered = 5)

  expect_equal(nrow(eligibility_criteria), 1)
  expect_equal(eligibility_criteria$type, "Inclusion")
  expect_equal(eligibility_criteria$text, "some text")
  expect_equal(eligibility_criteria$function_name, "fn_a")
  expect_equal(eligibility_criteria$function_filter, "filter code")
  expect_equal(eligibility_criteria$n_prior, 100)
  expect_equal(eligibility_criteria$n_filtered, 5)
})

test_that("eligibility_criteria_add() accumulates rows across multiple calls, in order", {
  local_reset_eligibility_criteria()

  eligibility_criteria_add(type = "Exclusion", text = "first", function_name = "fn_a", function_filter = "f", n_prior = 100, n_filtered = 10)
  eligibility_criteria_add(type = "Exclusion", text = "second", function_name = "fn_b", function_filter = "f", n_prior = 90, n_filtered = 5)

  expect_equal(nrow(eligibility_criteria), 2)
  expect_equal(eligibility_criteria$function_name, c("fn_a", "fn_b"))
  expect_equal(eligibility_criteria$text, c("first", "second"))
})

test_that("eligibility_criteria_add() with a repeated function_name only warns starting on the 3rd call", {
  # eligibility_criteria_check() (called at the top of add()) evaluates
  # duplication *before* the current row is added - so with the same
  # function_name repeated, the table only contains 1 duplicate-free row
  # after call 1, and 2 identical (now-duplicated) rows after call 2. The
  # check inside call 3 is therefore the first one to see any duplication.
  # The message says "applied ... twice", which is accurate at that point
  # (2 prior identical rows exist) but might read as one call earlier than
  # it actually fires - worth being aware of, not necessarily a bug.
  local_reset_eligibility_criteria()

  expect_no_message(eligibility_criteria_add(type = "Exclusion", text = "t", function_name = "fn_x", function_filter = "f", n_prior = 10, n_filtered = 1))
  expect_no_message(eligibility_criteria_add(type = "Exclusion", text = "t", function_name = "fn_x", function_filter = "f", n_prior = 9, n_filtered = 1))
  expect_message(
    eligibility_criteria_add(type = "Exclusion", text = "t", function_name = "fn_x", function_filter = "f", n_prior = 8, n_filtered = 1),
    "applied the same eligability criteria twice"
  )

  expect_equal(nrow(eligibility_criteria), 3)
})

# Tests for the small internal numeric helpers in R/helper_functions.R.
#
# These are all unexported helpers used inside pca_clean_data() (and friends)
# to clean/round PCA dosing values. None depend on the dataset, so they're
# called directly. Unexported functions are reachable in tests because
# testthat runs test files inside the package's own namespace.

test_that("%||% returns the left-hand side unless it is NULL", {
  expect_identical(NULL %||% "x", "x")
  expect_identical("y" %||% "x", "y")
  # NA is not NULL, so it passes through rather than falling back
  expect_identical(NA %||% "x", NA)
})

test_that("safe_floor_step() floors to the nearest step below, preserving exact boundaries", {
  expect_identical(safe_floor_step(NA, 0.5), NA_real_)
  expect_equal(safe_floor_step(1.24, 0.5), 1)
  # The +1e-9 epsilon keeps an exact multiple of step from being floored down
  expect_equal(safe_floor_step(1.5, 0.5), 1.5)
  expect_equal(safe_floor_step(0.99, 0.5), 0.5)
  expect_equal(safe_floor_step(c(1.24, NA, 1.5), 0.5), c(1, NA, 1.5))
})

test_that("floor_step_by_med() uses 10-unit steps for fentanyl and 0.5-unit steps otherwise", {
  expect_equal(floor_step_by_med(23, "fentanyl"), 20)
  expect_equal(floor_step_by_med(23, "Fentanyl"), 20) # case-insensitive
  expect_equal(floor_step_by_med(23, "morphine"), 23)
})

test_that("floor_step_by_med() treats a NULL medication as non-fentanyl via %||%", {
  expect_equal(floor_step_by_med(10, NULL), 10)
})

test_that("upper_bg_rate_by_med() returns 40 for fentanyl and 2 otherwise", {
  expect_equal(upper_bg_rate_by_med("fentanyl"), 40)
  expect_equal(upper_bg_rate_by_med("Fentanyl IV"), 40) # case-insensitive, substring match
  expect_equal(upper_bg_rate_by_med("morphine"), 2)
})

test_that("upper_bg_rate_by_med() errors on NULL, unlike floor_step_by_med()", {
  # Unlike floor_step_by_med(), this helper has no %||% guard, so grepl()
  # returns logical(0) for a NULL medication and if() errors on it.
  expect_error(upper_bg_rate_by_med(NULL), "argument is of length zero")
})

test_that("nearest_half_ml() snaps to the nearest 0.5 using round-half-to-even at exact boundaries", {
  expect_identical(nearest_half_ml(NA), NA_real_)
  expect_equal(nearest_half_ml(0.24), 0)
  expect_equal(nearest_half_ml(0.26), 0.5)
  # Exact halfway points round to even (R's default rounding), not always up
  expect_equal(nearest_half_ml(0.25), 0)
  expect_equal(nearest_half_ml(0.75), 1)
})

test_that("bg_pref_penalty() returns the distance to the nearest preferred rate (0, 0.5, 1.0)", {
  expect_equal(bg_pref_penalty(0.25), 0.25)
  expect_equal(bg_pref_penalty(0.5), 0)
  expect_equal(bg_pref_penalty(2), 1)
  expect_equal(bg_pref_penalty(NA), Inf)
})

test_that("has_half_mg_evidence() requires at least 2 valid ~0.5 ratios and a >=50% share of valid entries", {
  expect_true(has_half_mg_evidence(c(1, 1, 1), c(0.5, 0.5, 0.5)))
  expect_false(has_half_mg_evidence(c(1, 1), c(0.5, 10))) # only 1 of 2 in range
  expect_false(has_half_mg_evidence(c(1), c(0.5))) # only 1 valid pair, needs >= 2
  # Non-finite/non-positive entries are dropped before evaluating
  expect_true(has_half_mg_evidence(c(1, NA, 1), c(0.5, 0.5, 0.5)))
  # Boundary ratios of 0.45 and 0.55 are inclusive
  expect_true(has_half_mg_evidence(c(2, 2), c(0.9, 1.1)))
})

test_that("sum_distinct() collapses duplicate charting artifacts before summing", {
  # All three entries are the same value - treated as one duplicated
  # chart entry, not summed three times
  expect_equal(sum_distinct(c(5, 5, 5)), 5)
  expect_equal(sum_distinct(c(5, 10)), 15)
  # One duplicate pair (5) plus a distinct value (10) - the duplicate
  # collapses to a single 5, which is then summed with 10
  expect_equal(sum_distinct(c(5, 5, 10)), 15)
})

test_that("sum_distinct() drops non-finite values and returns NA for an empty/all-non-finite input", {
  expect_equal(sum_distinct(c(NA, 5, Inf)), 5)
  expect_identical(sum_distinct(c(NA, Inf)), NA_real_)
})

test_that("sum_distinct() treats values within its rounding tolerance as duplicates", {
  expect_equal(sum_distinct(c(5.0000001, 5.0000002)), 5)
})

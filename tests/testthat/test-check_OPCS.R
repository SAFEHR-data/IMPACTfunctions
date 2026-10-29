# Tests for check_OPCS() in R/check_OPCS.R.
#
# check_OPCS() is a pure validation function (no dataset dependency), so
# these tests exercise it directly rather than via the synthetic fixtures.
# Valid codes return TRUE, NA returns NA, and every invalid shape aborts
# (via cli::cli_abort()) rather than returning FALSE - the FALSE returns in
# the source are unreachable dead code after cli_abort().

test_that("check_OPCS() accepts a valid 3-character short code", {
  expect_true(check_OPCS("F34"))
  expect_true(check_OPCS("S12"))
})

test_that("check_OPCS() accepts a valid 5-character full code", {
  expect_true(check_OPCS("A01.3"))
  expect_true(check_OPCS("F34.1"))
})

test_that("check_OPCS() returns NA for NA input", {
  expect_identical(check_OPCS(NA), NA)
})

test_that("check_OPCS() errors on a code longer than 5 characters", {
  expect_error(check_OPCS("Z12.23"), "too long")
  expect_error(check_OPCS("A12, A13, A14"), "too long")
})

test_that("check_OPCS() errors when the code doesn't start with an upper-case letter", {
  expect_error(check_OPCS("12.3"), "UPPER CASE letter")
  expect_error(check_OPCS("f34"), "UPPER CASE letter")
})

test_that("check_OPCS() errors when characters 2-3 aren't a two-digit number", {
  expect_error(check_OPCS("A3.2"), "must be numbers")
  expect_error(check_OPCS("A1"), "must be numbers")
})

test_that("check_OPCS() errors when the code is a valid shape but the wrong length", {
  expect_error(check_OPCS("F340"), "3 .*short code.* or 5")
})

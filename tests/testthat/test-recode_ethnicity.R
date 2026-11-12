# Tests for recode_ethnicity() in R/recode_ethnicity.R.
#
# A pure vector-recoding function (no dataset dependency), so these tests
# call it directly rather than via the synthetic fixtures.

test_that("recode_ethnicity() maps every known EPIC value to its ONS 5-group category", {
  known_inputs <- c(
    "White British", "Other White Background", "White Irish",
    "Asian Bangladeshi", "Other Asian Background", "Asian Pakistani", "Asian Indian", "Chinese",
    "Black African", "Other Black Background", "Black Caribbean", "Black British",
    "Other Mixed Background", "Mixed White and Asian", "Mixed White and Black African", "Mixed White and Black Caribbean",
    "Other Ethnic Group",
    "Not Yet Asked", "Refused to Give", "Not Stated / Unknown"
  )
  expected <- c(
    rep("White", 3),
    rep("Asian, Asian British or Asian Welsh", 5),
    rep("Black, Black British, Black Welsh, Caribbean or African", 4),
    rep("Mixed or Multiple ethnic groups", 4),
    "Other ethnic group",
    rep(NA_character_, 3)
  )

  expect_identical(recode_ethnicity(known_inputs), expected)
})

test_that("recode_ethnicity() passes true NA values through as NA", {
  expect_identical(recode_ethnicity(c(NA, "White British")), c(NA_character_, "White"))
})

test_that("recode_ethnicity() errors on an unrecognised ethnicity string", {
  expect_error(recode_ethnicity("Klingon"), "Unmapped ethnicity")
})

test_that("recode_ethnicity() currently errors on an empty string, despite case_when() mapping it to NA", {
  # known_inputs (used for the validation step) does not include "", but the
  # case_when() mapping further down explicitly lists "" as an NA-mapped
  # value. The validation step runs first and rejects "" as "unmapped"
  # before the mapping logic that would have handled it ever runs. This
  # looks like an inconsistency between the two lists rather than intended
  # behaviour - documenting the current (erroring) behaviour here so this
  # test fails loudly and visibly if/when that's fixed, as a prompt to
  # update it rather than leaving it silently wrong.
  expect_error(recode_ethnicity(""), "Unmapped ethnicity")
})

test_that("recode_ethnicity() with short_names = TRUE returns an ordered factor of short labels", {
  result <- recode_ethnicity(c("White British", "Asian Indian", "Black African", "Mixed White and Asian", "Other Ethnic Group"), short_names = TRUE)

  expect_s3_class(result, "factor")
  expect_identical(as.character(result), c("White", "Asian", "Black", "Mixed", "Other"))
})

test_that("recode_ethnicity() with short_names = TRUE keeps NA-mapped values as NA", {
  result <- recode_ethnicity(c("Not Yet Asked", "White British"), short_names = TRUE)

  expect_true(is.na(result[1]))
  expect_identical(as.character(result[2]), "White")
})

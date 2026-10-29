# Tests for preproccess_MAR() in R/preproccess_MAR.R.
#
# preproccess_MAR() requires its `MAR` argument to be identical() to a
# global object named `HospitalMar` (see R/preproccess_MAR.R's
# `if(identical(MAR, HospitalMar))` check), so every test here calls
# local_set_global_hospitalmar() first (see helper-synthetic.R) to make that
# hold for whatever fixture is being passed in.

test_that("preproccess_MAR() joins the route/opioid/equivalence dictionaries and computes dose_mg and ome", {
  local_set_global_hospitalmar(test_hospitalmar)
  result <- preproccess_MAR(test_hospitalmar) %>% collect()

  expect_true(all(c("route", "route_simple", "opioid_name", "equivelence", "dose_mg", "ome") %in% names(result)))

  # Oracle: independently replicate the dictionary joins, the dose unit
  # standardisation (dose_mg = dose * multiplication_factor) and the OME formula
  # (ome = dose_mg * equivelence) against the raw (pre-administered_only-filter)
  # synthetic MAR. Unit conversion factors are pinned to the dictionary.
  factors <- dictionary_dose_units %>% select(dose_units, multiplication_factor)
  expect_equal(factors$multiplication_factor[factors$dose_units == "mg"], 1)

  expected <- test_hospitalmar %>%
    collect() %>%
    left_join(dictionary_medication_route, by = "administration_route") %>%
    left_join(dictionary_opioid, by = "simple_generic_name") %>%
    left_join(dictionary_equivelence, by = c("opioid_name", "route_simple")) %>%
    left_join(factors, by = "dose_units") %>%
    mutate(
      dose_mg = dose * multiplication_factor,
      ome = if_else(!is.na(equivelence) & !is.na(dose_mg), dose_mg * equivelence, NA_real_)
    ) %>%
    filter(administration_action %in% c("Given", "Dose Documentation"))

  expect_equal(nrow(result), nrow(expected))
  expect_equal(sum(!is.na(result$dose_mg)), sum(!is.na(expected$dose_mg)))
  expect_equal(sum(!is.na(result$ome)), sum(!is.na(expected$ome)))
  expect_equal(sum(result$ome, na.rm = TRUE), sum(expected$ome, na.rm = TRUE))
})

test_that("preproccess_MAR() standardise_units = FALSE creates no dose_mg and only computes ome for mg", {
  local_set_global_hospitalmar(test_hospitalmar)
  result <- suppressMessages(preproccess_MAR(test_hospitalmar, standardise_units = FALSE)) %>% collect()

  expect_false("dose_mg" %in% names(result))
  expect_true(all(is.na(result$ome[result$dose_units != "mg"])))
  expect_equal(
    result$ome[result$dose_units == "mg" & !is.na(result$equivelence)],
    result$dose[result$dose_units == "mg" & !is.na(result$equivelence)] * result$equivelence[result$dose_units == "mg" & !is.na(result$equivelence)]
  )
})

test_that("preproccess_MAR() sets dose_mg to NA and warns for a dose_units that cannot be standardised", {
  local_set_global_hospitalmar(test_hospitalmar)

  expect_message(preproccess_MAR(test_hospitalmar), "mg/ml")
  result <- suppressMessages(preproccess_MAR(test_hospitalmar)) %>% collect()
  expect_true(all(is.na(result$dose_mg[result$dose_units == "mg/ml"])))
  expect_true(all(!is.na(result$dose_mg[result$dose_units == "mg"])))
})

test_that("preproccess_MAR() sets ome to NA for a dose_units it doesn't recognise (e.g. \"mg/ml\")", {
  local_set_global_hospitalmar(test_hospitalmar)
  result <- preproccess_MAR(test_hospitalmar) %>% collect()

  expect_true(all(is.na(result$ome[result$dose_units == "mg/ml"])))
})

test_that("preproccess_MAR() administered_only = TRUE (default) keeps only administration_action == \"Given\"", {
  # The docs say this keeps "Given" or "Dose Documentation" rows, but the
  # filter's exact string "Dose Documentation" never matches the synthetic
  # generator's "Dose documented" (different wording/casing) - worth
  # checking against real data whether that's a synthetic-data mismatch or a
  # genuine string bug in the filter; documenting current behaviour either
  # way rather than assuming which is correct. See the plan document.
  local_set_global_hospitalmar(test_hospitalmar)
  result <- preproccess_MAR(test_hospitalmar) %>% collect()

  expect_setequal(unique(result$administration_action), "Given")
})

test_that("preproccess_MAR() administered_only = FALSE keeps every administration_action value", {
  local_set_global_hospitalmar(test_hospitalmar)
  result <- preproccess_MAR(test_hospitalmar, administered_only = FALSE) %>% collect()
  raw <- test_hospitalmar %>% collect()

  expect_setequal(result$administration_action, raw$administration_action)
})

test_that("preproccess_MAR() errors if a custom dict's column names don't match the built-in dict", {
  local_set_global_hospitalmar(test_hospitalmar)
  bad_route_dict <- dictionary_medication_route
  names(bad_route_dict)[1] <- "wrong_column"

  expect_error(
    preproccess_MAR(test_hospitalmar, route_dict = bad_route_dict),
    "column names don't"
  )
})

test_that("preproccess_MAR() errors when MAR is not identical to the global HospitalMar", {
  local_set_global_hospitalmar(test_hospitalmar)
  different_mar <- test_hospitalmar %>% filter(mrn == "definitely-not-a-real-mrn")

  expect_error(preproccess_MAR(different_mar), "not identical to the base MARs")
})

test_that("preproccess_MAR() errors on a duplicated mar_id", {
  fixture <- arrow::arrow_table(
    mar_id = c(1, 1),
    simple_generic_name = c("fentanyl", "fentanyl"),
    administration_route = c("Intravenous", "Intravenous"),
    dose = c(10, 10),
    dose_units = c("mg", "mg"),
    administration_action = c("Given", "Given")
  )
  local_set_global_hospitalmar(fixture)

  expect_error(preproccess_MAR(fixture), "duplication of the .mar_id. column")
})

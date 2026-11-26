# IMPACTfunctions 0.2.0.9000

* Development version.

# IMPACTfunctions 0.2.0

First tagged release. CI runs again, there is a test suite, and the
cohorting bugs listed in the v0.2.0 milestone are fixed.

## Breaking changes

* **Opioid equivalence (OME) values change.** The OME formula divided the dose
  by the opioid equivalence factor instead of multiplying by it, and `sum_ome`
  in `calculate_medication_feature()` summed the raw `dose` column instead of
  `ome`. Both are fixed (#62), so OME results will differ from earlier versions.

## Medication data

* `preproccess_MAR()` gains `standardise_units` (default `TRUE`), which converts
  `dose` into a common `dose_mg` column using the new `dictionary_dose_units`
  dictionary. Units that cannot be safely converted are left as `NA` and listed
  in a warning (#62).
* `calculate_medication_feature()` checks dose units before summing: silent when
  everything is in mg, a warning for a single non-mg unit, and an error for mixed
  units.
* `filter_medication()` and `filter_flowsheet_data()` replace
  `extract_medication()` and `extract_flowsheet_data()`. The old names still work
  but are deprecated.
* New PCA functions: `fetch_pca_data()`, `proccess_pca_data()` and the
  interactive `pca_diagnostics()` dashboard (#52). The `dictionary_flowsheet_pca`
  dataset is now documented.

## Cohorting fixes

* `cohort_enforce_single_procedure()` no longer errors on Arrow tables
  ("Slicing grouped data not supported in Arrow") (#60).
* `cohort_procedure_codes()`:
  * attaches `cohort_name` to the result; previously the argument was silently
    ignored (#58).
  * honours `add_to_eligability_criteria = FALSE` in the uncoded-removal step as
    well (#59).
  * the Exclusion row in `eligibility_criteria` now lists the `exclude_OPCS`
    codes instead of the `include_OPCS` codes.

## Infrastructure

* GitHub Actions work again (#43, #9). The repository only allows local
  actions, so the workflows now run in `rocker/r-ver` containers using plain
  shell and R steps: R CMD check on R release and R 4.4, a version-increment
  check on PRs into `main`, and the pkgdown site deploy to Netlify.
* R CMD check is clean of warnings, and CI fails on any new warning.
* New testthat suite covering the cohorting, eligibility, medication, flowsheet,
  PCA helper and search functions, using the synthetic dataset.
* `zoo` is now listed in `Imports`.
* Added a pkgdown website for the package, built and deployed automatically via
  GitHub Actions.

## Loading a dataset

* `load_dataset()` now **returns** the dataset as an `impact_dataset` object, in
  the same way that `read_csv()` returns a tibble, rather than only writing the
  tables into the global environment as a side effect:

  ```r
  impact <- load_dataset("~/data/IMPACT/full_extract", prefix = "SCB_Periop_")
  impact$Procedures
  ```

  An `impact_dataset` is a named list of arrow bindings that also records where
  the dataset was read from and when. `names()`, `$`, `[[` and `[` all work as
  they would on a list, and `print()` gives a summary of the tables and their
  dimensions.

* **This is backwards compatible.** `load_dataset()` still attaches the tables
  to your environment by name, because most functions in this package default to
  bare table names (for example `extract_medication(MAR = HospitalMar)`).
  Existing scripts do not need to change.

* New `attach_dataset()` makes that step explicit and optional. Use
  `load_dataset(..., attach = FALSE)` to leave your environment untouched, and
  `attach_dataset(impact)` to attach the tables later. `attach_dataset()` warns
  when it overwrites an existing object rather than doing so silently, and takes
  an `envir` argument so it need not target the global environment.

* `load_synthetic_dataset()` now returns an `impact_dataset` too, so real and
  synthetic data are handled the same way. `list2env(load_synthetic_dataset(),
  envir = .GlobalEnv)` continues to work, but `attach_dataset()` is preferred.

* New `is_impact_dataset()`.

## Bug fixes

* `load_dataset()` no longer errors when it encounters an extract containing the
  `anesthesia_csn` spelling. The column-renaming branch referred to `names[i]`,
  which resolves to the base function `names`, so the load aborted with
  `object of type 'builtin' is not subsettable` instead of repairing the name.
  It now renames the column as intended, and no longer relies on a tidyselect
  behaviour deprecated since tidyselect 1.1.0.

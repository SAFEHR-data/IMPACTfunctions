# IMPACTfunctions (development version)

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

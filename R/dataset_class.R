#' The `impact_dataset` object
#'
#' An `impact_dataset` is the value returned by [load_dataset()] and
#' [load_synthetic_dataset()]. It is a named list of arrow bindings, one per
#' table in the dataset, carrying a small amount of metadata about where the
#' dataset came from and when it was read.
#'
#' @details
#' Because the object is a list underneath, the usual list accessors work as you
#' would expect:
#'
#' ```r
#' impact <- load_dataset("~/data/IMPACT/full_extract", prefix = "SCB_Periop_")
#'
#' names(impact)             # the table names
#' impact$Procedures         # a single arrow binding
#' impact[["HospitalMar"]]   # the same, by name
#' impact[c("Procedures", "Demographics")]   # a smaller impact_dataset
#' ```
#'
#' To put the tables into your environment as free-standing objects - which is
#' what many of the package's functions expect by default - use
#' [attach_dataset()].
#'
#' @name impact_dataset
#' @seealso [load_dataset()], [attach_dataset()]
NULL

# Is `x` something we can treat as a table? Arrow bindings (`Table`, `Dataset`,
# `RecordBatch`, ...) all inherit from `ArrowObject`; a partially built query
# (the result of piping a Dataset through dplyr verbs) does not, so it is
# checked separately.
is_arrow_tabular <- function(x) {
  inherits(x, "ArrowObject") || inherits(x, "arrow_dplyr_query")
}

#' Construct an `impact_dataset`
#'
#' The internal constructor. Validates `tables` and attaches the dataset
#' metadata. Use [load_dataset()] rather than calling this directly.
#'
#' @param tables A named list of arrow bindings, one per table.
#' @param source Where the dataset was read from, for display purposes.
#' @param loaded_at The time the dataset was read.
#' @param package_version The version of IMPACTfunctions that read it.
#'
#' @return An object of class `impact_dataset`.
#' @keywords internal
#' @noRd
new_impact_dataset <- function(tables,
                               source = NA_character_,
                               loaded_at = Sys.time(),
                               package_version = utils::packageVersion("IMPACTfunctions")) {

  validate_impact_dataset_tables(tables)

  structure(
    tables,
    source          = as.character(source),
    loaded_at       = loaded_at,
    package_version = as.character(package_version),
    class           = "impact_dataset"
  )
}

# Checks that `tables` is shaped like a dataset before we class it. Kept
# separate from the constructor so the error messages can be tested directly.
validate_impact_dataset_tables <- function(tables) {

  if (!is.list(tables)) {
    cli::cli_abort("An {.cls impact_dataset} must be built from a {.cls list} of tables, not a {.cls {class(tables)[1]}}.")
  }

  table_names <- names(tables)

  if (length(tables) > 0) {

    if (is.null(table_names) || any(is.na(table_names)) || any(table_names == "")) {
      cli::cli_abort("Every table in an {.cls impact_dataset} must be named.")
    }

    if (any(duplicated(table_names))) {
      cli::cli_abort("Table names in an {.cls impact_dataset} must be unique. Duplicated: {.val {unique(table_names[duplicated(table_names)])}}.")
    }

    not_tabular <- !vapply(tables, is_arrow_tabular, logical(1))
    if (any(not_tabular)) {
      cli::cli_abort("Every table in an {.cls impact_dataset} must be an arrow binding. {.val {table_names[not_tabular]}} {?is/are} not.")
    }
  }

  invisible(tables)
}

#' Is this an `impact_dataset`?
#'
#' @param x An object to test.
#'
#' @return `TRUE` if `x` is an [impact_dataset], otherwise `FALSE`.
#' @export
#'
#' @examples
#' is_impact_dataset(load_synthetic_dataset())
#' is_impact_dataset(mtcars)
is_impact_dataset <- function(x) {
  inherits(x, "impact_dataset")
}

#' @export
print.impact_dataset <- function(x, ...) {

  source <- attr(x, "source")
  loaded_at <- attr(x, "loaded_at")

  cli::cli_h1("IMPACT dataset")

  if (length(source) == 1 && !is.na(source)) {
    cli::cli_text("{.strong Source:} {.path {source}}")
  }
  if (!is.null(loaded_at)) {
    cli::cli_text("{.strong Loaded:} {format(loaded_at, '%Y-%m-%d %H:%M:%S')}")
  }

  if (length(x) == 0) {
    cli::cli_text("{.strong Tables:} none")
    return(invisible(x))
  }

  cli::cli_text("{.strong Tables:} {.val {length(x)}}")

  # nrow() on a Dataset reads parquet metadata rather than the data itself, but
  # it can still fail on a malformed or unreadable source - never let printing
  # the object be the thing that errors.
  dims <- vapply(x, function(tbl) {
    tryCatch(
      sprintf("%s rows x %s cols", format(nrow(tbl), big.mark = ","), ncol(tbl)),
      error = function(e) "dimensions unavailable"
    )
  }, character(1))

  cli::cli_ul(paste0("{.field ", names(x), "} - ", dims))

  invisible(x)
}

#' @export
`[.impact_dataset` <- function(x, i, ...) {
  # Subsetting should give back a smaller dataset, not a bare list, so the
  # metadata and class have to be carried over by hand.
  out <- NextMethod()
  attr(out, "source")          <- attr(x, "source")
  attr(out, "loaded_at")       <- attr(x, "loaded_at")
  attr(out, "package_version") <- attr(x, "package_version")
  class(out) <- "impact_dataset"
  out
}

#' Attach the tables of a dataset to an environment
#'
#' Takes an [impact_dataset] and binds each of its tables as a free-standing
#' object, so that `impact$Procedures` becomes simply `Procedures`.
#'
#' @details
#' Many functions in this package take a table argument that defaults to a bare
#' name - for example `extract_medication(MAR = HospitalMar)` and
#' `cohort_procedure_codes(procedures_table = Procedures)`. Those defaults only
#' resolve if the tables exist as objects in your session, which is what this
#' function arranges. [load_dataset()] calls it for you unless you ask it not
#' to with `attach = FALSE`.
#'
#' Attaching is a side effect: it creates objects in `envir`, overwriting
#' anything already bound to those names. The names it would overwrite are
#' reported so that the clash is never silent.
#'
#' @param x An [impact_dataset], as returned by [load_dataset()] or
#'   [load_synthetic_dataset()].
#' @param envir The environment to attach the tables to. Defaults to the global
#'   environment, which is where an interactive session and most scripts look
#'   for them.
#' @param overwrite If `TRUE` (the default), existing objects of the same name
#'   are replaced and a warning names them. If `FALSE`, a clash is an error and
#'   nothing is attached.
#'
#' @return The attached table names, invisibly.
#' @export
#'
#' @examples
#' # Into a temporary environment, to leave the global environment alone
#' env <- new.env()
#' attach_dataset(load_synthetic_dataset(), envir = env)
#' ls(env)
#'
#' \dontrun{
#' # The usual interactive use: tables become available by name
#' impact <- load_dataset("~/data/IMPACT/full_extract",
#'                        prefix = "SCB_Periop_",
#'                        attach = FALSE)
#' attach_dataset(impact)
#' Procedures
#' }
attach_dataset <- function(x, envir = globalenv(), overwrite = TRUE) {

  if (!is_impact_dataset(x)) {
    cli::cli_abort("{.arg x} must be an {.cls impact_dataset}, not a {.cls {class(x)[1]}}.")
  }

  if (!is.environment(envir)) {
    cli::cli_abort("{.arg envir} must be an {.cls environment}, not a {.cls {class(envir)[1]}}.")
  }

  table_names <- names(x)

  if (length(table_names) == 0) {
    cli::cli_alert_warning("This dataset has no tables, so nothing was attached.")
    return(invisible(character()))
  }

  clashes <- table_names[vapply(table_names, exists, logical(1),
                                envir = envir, inherits = FALSE)]

  if (length(clashes) > 0) {
    if (overwrite) {
      cli::cli_alert_warning("Overwriting {length(clashes)} existing object{?s}: {.val {clashes}}.")
    } else {
      cli::cli_abort(c(
        "{length(clashes)} object{?s} of the same name already exist{?s/}: {.val {clashes}}.",
        "i" = "Use {.code overwrite = TRUE} to replace {?it/them}."
      ))
    }
  }

  list2env(unclass(x), envir = envir)

  invisible(table_names)
}

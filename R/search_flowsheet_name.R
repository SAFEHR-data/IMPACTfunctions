#' Helper function to search the flowsheet
#'
#' Flowsheet question names can be inconsistent or have multiple representations,
#' and a single question name can map to more than one Epic flowsheet row ID.
#' Use this function to understand what your search string captures before passing
#' arguments to `extract_flowsheet_data()`.
#'
#' In dataset version 20261112 on wards the flowsheets are split into `Measurements` (generally flowsheets manually entered) and `DeviceMeasurements` (Flowsheets automatically populated by monitors/anaesthetic machines. )
#'
#' @param string String (or vector of strings) to search for. Case insensitive for the soft search.
#' @param flowsheet The arrow table of the flowsheet of interest. Defaults to `Measurements` but use `DeviceMeasurements` for machine collected flowsheet values.
#' @param strict_search Do exact string and column matching. Useful for testing arguments for `extract_flowsheet_data()`.
#' @param search_column Defaults to both columns to do a soft search (return rows where there is a match in either). Specify one of `"flowsheet_question_name"` or `"flowsheet_row_epic_id"` to do a hard search only in that column. Note this is *only* enforced when `strict_search = TRUE`; for a soft search `search_column` is informational only and matches from both columns are still returned (so a term that happens to match the other column is still surfaced to help you debug your search) - if the term doesn't match within the `search_column` you named, a warning tells you so before showing the broader results.
#' @param verbose Print output.
#'
#' @return A dataframe summarising the flowsheet rows (one row per `flowsheet_question_name` / `flowsheet_row_epic_id` pair) matching the search criteria and their frequency.
#' @export
#' @template setup-data
#'
#' @examples
#' search_flowsheet_name("pulse", Measurements)
#'
#' # Epic flowsheet row IDs can be searched too
#' search_flowsheet_name("50000003", Measurements)
#'
#' # Machine collected values live in DeviceMeasurements (dataset 20261112 onwards)
#' # search_flowsheet_name("heart rate", DeviceMeasurements)
#'
#' search_flowsheet_name("pulse", Measurements,
#'    search_column = "flowsheet_question_name")
#'
#' # We can use the strict search to emulate extract_flowsheet_data()
#' search_flowsheet_name("Pulse", Measurements,
#'    search_column = "flowsheet_question_name",
#'    strict_search = TRUE)
#'
#' search_flowsheet_name("50000003", Measurements,
#'    search_column = "flowsheet_row_epic_id",
#'    strict_search = TRUE)
search_flowsheet_name <- function(
    string,
    flowsheet = Measurements,
    strict_search = FALSE,
    search_column = c("flowsheet_question_name", "flowsheet_row_epic_id"),
    verbose = TRUE
) {

  if (verbose) {
    cli::cli_h1("Starting flowsheet search tool")
    cli::cli_inform("Use this function to identify correct flowsheet names for use in {.fun extract_flowsheet_data}")
  }

  possible_col <- c("flowsheet_question_name", "flowsheet_row_epic_id")

  if (strict_search && missing(search_column)) {
    cli::cli_abort("Strict search requires a {.arg search_column}. Provide one of: {.val {possible_col}}")
  }

  if (!all(search_column %in% possible_col)) {
    cli::cli_abort("{.arg search_column} = {.val {search_column}} is not valid. Use one of: {.val {possible_col}}")
  }

  if (strict_search && length(search_column) != 1) {
    cli::cli_abort("Strict search requires a single {.arg search_column}. Provide one of: {.val {possible_col}}")
  }

  # Generate the summary for both columns
  summary <- flowsheet %>%
    group_by(flowsheet_question_name, flowsheet_row_epic_id) %>%
    summarise(rows = n(), .groups = "drop") %>%
    collect()

  if (!strict_search) {
    # If multiple strings attached then concat with bar for OR statement in grepl
    if (length(string) > 1) {
      string <- paste0(string, collapse = "|")
    }

    # Soft search always looks across both columns regardless of search_column.
    # If the caller did specify a search_column, let them know when their term
    # didn't actually match within it, so they're not misled about which column
    # their result came from.
    match_any <- grepl(string, summary$flowsheet_question_name, ignore.case = TRUE) |
      grepl(string, summary$flowsheet_row_epic_id, ignore.case = TRUE)

    if (!missing(search_column) && any(match_any)) {
      matched_in_search_column <- any(vapply(search_column, function(col) {
        any(grepl(string, summary[[col]], ignore.case = TRUE))
      }, logical(1)))

      if (!matched_in_search_column) {
        cli::cli_alert_warning("No match for {.val {string}} found in the specified {.arg search_column} ({.val {search_column}}). Soft search still returns matches from all columns below - set {.arg strict_search = TRUE} to enforce searching only within {.arg search_column}.")
      }
    }

    summary <- summary[match_any, ]
  } else {
    # Strict search
    summary <- summary %>%
      filter(.data[[search_column]] %in% string)
  }

  if (nrow(summary) == 0) {
    cli::cli_alert_danger("No rows returned, try different search settings")
  }

  return(summary)
}

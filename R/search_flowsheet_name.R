#' Helper function to search the flowsheet
#'
#' Flowsheet question names can be inconsistent or have multiple representations.
#' Use this function to understand what your search string captures before passing
#' arguments to `extract_flowsheet_data()`.
#'
#' In dataset version 20261112 on wards the flowsheets are split into `Measurements` (generally flowsheets manually entered) and `DeviceMeasurements` (Flowsheets automatically populated by monitors/anaesthetic machines. )
#'
#' @param flowsheet The arrow table of the flowsheet of interest. Defaults to `Measurements` but use `DeviceMeasurements` for machine collected flowsheet values.
#' @param string String to search for. Case insensitive for soft search.
#' @param strict_search Do exact string matching. Useful for testing arguments for `extract_flowsheet_data()`.
#' @param search_column Defaults to all columns for a soft search. Specify one of `"flowsheet_question_name"` or `"display_name"` for a hard search.
#' @param verbose Print output.
#'
#' @return A dataframe summarising the flowsheet rows matching the search criteria and their frequency.
#' @export
#' @template setup-data
#'
#' @examples
#' search_flowsheet_name("heart rate", Measurements)
#'
#' search_flowsheet_name("heart rate", DeviceMeasurements
#'    search_column = "flowsheet_question_name")
#'
#' search_flowsheet_name("heart rate", Measurements
#'    search_column = "flowsheet_question_name",
#'     strict_search = TRUE)
search_flowsheet_name <- function(
    string,
    flowsheet = Measurements,
    strict_search = FALSE,
    search_column = "flowsheet_question_name",
    verbose = TRUE
) {

  if (verbose) {
    cli::cli_h1("Starting flowsheet search tool")
    cli::cli_inform("Use this function to identify correct flowsheet names for use in {.fun extract_flowsheet_data}")
  }

  if (strict_search & missing(search_column)) {
    possible_col <- c("flowsheet_question_name","flowsheet_row_epic_id", "flowsheet_id")
    cli::cli_abort("Strict search requires a {.arg search_column}. Provide one of: {.val {possible_col}}")
  }

  summary <- flowsheet %>%
    group_by(flowsheet_question_name, flowsheet_row_epic_id) %>%
    summarise(rows = n(), .groups = "drop") %>%
    collect()

  if (!strict_search) {
    if (length(string) > 1) {
      string <- paste0(string, collapse = "|")
    }

    if(search_column %in% c("flowsheet_question_name")) {
      summary <- summary %>%
        filter(
          grepl(string, flowsheet_question_name, ignore.case = TRUE)
        )
    } else {
      summary <- summary %>%
        filter(grepl(string, .data[[search_column]], ignore.case = TRUE))
    }
  } else {
    summary <- summary %>%
      filter(.data[[search_column]] %in% string)
  }

  if (nrow(summary) == 0) {
    cli::cli_alert_danger("No rows returned, try different search settings")
  }

  return(summary)
}

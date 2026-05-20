#' Calculate features across flowsheet data
#'
#' For more details see the help vignette:
#' \code{vignette("extracting_features", package = "IMPACTfunctions")}
#'
#' @param dataset The dataset of patients of interest to return the feature against.
#' @param flowsheet The subsetted flowsheet from the output of `extract_flowsheet_data()`.
#' @param format The format to return: **"joined"** or **"table"**.
#' @param feature The feature to extract: **"binary"**, **"sum_values"**, **"mean_values"**, **"count_of_values"**, **"list_values"**.
#' @param feature_name The name for the returned feature column.
#' @param link_column The primary key to return against. Select from: `"admission_csn"`, `"anaesthesia_csn"`, `"operation_csn"`.
#' @param override_flowsheet_check The function checks all flowsheets are the same before summing or averaging. Set to `TRUE` to override if you intent to calculate across multiple flowsheets.
#' @param verbose Print the outputs of the function explicitly.
#'
#' @return Either a table of the feature of interest or the dataset left joined with the feature.
#' @template setup-data
#' @export
#' @examples
#'
#' dataset <- cohort_complete_admissions(Procedures) %>%
#'   cohort_procedure_codes(cohort_name = "Hip Arthroplasty", include_OPCS = c("W46", "W47", "W48"))
#'
#' heart_rate <- extract_flowsheet_data(dataset,
#'   flowsheet = Measurements,
#'   flowsheet_question_name = "Pulse",
#'   timeframe = "post_op")
#'
#' dataset <- calculate_flowsheet_feature(dataset,
#'   flowsheet = heart_rate,
#'   format = "joined",
#'   feature = "mean_values",
#'   feature_name = "mean_post_op_hr")
#'
#' hr_table <- calculate_flowsheet_feature(dataset,
#'   flowsheet = heart_rate,
#'   format = "table",
#'   feature = "sum_values",
#'   feature_name = "post_op_hr_values")
#'
calculate_flowsheet_feature <- function(dataset,
                                        flowsheet,
                                        format,
                                        feature,
                                        feature_name,
                                        link_column = "admission_csn",
                                        override_flowsheet_check = FALSE,
                                        verbose = TRUE) {

  # Link column validation

  if (link_column == "admission_csn") {
    csn_vector_name <- "admission_csn"
    if (any(duplicated(dataset %>% pull(admission_csn)))) {
      cli::cli_abort("Duplication error in {.val admission_csn}. \n\n Change {.arg link_column} or de-duplicate {.val dataset}.")
    }
  } else if (link_column == "anaesthesia_csn") {
    csn_vector_name <- "anaesthesia_csn"
    if (any(duplicated(dataset %>% pull(anaesthesia_csn)))) {
      cli::cli_abort("Duplication error in {.val anaesthesia_csn}. \n\n Change {.arg link_column} or de-duplicate {.val dataset}.")
    }
  } else if (link_column == "operation_csn") {
    csn_vector_name <- "operation_csn"
    if (any(duplicated(dataset %>% pull(operation_csn)))) {
      cli::cli_abort("Duplication error in {.val operation_csn}. \n\n Change {.arg link_column} or de-duplicate {.val dataset}.")
    }
  } else {
    cli::cli_abort("{.arg link_column} must be one of {.val admission_csn}, {.val anaesthesia_csn}, or {.val operation_csn}.")
  }

  # Flowsheet consistency check (skip for binary, count, list — units irrelevant)

    flowsheets_present <- unique(flowsheet %>% select(flowsheet_row_epic_id) %>% collect())

    if (length(flowsheets_present) == 1) {
      if (verbose) cli::cli_alert_success("Single flowsheet extracted. EPIC ID: {.val {flowsheets_present}}.")
    } else if (override_flowsheet_check) {
      if (verbose) cli::cli_alert_warning("Flowsheet check overridden. Proceeding with {length(flowsheets_present)} EPIC IDs: {.val {flowsheets_present}}.")
    } else {
      cli::cli_abort("Multiple flowsheet IDs detected: {.val {flowsheets_present}}. \n\n Set {.arg override_flowsheet_check} = {.val TRUE} to proceed, or restrict to a single flowsheet name in {.fn extract_flowsheet_data}.")
    }


  csn_vector <- dataset %>% pull(.data[[csn_vector_name]]) %>% na.omit()

  # Feature calculation

  if (feature == "binary") {

    flowsheet_df <- flowsheet %>% collect()
    result <- data.frame(csn_vector) %>%
      mutate(match_status = csn_vector %in% flowsheet_df[[csn_vector_name]])

  } else if (feature == "sum_values") {

    result <- flowsheet %>%
      group_by(.data[[csn_vector_name]]) %>%
      summarise(result = sum(value, na.rm = TRUE)) %>%
      collect()

  } else if (feature == "mean_values") {

    result <- flowsheet %>%
      group_by(.data[[csn_vector_name]]) %>%
      summarise(result = mean(value, na.rm = TRUE)) %>%
      collect()

  } else if (feature == "count_of_values") {

    result <- flowsheet %>%
      group_by(.data[[csn_vector_name]]) %>%
      summarise(result = n()) %>%
      collect()

  } else if (feature == "list_values") {

    result <- flowsheet %>%
      collect() %>%
      group_by(.data[[csn_vector_name]]) %>%
      summarise(result = list(value), .groups = "drop")

  } else {
    cli::cli_abort("{.arg feature} must be one of {.val binary}, {.val sum_values}, {.val mean_values}, {.val count_of_values}, or {.val list_values}.")
  }

  names(result) <- c(csn_vector_name, feature_name)


  if (format == "table") {
    return(result)
  } else if (format == "joined") {
    return(left_join(dataset, result))
  } else {
    cli::cli_abort("{.arg format} must be one of {.val table} or {.val joined}.")
  }

}

#' Extract flowsheet data
#'
#' Search the flowsheet to return features/variables for the specified measurements.
#'
#' Search time windows
#'
#' The default behaviour is to return all flowsheet entries for the admission of interest.
#' If *timeframe* = *"whole_flowsheet"* (**default**) it returns all entries for the admission.
#' If *timeframe* = *"pre_op"* it returns entries between admission datetime and operation datetime.
#' If *timeframe* = *"post_op"* it returns entries between operation datetime and discharge datetime.
#' If *timeframe* = *"custom"* supply values for `t0` and either `t1` or `t_int`.
#'
#' Timeframe CSN
#'
#' To allow temporal subsetting for each flowsheet entry we need a t0 and t1 time to execute the logical check against. To generate this we need to left join a timeframe dataframe to the flowsheet data. We do this leftjoin using the `admission_csn` column by default. This may cause issues in cases where `admission_csn` is duplicated within `dataset`.
#'
#' @param dataset The procedures dataset to search across.
#' @param flowsheet The flowsheet to extract from. Defaults to `Measurements`.
#' @param search_string Name of flowsheet to search for. Can be single flowsheet or vector. To return all flowsheets fitting the other filter criteria then set to NULL.
#' @param search_column The column which to search the `search_string` against, defaults to `flowsheet_question_name`. See help above.
#' @param check_search Boolean. Validate that the flowsheet names provided exist in the data.
#' @param timeframe Time window to search within. See details above.
#' @param timeframe_csn The CSN which to use to join back the timeframe parameters.
#' @param t0 If subsetting by time, a datetime column name to use as the start time.
#' @param t_int If subsetting by time, the time interval from `t0`.
#' @param t1 If subsetting by time, a datetime column name to use as the end time.
#' @param verbose Print steps as the function runs.
#'
#' @return An arrow query of the subset of flowsheet data of interest.
#' @export
#' @template setup-data
#'
#' @examples
#' # dataset <- cohort_complete_admissions(Procedures, add_to_eligability_criteria = FALSE)
#' # extract_flowsheet_data(dataset, flowsheet = Measurements, search_string = "Heart Rate")
extract_flowsheet_data <- function(
    dataset,
    flowsheet = Measurements,
    search_string = NA,
    search_column = flowsheet_question_name,
    check_search = TRUE,
    timeframe = c("whole_flowsheet", "pre_op", "post_op", "custom"),
    timeframe_csn = "admission_csn",
    t0 = NA,
    t_int = NA,
    t1 = NA,
    verbose = TRUE
) {

  if (verbose) {
    cli::cli_h1("Starting flowsheet feature extraction")
    cli::cli_h2("Checking data and variable quality")
  }

  # Argument checking
  if(is.null(dataset)){
    cli::cli_alert("No {.arg dataset} provided. Returning flowsheets for all patietns. Subsetting by time will not work. ")
  }else if (nrow(compute(dataset)) < 1) {
    cli::cli_abort("No rows in {.arg dataset}. Ensure you have not filtered away all patients.")
  }

  if (all(is.null(search_string))) {
    if (verbose) cli::cli_alert_info("No {.arg search_string} provided, returning all flowsheet rows.")
    check_search <- FALSE
  } else if (all(is.na(search_string))) {
    cli::cli_abort("No value specified for {.arg search_string}. \n\n If you intended to return all rows set {.arg search_string} = {.val NULL}.")
  }

  if (length(search_string) > 1) {
    if (verbose) cli::cli_alert_info("A Vector of flowsheet names provided, returning rows matching any within the vector.")
  }


  if(dataset %>% select(all_of(timeframe_csn)) %>% duplicated() %>% any()){
    cli::cli_alert_warning("There is duplication in {.val admission_csn} within {.val dataset}. This may cause duplication in the left join.")
  }


  # Timeframe assignment

  if (missing(timeframe)) {
    if (verbose) cli::cli_alert_success("No timeframe provided, searching whole flowsheet per encounter.")
    timeframe <- "whole_flowsheet"
  } else if (timeframe == "whole_flowsheet") {
    if (verbose) cli::cli_alert_success("{.arg timeframe} set to {.val {timeframe}}, searching whole flowsheet per encounter.")
    timeframe_df <- NA
  } else if (timeframe == "pre_op") {
    if (verbose) cli::cli_alert_success("{.arg timeframe} set to {.val {timeframe}}, searching only pre-operative period.")
    t0_col <- dataset %>% select(all_of(timeframe_csn), t0 = admission_date)
    t1_col <- dataset %>% select(all_of(timeframe_csn), t1 = operation_date)
    timeframe_df <- inner_join(t0_col, t1_col) %>% collect()
  } else if (timeframe == "post_op") {
    if (verbose) cli::cli_alert_success("{.arg timeframe} set to {.val {timeframe}}, searching only post-operative period.")
    t0_col <- dataset %>% select(all_of(timeframe_csn), t0 = operation_date)
    t1_col <- dataset %>% select(all_of(timeframe_csn), t1 = discharge_date)
    timeframe_df <- inner_join(t0_col, t1_col) %>% collect()
  } else if (timeframe == "custom") {
    if (verbose) cli::cli_alert_success("{.arg timeframe} set to {.val {timeframe}}, searching custom time period.")

    if (!is.na(t_int) & !is.na(t1)) {
      cli::cli_abort("Supply only one of {.arg t_int} or {.arg t1}, not both.")
    } else if (is.na(t0) & is.na(t1) & is.na(t_int)) {
      cli::cli_alert_danger("CAUTION: no end time supplied. Returning from {.arg t0} onwards.")
    }

    if (!is.na(t_int)) {

      timeframe_df <- dataset %>%
        select(all_of(timeframe_csn), t0 = all_of(t0)) %>%
        collect() %>%
        mutate(
          # Perform R-side arithmetic, then force UTC to match the Arrow schema
          t1 = lubridate::with_tz(t0 + t_int, tzone = "Etc/UTC")
        )

    } else if (!is.na(t1)) {
      timeframe_df <- dataset %>%
        select(all_of(timeframe_csn), t0 = all_of(t0), t1 = all_of(t1))
    } else {
      cli::cli_abort("Unexpected state in timeframe logic. Please {.href [open an issue](https://github.com/SAFEHR-data/IMPACTfunctions/issues)}.")
    }
  } else {
    cli::cli_abort("Incorrect value for {.arg timeframe}. See {.code ?extract_flowsheet_data}.")
  }

  if (verbose) cli::cli_alert_success("Arguments provided to function correct")

  # Validate flowsheet names

  if (check_search) {
    if (verbose) cli::cli_h2("Checking search terms")
    name_check_soft <- search_flowsheet_name(flowsheet, search_string, verbose = FALSE)
    if (sum(name_check_soft$rows) < 1) {
      cli::cli_abort("{.arg search_string} ({.val {search_string}}) not found. Check spelling with {.fun search_flowsheet_name}.")
    } else {
      if (verbose) cli::cli_alert_success("The following flowsheets where found: {.val {search_string}}")
    }

    name_check_strict <- search_flowsheet_name(
      flowsheet, search_string,
      search_column = "search_string", strict_search = TRUE, verbose = FALSE
    )
    if (sum(name_check_strict$rows) < 1) {
      cli::cli_abort(
        "{.val {search_string}} had no exact matches in {.val search_string}.
         \n\nUse {.fun search_flowsheet_name} to check available values:"
      )
      print(name_check_soft)
    } else {
      if (verbose) cli::cli_alert_success("{.val {search_string}} matched exactly in {.val search_string}")
    }
  }

  # Subsetting
  if (verbose) cli::cli_h2("Subsetting data")

  # 2a - Subset for flowsheet rows
  if (is.null(search_string)) {
    subset <- flowsheet
    if (verbose) cli::cli_alert_success("No specific flowsheet/s specified. Returning all ")
  } else {
    subset <- flowsheet %>% filter(get({{search_column}}) %in% search_string )
    if (verbose) cli::cli_alert_success("Subsetting for specifed flowsheets")
  }

  # 2b - Subset for patients in dataset
  csn_vector <- dataset %>% pull(admission_csn) %>% na.omit()
  subset <- subset %>% filter(admission_csn %in% csn_vector)
  if (verbose) cli::cli_alert_success("Subsetting for {.val admission_csn} in {.arg dataset}.")

  # ERROR CHECKING - See Github Issue: #31
  if(any(names(timeframe_df) %in% "anaesthesia_csn")){
    NULL
    # This is a bit of a bodge but when the datatype of the measuremtns sheet is fixed this whole section can be removed
    if (typeof(subset %>% pull(anaesthesia_csn)) != typeof(timeframe_df %>% pull(anaesthesia_csn))) {
      cli::cli_alert_danger("KNOWN BUG: {.val anaesthesia_csn} in Measurements is {.val int32} rather than {.val double}. Coercing to match. See {.href [github issue for more info](https://github.com/SAFEHR-data/IMPACTfunctions/issues/31)}")
      subset <- subset %>% mutate(anaesthesia_csn = as.double(anaesthesia_csn))
    } else {
      cli::cli_alert_success("KNOWN BUG FIXED: Please close {.href [github issue](https://github.com/SAFEHR-data/IMPACTfunctions/issues/31)}")
    }

  }

  # 2c - Subset for timeframe
  if (timeframe == "whole_flowsheet") {
    df <- subset
  } else {
    df <- left_join(subset, timeframe_df, by=timeframe_csn, copy = TRUE, relationship = "one-to-many")
    df <- df %>%
      filter(recorded_date  > t0) %>%
      filter(recorded_date  < t1)
    if (verbose) cli::cli_alert_success("Subsetting by {.val recorded_date} within {.arg timeframe}.")
  }

  return(df)
}

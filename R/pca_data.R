#' Extract PCA Data
#'
#' Extract PCA flowsheet rows from the measurements table and join with PCA metadata.
#'
#' @param dataset Study dataset with admission_csn.
#' @param flowsheet Measurements flowsheet table.
#' @param pca_flowsheet_lookup PCA flowsheet lookup dictionary.
#' @param verbose Logical. Print progress messages.
#' @param diagnostics Logical. Return diagnostic output (unused).
#'
#' @return Data frame with extracted PCA flowsheet data and joined PCA metadata.
#'
#' @export
extract_pca_data <- function(dataset,
                             flowsheet = Measurements,
                             pca_flowsheet_lookup = dictionary_flowsheet_pca,
                             verbose=TRUE,
                             diagnostics= FALSE){

  if(verbose){
    cli::cli_h3("Extracting PCA data")
    cli::cli_progress_step(
    "Extracting the PCA rows from the master flowsheet table...",
    msg_done = "PCA data extracted."
  )}

  if(any(is.na(pca_flowsheet_lookup))){
    cli::cli_alert_warning(paste0("Caution: there are NA values in the PCA flowsheet lookup. Some medications may not calcaulte the PCA totals correctly. \n \n Review the relevent sheet of the {.href [dictonary](", dict_url,")}. "))
  }


  # Pre-filter for involved patients only
  if(!missing(dataset)){
    flowsheet <- flowsheet %>%
      filter(admission_csn %in% dataset$admission_csn)

  }else{
    cli::cli_alert_warning("You have not provided a study dataset. Will retrive and proccess pca flowsheets for all patients... \n 'This little maneuver's gonna cost you 51 years... - Cooper, Intersteller'")
  }

  # Gather all of the flowsheet rows for the patients in dataset


  flowsheet <- extract_flowsheet_data(flowsheet = flowsheet,
                                      search_string = pca_flowsheet_lookup$flowsheet_row_epic_id,
                                      search_column = "flowsheet_row_epic_id",
                                      dataset = dataset,
                                      check_search = FALSE,
                                      timeframe = "whole_flowsheet",
                                      timeframe_csn = "admission_csn",
                                      verbose = FALSE)

  flowsheet_df <- flowsheet %>%
    collect() %>%
    mutate(flowsheet_row_epic_id = as.numeric(flowsheet_row_epic_id),
           value = as.numeric(value))

  if (verbose) {cli::cli_progress_done()}

  ### Prep the pca_flowsheet for the conversions by creating the mutipler, which is the value needed to get the mg per ml

  if(verbose){ cli::cli_progress_step(
    "Joining further PCA specific data columns from PCA dictionary...",
    msg_done = "PCA dictonary joined."
  )}

  pca_flowsheet_lookup <- pca_flowsheet_lookup %>%
    rowwise() %>%
    mutate(multiplier = str_extract_all(concentration,"\\(?[0-9,.]+\\)?")[[1]]) %>%
    mutate(multiplier = case_when(
      str_detect(concentration, "mcg") ~ as.numeric(multiplier)/1000,
      .default = as.numeric(multiplier)
    ))


  flowsheet_df <- flowsheet_df %>%
    select(admission_csn,flowsheet_row_epic_id, first_documented_date, recorded_date, value ) %>%
    left_join(pca_flowsheet_lookup, by= "flowsheet_row_epic_id", relationship = "many-to-many")


  if(verbose){
    cli::cli_progress_done()
    cli::cli_alert_success("Returning raw data from flowsheets")
    }

  return(flowsheet_df)

}

#' Process PCA Data
#'
#' Clean, classify, and interpolate PCA flowsheet data.
#'
#' @param pca_data Output from \code{extract_pca_data()}.
#' @param threshold_reset Numeric. Percentage drop threshold to detect resets (default 0.5).
#' @param threshold_cumulative Numeric. Proportion threshold to classify documentation type (default 0.9).
#' @param verbose Logical. Print progress messages.
#' @param diagnostics Logical. Return list for diagnostic dashboard instead of results.
#'
#' @return If \code{diagnostics = FALSE}: data frame with median PCA usage by admission and date.
#'   If \code{diagnostics = TRUE}: list containing processed flowsheet data and results.
#'
#' @export
proccess_pca_data <- function(pca_data,
                              threshold_reset = 0.5, # a drop of x percentage will trigger a reset
                    threshold_cumulative = 0.9, # What percentage of data points should be positive or neutral to conside the documentation type as cumulative
                    verbose = TRUE,
                    diagnostics = FALSE ){


  if(verbose){
    cli::cli_h3("Proccessing PCA data")
    cli::cli_progress_step(
    "Cleaning and deduplicating rows...",
    msg_done = "PCA data cleaned"
  )}

  # Check duplication, where there is duplication, keep the row with the most recent documented date
  flowsheet_df <- pca_data %>%
    group_by(admission_csn, flowsheet_row_epic_id, recorded_date) %>%
    slice_max(first_documented_date, n= 1, with_ties = FALSE) %>%
    arrange(admission_csn, recorded_date)

  ## Convert the volume given to mg given for direct comparison
  flowsheet_df <- flowsheet_df %>%
    mutate(
    value = case_when(
      flowsheet_type == "volume_given" ~ (value * as.numeric(multiplier)),
      flowsheet_type == "mg_given" & simple_generic_name == "fentanyl" ~ (value/1000),
      .default = value
    ))


  flowsheet_df <- flowsheet_df %>% filter(flowsheet_type %in% c("doses_given","mg_given", "volume_given" ))

  if(verbose){
    cli::cli_progress_done()
    cli::cli_progress_step(
      "Generating features...",
      msg_done = "Features generated"
    )}

  ## Assign the shift start time at 7am or pm.
  flowsheet_df <- flowsheet_df %>%
    mutate(shift_start = floor_date(recorded_date - hours(7), "12 hours") + hours(7)) %>%
    arrange(admission_csn, flowsheet_type, recorded_date)

  ## Calculate the delta of the value before (absolute diff) and the Delta (percentage diff)
  ## Calculate the proportion of delta values that are positive
  ## Detect resets
  flowsheet_df <- flowsheet_df %>%
    group_by(admission_csn, flowsheet_type, shift_start) %>%
    mutate(delta_value = if_else(row_number() == 1, 0, value - lag(value)),
           Delta_value = if_else(row_number() == 1, 0, (value - lag(value))/value)) %>%
    ungroup()

  ## Detect resetting values, defined as a drop by over 80%

  flowsheet_df <- flowsheet_df %>%
    group_by(admission_csn, flowsheet_type) %>%
    mutate(reset = ifelse(value < lag(value, 1)*threshold_reset, T, F),
           reset_to_zero = ifelse(value==0, T, F),
           pre_window_avg = (lag(value, 1) + lag(value, 2) + lag(value, 3)) / 3,
           post_window_avg = (lead(value, 1) + lead(value, 2) + lead(value, 3)) / 3,

           reset_true = case_when(
             reset == FALSE ~ NA,
             post_window_avg >= pre_window_avg * 0.8 ~ FALSE,
             post_window_avg < pre_window_avg * 0.8 ~ TRUE,
             .default = NA
           )) %>%
    group_by(admission_csn, flowsheet_type) %>%
    mutate(poportion_positive = sum(delta_value >= 0, na.rm = TRUE) / max(1, n())) %>%
    group_by(admission_csn, flowsheet_type, shift_start) %>%
    mutate(
      shift_poportion_positive = sum(delta_value >= 0, na.rm = TRUE) / max(1, n() - 1),

      ) %>%
    ungroup()

 # flowsheet_df %>% select(value, flowsheet_type, pre_window_avg:last_col()) %>% filter(flowsheet_type=="volume_given")

  if(verbose){
    cli::cli_progress_done()
    cli::cli_progress_step(
      "Classifying documentation type for flowsheets...",
      msg_done = "Flowsheets classifed"
    )}

  flowsheet_df <- flowsheet_df %>%
    group_by(admission_csn, flowsheet_type) %>%
    mutate(
      doc_type = ifelse(shift_poportion_positive>= threshold_cumulative, "Cumulative", "Incremental")) %>%
    ungroup()

  doc_type_guesses <- flowsheet_df %>%
    group_by(admission_csn, flowsheet_type) %>%
    summarize(flowsheet_doc_type =names(which.max(table(doc_type)))) %>%
    ungroup()

  flowsheet_df <- flowsheet_df %>%
    left_join(doc_type_guesses)

  # Correct the flowsheet values.
  # Where the flowsheet type is Incremental we just calculate the cumulative sum of the each subsequent value.
  # Where the flowsheet type is Cumulative,then we look for a reset signal before adding the previous value to all the subsequent values.

  if(verbose){
    cli::cli_progress_done()
    cli::cli_progress_step(
      "Interpolating data for resets and different documentation...",
      msg_done = "Flowsheets interpolated"
    )}


  flowsheet_df <- flowsheet_df %>%
    group_by(admission_csn, flowsheet_type) %>%
    mutate(
      reset_offset = case_when(
        reset == TRUE ~ lag(value, default = 0),
        TRUE ~ 0
      ),
      running_offset = cumsum(reset_offset),
      cumulative_value = case_when(
        flowsheet_doc_type == "Incremental" ~ cumsum(value),
        flowsheet_doc_type == "Cumulative" ~ value + running_offset,
        TRUE ~ value
      )
    ) %>%
    select(-reset_offset, -running_offset) %>%
    ungroup()


    ## Find any missing rows and interpolate them with the previous value (or if not present then the next future value)

    flowsheet_df <- flowsheet_df %>%
      group_by(admission_csn) %>%
      tidyr::complete(flowsheet_type, recorded_date) %>%
      arrange(recorded_date, .by_group = TRUE) %>%
      group_by(admission_csn, flowsheet_type) %>%
      tidyr::fill(cumulative_value, .direction = "downup") %>%
      ungroup()


    ## Correct the doses_given curve with the "mg per dose". This is an unknown quantity set by the prescription and  may change during the course

    # The volume and mg_given are already corrected at an earlier step


      candidate_lookup <- tibble::tribble(
        ~simple_generic_name, ~mg_per_dose_candidate,
        "morphine sulfate",  0.5,
        "morphine sulfate",  1,
        "morphine sulfate",  2,
        "fentanyl",  25,
        "fentanyl",  50
      )

      # There are some flowsheet types (eg "doses_given" that can be recorded with the same medication but on different flowsheets.
      #Pick the "most common" flowsheet to source this data. )
      flowsheet_df <- flowsheet_df %>%
        mutate(simple_generic_name = first(simple_generic_name),
               .by = c(admission_csn, recorded_date)) %>%
        mutate(n_id = n(), .by = c(admission_csn, flowsheet_type, flowsheet_row_epic_id, recorded_date, simple_generic_name)) %>%
        slice_max(n_id, by = c(admission_csn, flowsheet_type, recorded_date, simple_generic_name), with_ties = FALSE) %>%
        select(-n_id)

      flowsheet_wide <- flowsheet_df %>%
        select(admission_csn, recorded_date, flowsheet_type, cumulative_value, simple_generic_name) %>%
        distinct()

      flowsheet_wide <- flowsheet_wide%>%
        tidyr::pivot_wider(
          id_cols     = c(admission_csn, recorded_date, simple_generic_name),
          names_from  = flowsheet_type,
          values_from = cumulative_value
        )

      increments <- flowsheet_wide %>%
        arrange(admission_csn, recorded_date) %>%
        group_by(admission_csn, simple_generic_name) %>%
        mutate(
          dose_increment = doses_given - lag(doses_given),
          mg_increment   = mg_given - lag(mg_given)
        ) %>%
        ungroup() %>%
        filter(!is.na(dose_increment), dose_increment > 0)

      increments <- increments %>%
        inner_join(candidate_lookup, by = "simple_generic_name", relationship = "many-to-many") %>%
        mutate(sq_error = (mg_increment - dose_increment * mg_per_dose_candidate)^2) %>%
        group_by(admission_csn, simple_generic_name, mg_per_dose_candidate) %>%
        summarise(total_sq_error = sum(sq_error), .groups = "drop") %>%
        group_by(admission_csn, simple_generic_name) %>%
        slice_min(total_sq_error, n = 1, with_ties = FALSE) %>%
        ungroup() %>%
        rename(mg_per_dose = mg_per_dose_candidate) %>%
        select(admission_csn, simple_generic_name, mg_per_dose)

      flowsheet_df <- flowsheet_df %>%
      left_join(increments)

      flowsheet_df <- flowsheet_df%>%
      mutate(cumulative_value = case_when(
        flowsheet_type == "doses_given" ~ cumulative_value / mg_per_dose,
        .default = cumulative_value
      ))

    # Now we have three sheets, calculate the median trajectory

      if(verbose){
        cli::cli_progress_done()
        cli::cli_progress_step(
          "Producing best estimate of true PCA useage...",
          msg_done = "Estimates produced"
        )}

    result <- flowsheet_df %>%
      group_by(admission_csn, recorded_date, simple_generic_name) %>%
      summarise(
        cumulative_sum = median(cumulative_value, na.rm = TRUE),
        .groups = "drop"
      )

    result <- result %>%
      group_by(admission_csn, simple_generic_name) %>%
      tidyr::complete(recorded_date = seq(min(recorded_date), max(recorded_date), by = "1 hour")) %>%
      arrange(admission_csn, simple_generic_name, recorded_date) %>%
      mutate(
        cumulative_sum = zoo::na.approx(cumulative_sum, na.rm = FALSE),
        binned_value = cumulative_sum - lag(cumulative_sum, default = 0)
      ) %>%
      ungroup()

    if(verbose){cli::cli_progress_done()}

    if(diagnostics){
      cli::cli_alert_warning("Returning the list for diagnostics. Use in {.fn pca_diagnostics} only.")
      diagnotic_return <- list(flowsheet_df, result)
      return(diagnotic_return)
    }else{
      cli::cli_alert_success("Returning pca_dataset")
      return(result)
    }
}










extract_pca_data <- function(dataset,
                             flowsheet = Measurements,
                             pca_flowsheet_lookup = dictionary_flowsheet_pca,
                             verbose = TRUE){


  if(any(is.na(pca_flowsheet_lookup))){
    cli::cli_alert_warning(paste0("Caution: there are NA values in the PCA flowsheet lookup. \n \n Review the relevent sheet of the {.href [dictonary](", dict_url,")}. "))
  }


  # Pre-filter for involved patients only
  if(!missing(dataset)){
    flowsheet <- flowsheet %>%
      filter(admission_csn %in% dataset$admission_csn)

  }else{
    cli::cli_alert_warning("You have not provided a study dataset. Will retrive and proccess pca flowsheets for all patients... \nThis may take some time...")
  }

  # Gather all of the flowsheet rows for the patients in dataset
  if(verbose){cli::cli_progress_done("Gathering data from flowsheets")}

  flowsheet <- extract_flowsheet_data(flowsheet = flowsheet,
                                      search_string = pca_flowsheet_lookup$flowsheet_row_epic_id,
                                      search_column = "flowsheet_row_epic_id",
                                      dataset = dataset,
                                      check_search = FALSE,
                                      timeframe = "whole_flowsheet",
                                      timeframe_csn = "admission_csn")

  flowsheet_df <- flowsheet %>%
    collect() %>%
    mutate(flowsheet_row_epic_id = as.numeric(flowsheet_row_epic_id),
           value = as.numeric(value))

  flowsheet_df <- flowsheet_df %>%
    select(admission_csn,flowsheet_row_epic_id, first_documented_date, recorded_date, value ) %>%
    left_join(pca_flowsheet_lookup, by= "flowsheet_row_epic_id", relationship = "many-to-many")

  if(verbose){cli::cli_progress_done("Gathered data from flowsheets")}

  # Check duplication, where there is duplication, keep the row with the most recent documented date

  flowsheet_df <- flowsheet_df %>%
    group_by(admission_csn, flowsheet_row_epic_id, recorded_date) %>%
    slice_max(first_documented_date, n= 1, with_ties = FALSE) %>%
    arrange(admission_csn, recorded_date)

  ## For each flowsheet type, calculate the delta from the previous row. For the first recorded row the delta is just 0 as we assume a starting value of 0

  flowsheet_df <- flowsheet_df %>%
    #Find the shift start time (assuming 12 hour shifts and starting at 7am/pm) for use later
    mutate(
      shift_start = floor_date(recorded_date - hours(7), "12 hours") + hours(7)
    ) %>%
    arrange(admission_csn, simple_generic_name, flowsheet_type, shift_start, recorded_date) %>%
    group_by(admission_csn, simple_generic_name, flowsheet_type,shift_start) %>%
    # Group by the above and calcualte the delta of the previous value for that flowsheet in that shift
    mutate(
      delta_value = if_else(
        condition = row_number() == 1,
        true = value,
        false = value - lag(value)
      )
    ) %>%
    ungroup()



  # Define the documentation type
  # We can determine two types of documentation: cumulative (where you recorded the cumulative value to value) or Incremental (where you record the difference or "value since the last")
  # Cumulative will have high positivity ratio (its always growing) where as incremental will be more like a random walk (half the time it grows, half the time it shrinks)
  # We can also consider that the documentation type may change between shifts and between flowsheets.


  positivity_index <- flowsheet_df %>%
    group_by(admission_csn, simple_generic_name, flowsheet_type, shift_start) %>%
    summarise(n_obs = n(),
              # Monotonicity: Proportion of steps that do not decrease
              positivity_index = sum(delta_value > 0)/n_obs,
              # Structural Resets: Count how many times a negative delta occurs
              proportion_negative = sum(delta_value <0)/n_obs,


    )


  ggplot(positivity_index, aes(x=proportion_negative, ))+
    geom_density()
}

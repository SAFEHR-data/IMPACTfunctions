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

  ### REMOVE ME AFTER TESTING
  flowsheet_df <- flowsheet_df %>% filter(admission_csn %in% unique(flowsheet_df$admission_csn)[1:50])
  cli::cli_alert("Remove me after testing")

  ### Prep the pca_flowsheet for the conversions by creating the mutipler, which is the value needed to get the mg per ml

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

  if(verbose){cli::cli_progress_done("Gathered data from flowsheets")}

  # Check duplication, where there is duplication, keep the row with the most recent documented date
  flowsheet_df <- flowsheet_df %>%
    group_by(admission_csn, flowsheet_row_epic_id, recorded_date) %>%
    slice_max(first_documented_date, n= 1, with_ties = FALSE) %>%
    arrange(admission_csn, recorded_date)

  ## Convert the volume given to mg given for direct comparison
  flowsheet_df <- flowsheet_df %>%
    mutate(
    value = case_when(
      flowsheet_type == "volume_given" ~ (value * as.numeric(multiplier)),
      .default = value
    )
    )

  ### NEW process

  flowsheet_df <- flowsheet_df %>% filter(flowsheet_type %in% c("doses_given","mg_given", "volume_given" ))

  ## Assign the shift start time at 7am or pm.
  flowsheet_df <- flowsheet_df %>%
    mutate(shift_start = floor_date(recorded_date - hours(7), "12 hours") + hours(7)) %>%
    arrange(admission_csn, flowsheet_type, recorded_date)

  ## Calculate the delta of the value before (absolute diff) and the Delta (percetnage diff)
  ## Calculate the proportion of delta values that are positive
  ## Detect resets
  flowsheet_df <- flowsheet_df %>%
    group_by(admission_csn, flowsheet_type, shift_start) %>%
    mutate(delta_value = if_else(row_number() == 1, 0, value - lag(value)),
           Delta_value = if_else(row_number() == 1, 0, (value - lag(value))/value)) %>%
    ungroup()

  ## Detect resetting values, defined as a drop by over 80%

  flowsheet_df <- flowsheet_df %>%
    mutate(reset = ifelse(Delta_value <= -0.8, T, F),
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

  flowsheet_df %>% select(value, flowsheet_type, pre_window_avg:last_col()) %>% filter(flowsheet_type=="volume_given")

  threshold_cumulative <- 0.7

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

  ggplot(flowsheet_df %>% filter(admission_csn == unique(flowsheet_df$admission_csn)[2] ), aes(x= recorded_date, y=value, colour = flowsheet_doc_type, group = flowsheet_type))+
    geom_line()+
    geom_point(shape=4, colour="darkgray")+
    geom_point(data = . %>% filter(reset==TRUE), colour = "orange")+
    geom_point(data = . %>% filter(reset_to_zero==TRUE), colour="red")+
    geom_point(data = . %>% filter(reset_true==FALSE), colour="purple", shape = 3)+
    facet_grid(flowsheet_type~., scales = "free")


  ### CLASSIFY THE DOCUMENTATION TYPE







  ## For each flowsheet type, calculate the delta from the previous row. For the first recorded row the delta is just 0 as we assume a starting value of 0

  flowsheet_df <- flowsheet_df %>%
    #Find the shift start time (assuming 12 hour shifts and starting at 7am/pm) for use later
    mutate(
      shift_start = floor_date(recorded_date - hours(7), "12 hours") + hours(7)
    ) %>%
    arrange(admission_csn, simple_generic_name, flowsheet_type, shift_start, recorded_date) %>%
    group_by(admission_csn, simple_generic_name, flowsheet_type,shift_start) %>%
    # Group by the above and calculate the delta of the previous value for that flowsheet in that shift
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

  flowsheet_df <- flowsheet_df %>% filter(flowsheet_type == "mg_given")

  positivity_index <- test %>%
    group_by(admission_csn, simple_generic_name, flowsheet_type) %>%
    summarise(n_obs = n(),
              #proportion_positive = sum(delta_value > 0)/n_obs,
              #proportion_negative = sum(delta_value < 0)/n_obs,
              #count_negative = count(delta_value < 0)

    )

  value <- flowsheet_df %>% filter(admission_csn %in% unique(flowsheet_df$admission_csn)[1]) %>% pull(value)

  reconstruct_cumulative <- function(value, look_ahead = 2, tolerance = 0) {



    n <- length(value)
    clean_vec <- value

    # --- Phase 1: Detect and Impute False Resets ---
    i <- 2
    while (i <= n) {
      # Check for a significant downward drop
      if ((clean_vec[i] - clean_vec[i-1]) < -tolerance) {
        print("Negative delta detected at i = ")
        print(i)
        pre_drop_val <- clean_vec[i-1]
        window_end <- min(i + look_ahead, n)

        # Look ahead to find if/where the signal recovers
        recovery_idx <- which(clean_vec[i:window_end] >= pre_drop_val)[1]

        if (!is.na(recovery_idx)) {
          print("Negative delta appears false reset")
          # Actual index in the main vector
          j <- i + recovery_idx - 1

          # False Reset Confirmed: Linearly interpolate over the transient drop
          span <- (i-1):j
          clean_vec[span] <- seq(clean_vec[i-1], clean_vec[j], length.out = length(span))

          # Advance pointer past the anomaly
          i <- j + 1
        } else {
          print("Negative delta appears true reset")
          # True Reset Confirmed: Leave for Phase 2
          i <- i + 1
        }
      } else {
        i <- i + 1
      }
    }

    # --- Phase 2: Unwrap True Resets ---
    # Compute first-differences of the partially smoothed signal
    deltas <- diff(clean_vec)

    # Replace true reset drops with 0 (preserves baseline)
    # or use mean/median increment if continuous growth is expected during the reset step
    deltas[deltas < 0] <- median(deltas[deltas > 0])

    # Reconstruct the uninterrupted monotonic cumulative trajectory
    corrected_cumulative <- cumsum(c(clean_vec[1], deltas))

    return(corrected_cumulative)
  }

  value <- flowsheet_df %>% filter(admission_csn %in% unique(flowsheet_df$admission_csn)[1]) %>% pull(value)
  clean_vec <- reconstruct_cumulative(value, tolerance = 0, look_ahead=5)

  ex <- data.frame(value, clean_vec, "n"=1:length(value))

  ggplot(ex, aes(x=n, y = value))+
    geom_line()+
    geom_line(aes(y=clean_vec), color="red")

  print(ex)


  ggplot(flowsheet_df %>% filter(admission_csn %in% unique(flowsheet_df$admission_csn)[1]),
         aes(y= value, x= recorded_date, group = admission_csn, color = admission_csn))+
    geom_line()

  ggplot(positivity_index, aes(x=proportion_negative, ))+
    geom_density()



}



#' PCA data helper function - reconstruct cumulative sums
#'
#' This function is used to correct the observed values in pca flowsheets that are documented cumulativley.
#'
#' In a cumulative documentation style we expect the value for `i` to tbe the equal to previous value plus the change since the last value.
#'
#' We can have two type of resets:
#'
#' - A true reset where we reset the count and the value for `i` is now equal to `0` plus the change since the last value, this then contiunes until the next reset.
#'
#' - A false reset where a single or a few values are erronously reset before the count then return back to the previous reset.
#'
#'
#'
#' @param value Vector of the observed values
#' @param look_ahead
#' @param tolerance
#'
#' @return
#' @export
#'
#' @examples
reconstruct_cumulative <- function(value, look_ahead = 2, tolerance = 0) {

   n <- length(value)
  clean_vec <- value

  # --- Phase 1: Detect and Impute False Resets ---
  i <- 2
  while (i <= n) {
    # Check for a significant downward drop
    if ((clean_vec[i] - clean_vec[i-1]) < -tolerance) {
      print("Negative delta detected at i = ")
      print(i)
      pre_drop_val <- clean_vec[i-1]
      window_end <- min(i + look_ahead, n)

      # Look ahead to find if/where the signal recovers
      recovery_idx <- which(clean_vec[i:window_end] >= pre_drop_val)[1]

      if (!is.na(recovery_idx)) {
        print("Negative delta appears false reset")
        # Actual index in the main vector
        j <- i + recovery_idx - 1

        # False Reset Confirmed: Linearly interpolate over the transient drop
        span <- (i-1):j
        clean_vec[span] <- seq(clean_vec[i-1], clean_vec[j], length.out = length(span))

        # Advance pointer past the anomaly
        i <- j + 1
      } else {
        print("Negative delta appears true reset")
        # True Reset Confirmed: Leave for Phase 2
        i <- i + 1
      }
    } else {
      i <- i + 1
    }
  }

  # --- Phase 2: Unwrap True Resets ---
  # Compute first-differences of the partially smoothed signal
  deltas <- diff(clean_vec)

  # Replace true reset drops with 0 (preserves baseline)
  # or use mean/median increment if continuous growth is expected during the reset step
  deltas[deltas < 0] <- median(deltas[deltas > 0])

  # Reconstruct the uninterrupted monotonic cumulative trajectory
  corrected_cumulative <- cumsum(c(clean_vec[1], deltas))

  return(corrected_cumulative)
}

#' Cohorting: Ensure complete minimum dataset
#'
#' Middle level contorting function to ensure minimum dataset is present.
#' This function when run with the all enforcements turned on (default) will essentially run:
#'
#' `Procedures %>% cohort_enforce_discharged() %>% cohort_enforce_dates_complete() %>% cohort_enforce_dates_ordered() %>% cohort_enforce_complete_anaesthetic_chart() %>% cohort_enforce_procedure_coded()`
#'
#' To drop one of these enforcement functions out turn the corresponding function off.
#' To investigate exactly which patients are being removed at each step you can run each function with diagnostics set to TRUE
#'
#' `result <- Procedures %>% cohort_enforce_complete_anaesthetic_chart(diagnostic=TRUE)`
#' `result$excluded %>% collect() %>% View()`
#'
#'
#' @param procedures_table A arrow database binding to the procedures table
#' @param enforce_dates_complete Enforce that the date columns provided in `dates_completed_cols` are complete.
#' @param enforce_date_order Enforce that the order of dates is suitable and correct
#' @param enforce_discharged Enforce that patient have a discharge date (to remove ongoing admissions)
#' @param enforce_coded Enforce that the surgery had been coded by the clinical codeing dept
#' @param enforce_complete_anaesthetic_chart Enforce that `anaesthesia_csn` is not `NA` to ensure there is a corresponding record
#' @param dates_completed_cols A vector of character column names to ensure completeness for the `enforce_dates_complete`
#' @param verbose Print output and numbers of patients removed at each step
#' @param add_to_eligability_criteria If this filtering should be added to the `eligability_criteria` dataframe
#'
#' @return A arrow object that is a filtered subset of the origional `procedures_table`
#' @family cohort
#' @export
#' @template setup-data
#' @examples
#'
#' library(dplyr)
#' dataset <- cohort_complete_admissions(procedures_table = Procedures, verbose=TRUE)
#' #Use the glimpse function to peer into the dataset
#'  glimpse(dataset)
#'
#' #Use the collect function to pull the dataset into memory as a dataframe
#' dataset_df <- collect(dataset)
cohort_complete_admissions <- function(procedures_table=Procedures,
                                         enforce_discharged=TRUE,
                                         enforce_dates_complete=TRUE,
                                         dates_completed_cols = c("admission_date", "operation_date", "discharge_date"),
                                         enforce_date_order=TRUE,
                                         enforce_coded=TRUE,
                                         enforce_complete_anaesthetic_chart=TRUE,
                                         add_to_eligability_criteria = TRUE,
                                         verbose=FALSE){


  # Preamble for verbose
  if(verbose){
    cli::cli_h1("Filtering admissions on completeness")
    cli::cli_alert_info("Starting with a dataset of {.val {nrow(procedures_table)}} surgical procedures")
    n_start <- nrow(procedures_table) }

  # cohort_enforce_discharged()
  if(enforce_discharged){
    procedures_table <- cohort_enforce_discharged(procedures_table, add_to_eligability_criteria, verbose)
  }

  # cohort_enforce_dates_complete()
  if(enforce_dates_complete){
    procedures_table <- cohort_enforce_dates_complete(procedures_table, dates_completed_cols = dates_completed_cols , add_to_eligability_criteria, verbose)
  }

  if(enforce_date_order){
    procedures_table <- cohort_enforce_dates_ordered(procedures_table, add_to_eligability_criteria, verbose)
  }

  if(enforce_complete_anaesthetic_chart){
    procedures_table <- cohort_enforce_complete_anaesthetic_chart(procedures_table, add_to_eligability_criteria, verbose)
  }

  if(enforce_coded){
    procedures_table <- cohort_enforce_procedure_coded(procedures_table, add_to_eligability_criteria, verbose)
  }

  if(verbose){cli::cli_alert_success("Returning the filtered procedures table with {.val {nrow(procedures_table)}} procedures, having removed {.val {n_start-nrow(procedures_table)}} procedures.")}
  return(procedures_table)
}

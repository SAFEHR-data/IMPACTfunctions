#' Eligibility criteria - enforcement level functions
#'
#' Below is all of the enforcement level functions for contorting Each one will execute a specific filter against the dataset. These can be chained together or used in a middle or top level function.
#'
#' Find a list of the possible functions in the **See Also** section below.
#'
#' @param dataset A arrow database binding
#' @param add_to_eligability_criteria If we want to add this level of filtering to the `eligability_criteria` dataframe
#' @param verbose Print outputs of the function
#' @param diagnostics To check the eligibility criteria, `diagnostics=TRUE` will return the filtered data but also the excluded data
#'
#' @family cohort_enforce
#' @name cohort_enforce_family
#' @examples
#' library(dplyr)
#' load_synthetic_dataset()
#' filtered_dataset <- cohort_enforce_complete_anaesthetic_chart(Procedures)
#' filtered_dataset %>% collect() %>% head()
#'
#' ## Using diagnostics to see which patients are being excluded
#' list_of_datasets <- cohort_enforce_dates_ordered(dataset = Procedures,
#'       verbose = TRUE,
#'      diagnostics = TRUE)
#'
#' # View the rows that are included
#' list_of_datasets$included %>% collect() %>% head()
#' # View the rows that have been excluded
#' list_of_datasets$excluded %>% collect() %>% head()
NULL
#> NULL


#' Eligibility criteria: Complete anaesthetic chart
#'
#' Filters out patients where the is no `anaesthetisa_CSN` number. This is commonly because there was no anaesthetic chart found for that procedure.
#'
#' @inheritParams cohort_enforce_family
#' @family cohort_enforce
#' @export
#'
cohort_enforce_complete_anaesthetic_chart <- function(dataset,
                                                      add_to_eligability_criteria=TRUE,
                                                      verbose = FALSE,
                                                      diagnostics = FALSE){

  prior <- nrow(dataset)
  dataset_filtered <- dataset %>% filter(!is.na(anaesthesia_csn))
  filtered <- prior - nrow(dataset_filtered)

  if(verbose){
    cli::cli_alert("Filtering out {.val {filtered}} due to missing anaesthetic chart entries")
  }

  if(add_to_eligability_criteria){
    eligibility_criteria_add(type = "Exclusion",
                             function_name = "cohort_enforce_complete_anaesthetic_chart()",
                             function_filter = "filter(!is.na(anaesthesia_csn))",
                             n_prior = prior,
                             n_filtered = filtered)
  }

  if(diagnostics){
    return(list("included" = dataset_filtered,
                "excluded" =  dataset %>% filter(!case_id %in% (dataset_filtered %>% pull(case_id))))
           )
  }else{
    return(dataset_filtered)
  }

}

#' Eligibility criteria: Ensure OPSC codeing is complete
#' @inheritParams cohort_enforce_family
#' @family cohort_enforce
#' @export
#'
cohort_enforce_procedure_coded <- function(dataset,
                                           add_to_eligability_criteria=TRUE,
                                           verbose = FALSE,
                                           diagnostics = FALSE){


  prior <- nrow(dataset)
  dataset_filtered <- dataset %>% filter(!is.na(primary_coded_procedure) & !is.na(combined_coded_opcs_procedures))
  filtered <- prior - nrow(dataset_filtered)

  if(verbose){
    cli::cli_alert("Filtering out {.val {filtered}} due to procedure codeing not complete")
  }

  if(add_to_eligability_criteria){
    eligibility_criteria_add(type = "Exclusion",
                             function_name = "cohort_enforce_procedure_coded()",
                             function_filter = "filter(!is.na(primary_coded_procedure) & !is.na(combined_coded_opcs_procedures)))",
                             n_prior = prior,
                             n_filtered = filtered)
  }

  if(diagnostics){
    return(list("included" = dataset_filtered,
                "excluded" =  dataset %>% filter(!case_id %in% (dataset_filtered %>% pull(case_id))))
    )
  }else{
    return(dataset_filtered)
  }

}

#' Eligibility criteria: Ensure key dates complete
#' @param dates_completed_cols The date columns which to ensure are complete. This defaults to the admission, operation and discharge date
#' @inheritParams cohort_enforce_family
#' @family cohort_enforce
#' @export
#'
cohort_enforce_dates_complete <- function(dataset,
                                           dates_completed_cols = c("admission_date", "operation_date", "discharge_date"),
                                           add_to_eligability_criteria=TRUE,
                                           verbose = FALSE,
                                           diagnostics = FALSE){


  prior <- nrow(dataset)
  dataset_filtered <- dataset %>% filter(if_all(all_of(dates_completed_cols), ~ !is.na(.x)))
  filtered <- prior - nrow(dataset_filtered)

  if(verbose){
    cli::cli_alert("Filtering out {.val {filtered}} due to missing dates in one of {.vals {dates_completed_cols}} columns")
  }

  if(add_to_eligability_criteria){
    eligibility_criteria_add(type = "Exclusion",
                             function_name = "cohort_enforce_dates_complete()",
                             function_filter = "filter(if_all(all_of(dates_completed_cols), ~ !is.na(.x))))",
                             n_prior = prior,
                             n_filtered = filtered)
  }

  if(diagnostics){
    return(list("included" = dataset_filtered,
                "excluded" =  dataset %>% filter(!case_id %in% (dataset_filtered %>% pull(case_id))))
    )
  }else{
    return(dataset_filtered)
  }

}

#' Eligibility criteria: Ensure key dates are in the correct temporal order
#'
#' The function ensures that of the date columns they are in correct order.
#'
#' The order is currently fixed that following datetime occur sequentially:  admission -> operation -> discharge. In a future version of the fuction it would be good to add flexability and specify the columns of intrest.
#' @inheritParams cohort_enforce_family
#' @family cohort_enforce
#' @export
#'
cohort_enforce_dates_ordered <- function(dataset,
                                          add_to_eligability_criteria=TRUE,
                                          verbose = FALSE,
                                          diagnostics = FALSE){


  prior <- nrow(dataset)
  dataset_filtered <- dataset %>% filter(admission_date < operation_date) %>% filter(operation_date < discharge_date)
  filtered <- prior - nrow(dataset_filtered)

  if(verbose){
    cli::cli_alert("Filtering out {.val {filtered}} due to the temoral order of admission datetime < operation datetime < discharge datetime")
  }

  if(add_to_eligability_criteria){
    eligibility_criteria_add(type = "Exclusion",
                             function_name = "cohort_enforce_dates_ordered()",
                             function_filter = "filter(admission_date < operation_date) %>% filter(operation_date < discharge_date))",
                             n_prior = prior,
                             n_filtered = filtered)
  }

  if(diagnostics){
    return(list("included" = dataset_filtered,
                "excluded" =  dataset %>% filter(!case_id %in% (dataset_filtered %>% pull(case_id))))
    )
  }else{
    return(dataset_filtered)
  }

}

#' Eligibility criteria: Ensure that a patient has been discharged
#'
#' The function ensures that there is a discharge datetime recorded.
#'
#' @inheritParams cohort_enforce_family
#' @family cohort_enforce
#' @export
#'
cohort_enforce_discharged <- function(dataset,
                                      add_to_eligability_criteria=TRUE,
                                      verbose = FALSE,
                                      diagnostics = FALSE){


  prior <- nrow(dataset)
  dataset_filtered <- dataset %>% filter(!is.na(discharge_date))
  filtered <- prior - nrow(dataset_filtered)

if(verbose){
  cli::cli_alert("Filtering out {.val {filtered}} due to missing discharge datetime")
}

if(add_to_eligability_criteria){
  eligibility_criteria_add(type = "Exclusion",
                           function_name = "cohort_enforce_discharged()",
                           function_filter = "filter(!is.na(discharge_date))",
                           n_prior = prior,
                           n_filtered = filtered)
}

  if(diagnostics){
    return(list("included" = dataset_filtered,
                "excluded" =  dataset %>% filter(!case_id %in% (dataset_filtered %>% pull(case_id))))
    )
  }else{
    return(dataset_filtered)
  }

}


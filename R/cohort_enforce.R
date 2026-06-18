#' Eligibility criteria - enforcement level functions
#'
#' Below is all of the enforcement level functions for contorting Each one will execute a specific filter against the dataset. These can be chained together or used in a middle or top level function.
#'
#' Find a list of the possible functions in the **See Also** section below.
#'
#' @param procedures_table A arrow database binding to the procedures table
#' @param add_to_eligability_criteria If we want to add this level of filtering to the `eligability_criteria` dataframe
#' @param verbose Print outputs of the function
#' @param diagnostics To check the eligibility criteria, `diagnostics=TRUE` will return the filtered data but also the excluded data
#'
#' @name cohort_enforce_family
#' @keywords internal
#' @family cohort_enforce
#' @import dplyr
#' @template setup-data
#' @examples
#'
#' library(dplyr)
#'
#' filtered_dataset <- cohort_enforce_complete_anaesthetic_chart(Procedures)
#' filtered_dataset %>% collect() %>% head()
#'
#' ## Using diagnostics to see which patients are being excluded
#' list_of_datasets <- cohort_enforce_dates_ordered(procedures_table = Procedures,
#'       verbose = TRUE,
#'      diagnostics = TRUE)
#'
#' # View the rows that are included
#' list_of_datasets$included %>% collect() %>% head()
#' # View the rows that have been excluded
#' list_of_datasets$excluded %>% collect() %>% head()
#'
NULL

#' Eligibility criteria: Complete anaesthetic chart
#'
#' Filters out patients where the is no `anaesthetisa_CSN` number. This is commonly because there was no anaesthetic chart found for that procedure.
#'
#' @inheritParams cohort_enforce_family
#' @family cohort_enforce
#' @export
#'
cohort_enforce_complete_anaesthetic_chart <- function(procedures_table,
                                                      add_to_eligability_criteria=TRUE,
                                                      verbose = FALSE,
                                                      diagnostics = FALSE){

  prior <- nrow(procedures_table)
  procedures_table_filtered <- procedures_table %>% dplyr::filter(!is.na(anaesthesia_csn))
  filtered <- prior - nrow(procedures_table_filtered)

  if(verbose){
    cli::cli_alert("Filtering out {.val {filtered}} due to missing anaesthetic chart entries")
  }

  if(add_to_eligability_criteria){
    eligibility_criteria_add(type = "Exclusion",
                             text = "Procedures missing anaesthetic chart entry",
                             function_name = "cohort_enforce_complete_anaesthetic_chart()",
                             function_filter = "dplyr::filter(!is.na(anaesthesia_csn))",
                             n_prior = prior,
                             n_filtered = filtered)
  }

  if(diagnostics){
    return(list("included" = procedures_table_filtered,
                "excluded" =  procedures_table %>% dplyr::filter(!case_id %in% (procedures_table_filtered %>% pull(case_id, as_vector = FALSE))))
           )
  }else{
    return(procedures_table_filtered)
  }

}

#' Eligibility criteria: Ensure OPSC codeing is complete
#' @inheritParams cohort_enforce_family
#' @family cohort_enforce
#' @export
#'
cohort_enforce_procedure_coded <- function(procedures_table,
                                           add_to_eligability_criteria=TRUE,
                                           verbose = FALSE,
                                           diagnostics = FALSE){


  prior <- nrow(procedures_table)
  procedures_table_filtered <- procedures_table %>% dplyr::filter(!is.na(primary_coded_procedure) & !is.na(combined_coded_opcs_procedures))
  filtered <- prior - nrow(procedures_table_filtered)

  if(verbose){
    cli::cli_alert("Filtering out {.val {filtered}} due to procedure codeing not complete")
  }

  if(add_to_eligability_criteria){
    eligibility_criteria_add(type = "Exclusion",
                             text = "Procedures without codeing information",
                             function_name = "cohort_enforce_procedure_coded()",
                             function_filter = "dplyr::filter(!is.na(primary_coded_procedure) & !is.na(combined_coded_opcs_procedures)))",
                             n_prior = prior,
                             n_filtered = filtered)
  }

  if(diagnostics){
    return(list("included" = procedures_table_filtered,
                "excluded" =  procedures_table %>% dplyr::filter(!case_id %in% (procedures_table_filtered %>% pull(case_id, as_vector = FALSE))))
    )
  }else{
    return(procedures_table_filtered)
  }

}

#' Eligibility criteria: Ensure key dates complete
#' @param dates_completed_cols The date columns which to ensure are complete. This defaults to the admission, operation and discharge date
#' @inheritParams cohort_enforce_family
#' @family cohort_enforce
#' @export
#'
cohort_enforce_dates_complete <- function(procedures_table,
                                           dates_completed_cols = c("admission_date", "operation_date", "discharge_date"),
                                           add_to_eligability_criteria=TRUE,
                                           verbose = FALSE,
                                           diagnostics = FALSE){


  prior <- nrow(procedures_table)
  procedures_table_filtered <- procedures_table %>% dplyr::filter(if_all(all_of(dates_completed_cols), ~ !is.na(.x)))
  filtered <- prior - nrow(procedures_table_filtered)

  if(verbose){
    cli::cli_alert("Filtering out {.val {filtered}} due to missing dates in one of {.vals {dates_completed_cols}} columns")
  }

  if(add_to_eligability_criteria){
    eligibility_criteria_add(type = "Exclusion",
                             text = "Procedures with incomplete timestamps for admission, operation or discharge",
                             function_name = "cohort_enforce_dates_complete()",
                             function_filter = "dplyr::filter(if_all(all_of(dates_completed_cols), ~ !is.na(.x))))",
                             n_prior = prior,
                             n_filtered = filtered)
  }

  if(diagnostics){
    return(list("included" = procedures_table_filtered,
                "excluded" =  procedures_table %>% dplyr::filter(!case_id %in% (procedures_table_filtered %>% pull(case_id, as_vector = FALSE))))
    )
  }else{
    return(procedures_table_filtered)
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
cohort_enforce_dates_ordered <- function(procedures_table,
                                          add_to_eligability_criteria=TRUE,
                                          verbose = FALSE,
                                          diagnostics = FALSE){


  prior <- nrow(procedures_table)
  procedures_table_filtered <- procedures_table %>% dplyr::filter(admission_date < operation_date) %>% dplyr::filter(operation_date < discharge_date)
  filtered <- prior - nrow(procedures_table_filtered)

  if(verbose){
    cli::cli_alert("Filtering out {.val {filtered}} due to the temoral order of admission datetime < operation datetime < discharge datetime")
  }

  if(add_to_eligability_criteria){
    eligibility_criteria_add(type = "Exclusion",
                             text = "Procedures where timestamps for admission, operation and discharge are not sequential",
                             function_name = "cohort_enforce_dates_ordered()",
                             function_filter = "dplyr::filter(admission_date < operation_date) %>% dplyr::filter(operation_date < discharge_date))",
                             n_prior = prior,
                             n_filtered = filtered)
  }

  if(diagnostics){
    return(list("included" = procedures_table_filtered,
                "excluded" =  procedures_table %>% dplyr::filter(!case_id %in% (procedures_table_filtered %>% pull(case_id, as_vector = FALSE))))
    )
  }else{
    return(procedures_table_filtered)
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
cohort_enforce_discharged <- function(procedures_table,
                                      add_to_eligability_criteria=TRUE,
                                      verbose = FALSE,
                                      diagnostics = FALSE){


  prior <- nrow(procedures_table)
  procedures_table_filtered <- procedures_table %>% dplyr::filter(!is.na(discharge_date))
  filtered <- prior - nrow(procedures_table_filtered)

if(verbose){
  cli::cli_alert("Filtering out {.val {filtered}} due to missing discharge datetime")
}

if(add_to_eligability_criteria){
  eligibility_criteria_add(type = "Exclusion",
                           text = "Patients not yet discharged or missing discharge dates",
                           function_name = "cohort_enforce_discharged()",
                           function_filter = "dplyr::filter(!is.na(discharge_date))",
                           n_prior = prior,
                           n_filtered = filtered)
}

  if(diagnostics){
    return(list("included" = procedures_table_filtered,
                "excluded" =  procedures_table %>% dplyr::filter(!case_id %in% (procedures_table_filtered %>% pull(case_id, as_vector = FALSE))))
    )
  }else{diagnostics
    return(procedures_table_filtered)
  }

}

#' Eligibility criteria: Remove patients who have triggered the Data opt-out
#'
#' The function removed patients flagged for data opt out. This is useful for generating research cohorts.
#' See Data schema for exact details on how the flag is triggered.
#'
#' @param demographics_table Demographics table
#' @param remove_restricted If restricted (break the glass) patients should also be filtered out
#' @inheritParams cohort_enforce_family
#' @family cohort_enforce
#' @export
#'
cohort_enforce_opt_out <- function(procedures_table,
                                   demographics_table = Demographics,
                                   remove_restricted = TRUE,
                                   add_to_eligability_criteria=TRUE,
                                   verbose = FALSE,
                                   diagnostics = FALSE){


  mrn_to_filter <- demographics_table %>% select(mrn,restricted_patient_flag, opt_out_flag)

  if(remove_restricted){
    # If remove_restricted = true then filter by OR
    mrn_to_filter <- mrn_to_filter  %>% filter(opt_out_flag == 1 | restricted_patient_flag == 1) %>% pull(mrn, as_vector = FALSE)
  }else{
    # If not then only filter by Opt out
    mrn_to_filter <- mrn_to_filter %>% filter(opt_out_flag == 1) %>% pull(mrn, as_vector = FALSE)
  }

  prior <- nrow(procedures_table)
  procedures_table_filtered <- procedures_table %>% dplyr::filter(!mrn %in% mrn_to_filter)
  filtered <- prior - nrow(procedures_table_filtered)

  if(verbose){
    cli::cli_alert("Filtering out {.val {filtered}} procedures as from Opt out +/- Restricted access ")
  }

  if(add_to_eligability_criteria){
    eligibility_criteria_add(type = "Exclusion",
                             text = ifelse(remove_restricted, "Patients in the NHS National data opt out and restricted records", "Patients in the NHS National data opt out"),
                             function_name = "cohort_enforce_opt_out()",
                             function_filter = "dplyr::filter(!mrn %in% mrn_to_filter)",
                             n_prior = prior,
                             n_filtered = filtered)
  }

  if(diagnostics){
    return(list("included" = procedures_table_filtered,
                "excluded" =  procedures_table %>% dplyr::filter(!case_id %in% (procedures_table_filtered %>% pull(case_id, as_vector = FALSE))))
    )
  }else{
    return(procedures_table_filtered)
  }

}

#' Eligibility criteria: Remove procedures after the index (first) procedure of the encounter.
#'
#' A number of functions rely on a single surgical procedure per hospital encounter. This is due to the difficulties with left joins not directly linked to the  anaesthetic_csn.
#' See Data schema for exact details on how the flag is triggered.
#'
#' @inheritParams cohort_enforce_family
#' @family cohort_enforce
#' @export
#'
cohort_enforce_single_procedure <- function(procedures_table,
                                            diagnostics = FALSE,
                                            verbose){

  prior <- nrow(procedures_table)
  procedures_table_filtered <- procedures_table %>% group_by(admission_csn) %>% slice_min(operation_date) %>% ungroup()
  filtered <- prior - nrow(procedures_table_filtered)

  if(verbose){
    cli::cli_alert("Filtering out {.val {filtered}} procedures that are not index (first procedure of encounter). ")
  }

  if(add_to_eligability_criteria){
    eligibility_criteria_add(type = "Exclusion",
                             text = "Procedures subsequent to index procedure",
                             function_name = "cohort_enforce_single_procedure()",
                             function_filter = "group_by(admission_csn) %>% slice_min(operation_date) %>% ungroup()",
                             n_prior = prior,
                             n_filtered = filtered)
  }

  if(diagnostics){
    return(list("included" = procedures_table_filtered,
                "excluded" =  procedures_table %>% dplyr::filter(!case_id %in% (procedures_table_filtered %>% pull(case_id, as_vector = FALSE))))
    )
  }else{
    return(procedures_table_filtered)
  }

}




#' Cohorting: Select by set of OPCS codes
#'
#' @param cohort_name A string to group the cohort, this is useful if producing multiple cohorts with this function
#' @param include_OPCS A vector of OPSC codes (either 3 or 4 digit) to include in the dataset
#' @param exclude_OPCS A vector of OPSC codes (either 3 or 4 digit) to exclude in the dataset
#' @param enforce_coded Enforce that the surgery had been coded by the clinical coding dept
#'
#' @family cohort
#' @inheritParams cohort_complete_admissions
#' @export
#' @importFrom stringi stri_split_fixed
#' @import dplyr
#' @template setup-data
#' @examples
#' library(dplyr)
#'
#' cohort_1 <- cohort_procedure_codes(Procedures,
#'              cohort_name = "Tonsillectomy",
#'              include_OPCS = c("F34", "F35"),
#'              exclude_OPCS = "E20")
#'
cohort_procedure_codes <- function(procedures_table = Procedures,
                                   cohort_name = NA,
                                   include_OPCS = NA,
                                   exclude_OPCS = NA,
                                   add_to_eligability_criteria=TRUE,
                                   enforce_coded = TRUE,
                                   verbose=TRUE){

  if(verbose){cli::cli_h1("Starting pre-proccessing checks")}
  ## Pre-processing checks

  n_prior <- nrow(procedures_table)

  check <- cohort_enforce_procedure_coded(procedures_table, diagnostics = TRUE, add_to_eligability_criteria = FALSE)
  if(nrow(check$excluded)==0){
    #Nil uncoded patients in the dataset proceed
    if(verbose){cli::cli_alert_success("Passed check: All procedures in {.arg procedures_table} are coded")}
  }else{
    if(verbose){cli::cli_alert_warning("There are {.val {nrow(check$excluded)}} procedures in the dataset that are missing codes.")}

    if(enforce_coded){
      #Uncoded patients presnet but function allowed to remove them
      procedures_table <- cohort_enforce_procedure_coded(procedures_table, verbose=verbose)
      if(verbose){cli::cli_alert_success("Uncoded procedures removed")}
    }else{
      #Uncoded prcedures and enforce coded to false. Return danger message
        cli::cli_alert_danger("Danger! There are uncoded procedures in the table that are not being removed.\n\n Either use  {.fun cohort_enforce_procedure_coded} in the data preprocessing steps \n Or use {.arg enforce_coded = TRUE} in the arguments to let the function do this for you. \n")
      }
    }

  ## Check at least one OPSC code has been provided

  if(any(is.na(include_OPCS)) & any(is.na(exclude_OPCS))){
    cli::cli_abort("No procedures codes provided. \n \n Use the {.arg include_OPCS } or {.arg exclude_OPCS} arguments.\n\n If you want to search by other criteria (age, length of admission, etc) but not by procedure codes, check out the other cohorting functions) ")
  }

  # Check of OPSC codes
  # This is a formatting check, the future this could be linked to the Actual OPCS dictionary rather than just a regex check

  codes = c(include_OPCS, exclude_OPCS)
  for(c in codes){check_OPCS(c)}
  if(verbose){cli::cli_alert_success("Passed check: OPCS codes provided as valid")}

  # Filtering

  if(verbose){cli::cli_h1("Starting filtering")}

  # Inclusion filtering
  if(any(!is.na(include_OPCS))){

  if(verbose){cli::cli_alert_info("Applying the {.arg include_OPCS} codes {.val {include_OPCS}}")}

    #Create the string for the Grepl utilising the bar for an OR statement
  code_vector = na.omit(include_OPCS) %>% paste(collapse = "|")

  ## COMMENT: Previously we had some fancy method of passing vectors, this wasn't great as arrow can read in parallel, so the order of the rows may vary if your not using an arrange statement.

  #Extract the codes and case_ids and bring to memory
  cohort_codes <- procedures_table %>%
    select(case_id, combined_coded_opcs_procedures) %>%
    collect()

  #For each case_id assign a boolean
  cohort_codes$keep_row <- vapply(cohort_codes$combined_coded_opcs_procedures, function(sub_list) {
    # Ensure we handle NULLs or potential list structure issues
    if (is.null(sub_list)) return(FALSE)
    any(grepl(pattern = code_vector, x = unlist(sub_list, use.names = FALSE)))
  }, FUN.VALUE = logical(1))

  #create a vector of case_id where boolean ==TREU
  eligiable_case_ids <- cohort_codes %>%
    filter(keep_row) %>%
    pull(case_id)

  # Subset the table
  procedures_table <- procedures_table %>% filter(case_id %in% eligiable_case_ids)

  if(add_to_eligability_criteria){
    eligibility_criteria_add(type = "Inclusion",
                             text = paste0("Procedures with OPSC codes: ", paste(include_OPCS, collapse = ", "), ". Inclusive of 4 digit codes where only 3 digit code provided."),
                             function_name = "cohort_procedure_codes()",
                             function_filter = "%>% filter(case_id %in% eligiable_case_ids)",
                             n_prior = n_prior,
                             n_filtered =  nrow(procedures_table))
  }

  if(verbose){cli::cli_alert_success("Selecting {nrow(procedures_table)} procedures where inclusion codes were found")}
  }

  #Exclusion filtering

  if(any(!is.na(exclude_OPCS))){

    if(verbose){cli::cli_alert_info("Applying the {.arg exclude_OPCS} codes {.val {exclude_OPCS}}")}

    #Generate a subset mask
    code_vector = na.omit(exclude_OPCS) %>% paste(collapse = "|")

    #Re-Extract the codes and case_ids and bring to memory, thois should be much smaller so if efficent
    cohort_codes <- procedures_table %>%
      select(case_id, combined_coded_opcs_procedures) %>%
      collect()

    #For each case_id assign a boolean
    cohort_codes$keep_row <- vapply(cohort_codes$combined_coded_opcs_procedures, function(sub_list) {
      # Ensure we handle NULLs or potential list structure issues
      if (is.null(sub_list)) return(FALSE)
      any(grepl(pattern = code_vector, x = unlist(sub_list, use.names = FALSE)))
    }, FUN.VALUE = logical(1))

    #create a vector of case_id where boolean ==TRUE
    exclusion_case_ids <- cohort_codes %>%
      filter(keep_row) %>%
      pull(case_id)

    # Subset the table, inverted logic to above
    n_prior <- nrow(procedures_table)
    procedures_table <- procedures_table %>% filter(!case_id %in% exclusion_case_ids)

    if(add_to_eligability_criteria){
      eligibility_criteria_add(type = "Exclusion",
                               text = paste0("Procedures with OPSC codes: ", paste(include_OPCS, collapse = ", "), ". Inclusive of 4 digit codes where only 3 digit code provided."),
                               function_name = "cohort_procedure_codes()",
                               function_filter = "%>% filter(!case_id %in% exclusion_case_ids)",
                               n_prior = n_prior,
                               n_filtered =  nrow(procedures_table))
    }

    if(verbose){cli::cli_alert_success("Removing {(n_prior - nrow(procedures_table))} procedures where exclsuion codes were found")}
  }

  if(!is.na(cohort_name)){
  procedures_table$cohort_name <- cohort_name
  }

  return(procedures_table)
#End of Func
}



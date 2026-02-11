#' Cohorting: Select by set of OPCS codes
#'
#' @param cohort_name A string to group the cohort, this is useful if producing multiple cohorts with this function
#' @param include_OPCS A vector of OPSC codes (either 3 or 4 digit) to include in the dataset
#' @param exclude_OPCS A vector of OPSC codes (either 3 or 4 digit) to exclude in the dataset
#' @param enforce_coded Enforce that the surgery had been coded by the clinical coding dept
#'
#' @name cohort_enforce_family
#' @importFrom stringi stri_split_fixed
#' @import dplyr
#' @export
#' @examples
#' library(dplyr)
#' cohort_1 <- cohort_procedure_codes(Procedures, cohort_name = "Tonsillectomy", include_OPCS = c("F34", "F35"), exclude_OPCS = "E20")
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

  #Generate the split code matrix
  code_matrix <- procedures_table %>% pull(combined_coded_opcs_procedures) %>% stringi::stri_split_fixed(pattern = " | ")
  #Generate a subset mask
  code_vector = na.omit(include_OPCS) %>% paste(collapse = "|")
  inclusion_mask <- vapply(code_matrix, function(sub_list) {
    any(grepl(pattern = code_vector, x = unlist(sub_list, use.names = FALSE)))
  }, FUN.VALUE = logical(1))

  # Subset by mask
  procedures_table <- procedures_table[inclusion_mask,]

  if(add_to_eligability_criteria){
    eligibility_criteria_add(type = "Inclusion",
                             text = paste0("Procedures with OPSC codes: ", paste(include_OPCS, collapse = ", "), ". Inclusive of 4 digit codes where only 3 digit code provided."),
                             function_name = "cohort_procedure_codes()",
                             function_filter = "Complex",
                             n_prior = n_prior,
                             n_filtered =  nrow(procedures_table))
  }

  if(verbose){cli::cli_alert_success("Selecting {nrow(procedures_table)} procedures where inclusion codes were found")}
  }

  #Exclusion filtering

  if(any(!is.na(exclude_OPCS))){

    if(verbose){cli::cli_alert_info("Applying the {.arg exclude_OPCS} codes {.val {exclude_OPCS}}")}

    #Generate the split code matrix
    code_matrix <- procedures_table %>% pull(combined_coded_opcs_procedures) %>% stringi::stri_split_fixed(pattern = " | ")
    #Generate a subset mask
    code_vector = na.omit(exclude_OPCS) %>% paste(collapse = "|")
    exclusion_mask <- vapply(code_matrix, function(sub_list) {
      any(grepl(pattern = code_vector, x = unlist(sub_list, use.names = FALSE)))
    }, FUN.VALUE = logical(1))

    n_prior <- nrow(procedures_table)
    # Subset by mask
    procedures_table <- procedures_table[!exclusion_mask,]

    if(add_to_eligability_criteria){
      eligibility_criteria_add(type = "Exclusion",
                               text = paste0("Procedures with OPSC codes: ", paste(include_OPCS, collapse = ", "), ". Inclusive of 4 digit codes where only 3 digit code provided."),
                               function_name = "cohort_procedure_codes()",
                               function_filter = "Complex",
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



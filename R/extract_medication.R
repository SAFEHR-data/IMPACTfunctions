dataset <- Procedures
medication <- "morphine sulfate"
MAR <- IntraOpMar
search_column = "SimpleGenericName"


extract_medication <- function(dataset,
                               MAR = c(IntraOpMar, PostOpMar),
                               medication,
                               col = "SimpleGenericName",
                               timeframe = c("whole_encounter", "post_op", "specific"),
                               t0,
                               t1,
                               format,
                               verbose=TRUE,
                               feature_name){

  # Step 1 - Error checking

  if(verbose){
    cli::cli_h1("Starting medication feature extraction")
    cli::cli_h2("Checking data and variable quality")
  }

  # Argument checking
  if(nrow(dataset)< 1 ){cli::cli_abort("No rows in {.arg dataset} to extract, \n \n Ensure you have not filtered away")}
  if(missing(medication)){cli::cli_abort("No medication specifed {.arg medication} to search for, \n \n Please provide a medication to search for")}
  if(length(medication)>1){cli::cli_alert_info("You have provided a vector of medications, will composite them")}


  # TO add time point rules and checking

  if(verbose){cli::cli_alert_success("Arguments provided to function correct")}

  # Data quality checking

  #First do a soft check to ensure no misspelling
  med_check_soft <- search_drug_names(MAR, medication)
  if(nrow(med_check_soft) < 1){
    cli::cli_abort("The {.arg medication} provided ({.val {medication}}) isnt't in the dataset in any column, \n \n Check for spelling. Use the function {.fun search_drug_names()} to help you check the correct string.")
  }else{
    if(verbose){cli::cli_alert_success("{.val {medication}} found in the MAR")}
  }

  med_check_strict <- search_drug_names(MAR, medication, col = col, strict_search = TRUE)
  if(nrow(med_check_strict) < 1){
    cli::cli_abort("The {.arg medication} provided ({.val {medication}}) didn't have any exact matched in the {.arg col} = {.val {col}},
                   \n \nThere were some rows of the MAR that matched! Use the function {.fun search_drug_names} to help you check the variables to pass or see the table below:")
    print(med_check_soft)
  }else{
    if(verbose){cli::cli_alert_success("{.val {medication}} matched values in the column {.val {col}}")}
  }









  # Step 2 - Subsetting

  # 2a - Subset for medication

  # 2b - Subsetting for timeframe





  # Step 3 - Calulating





}



#' Helper function to search the MAR
#'
#' There is often inconsistency in how a medication name, formulation and group is stored. This may represent old formulations being removed, or not listed correctly. This functions helps you to understand what your search string is doing and which medications may be captures for your feature extraction.
#'
#' @param MAR The arrow table of the MAR of interest. Pick either `IntraOpMar` or `PostOpMar`
#' @param medication String of the medication to search. It is case insensitive for the soft search
#' @param col Defaults to all three columns to do a soft search (return rows where there is a match in any). Specify one of `"medication_name", "generic_name", "simple_generic_name"` to do a hard search only in that column.
#' @param verbose Print output
#'
#' @return
#' @export
#'
#' @examples
#' search_drug_names(IntraOpMar, "morphine") #Notice that we capture Diamorph as well
#' search_drug_names(IntraOpMar, "morphine sulfate") # We have now refined our string to remove diamorph but we have also lost the `Morphine 10mg for recovery`
#' search_drug_names(IntraOpMar, "MORPHINE") #Notice that the search is case insenstive becuase capitilisation is not consistent
#'
#' search_drug_names(IntraOpMar, "fentanyl")
#' search_drug_names(IntraOpMar, "fentanyl", col="simple_generic_name")
#' # We can use the strict search to emulate the medications search functions
#' search_drug_names(IntraOpMar, "fentanyl", col="simple_generic_name", strict_search=TRUE)
search_drug_names <- function(MAR = c(IntraOpMar, PostOpMar),
                              medication,
                              strict_search = FALSE,
                              col = c("medication_name", "generic_name", "simple_generic_name"),
                              verbose=TRUE){

  if(verbose){
  cli::cli_h1("Starting drug search tool")
  cli::cli_inform("Use this function to help understand the names that drugs use within the the diffrent naming columns")
  }

  if(strict_search & missing(col)){
    Possible_col = c("medication_name", "generic_name", "simple_generic_name")
    cli::cli_abort("You gave tried to do a strict search but not included a column to search in. \n \n Provide the argument {.arg col} with one of following column names: {.val {Possible_col}} ")
  }

  # Generate the summary for all columns
  summary <- MAR %>% group_by(medication_name,generic_name, simple_generic_name ) %>% summarise(rows = n(), .groups = "drop") %>% collect()

  if(!strict_search){
  # If doing soft search then do OR for the grep
    if(all(col ==  c("medication_name", "generic_name", "simple_generic_name"))){
    summary <- summary %>%
      filter(
        grepl(medication, medication_name, ignore.case = TRUE) |
        grepl(medication, generic_name, ignore.case = TRUE) |
        grepl(medication, simple_generic_name, ignore.case = TRUE)
      )
    } else{
      # Else doing a single column soft search
     summary <- summary %>%
        filter(
          grepl(medication, .data[[col]], ignore.case = TRUE)
        )
    }
  }else{
    # Strict search
    summary <- summary %>%
      filter(.data[[col]] == medication)
  }
  return(summary)
}






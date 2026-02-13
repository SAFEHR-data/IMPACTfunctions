dataset <- Procedures
medication <- "morphine"
MAR <- IntraOpMar
search_column = "SimpleGenericName"


extract_medication <- function(dataset,
                               MAR = c(IntraOpMar, PostOpMar),
                               medication,
                               search_column = "SimpleGenericName",
                               timeframe = c("whole_encounter", "post_op", "specific"),
                               t0,
                               t1,
                               format,
                               verbose=TRUE,
                               feature_name){

  # Step 1 - Error checking

  # Argument checking

  if(nrow(dataset)< 1 ){cli::cli_abort("No rows in {.arg dataset} to extract, \n \n Ensure you have not filtered away")}
  if(missing(medication)){cli::cli_abort("No medication specifed {.arg medication} to search for, \n \n Please provide a medication to search for")}

  # Data quality checking


  med_check_soft <- search_drug_names(MAR, "morpggghine")
  if(nrow(med_check)< 1){
    cli::cli_abort("The {.arg medication} provided ({.val {medication}}) isnt't in the dataset in any column, \n \n Check for spelling. Use the function {.fun search_drug_names()} to help you check the correct string.")
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
#' @param drug_name_string String of the medication to search. It is case insensitive for the soft search
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
                              drug_name_string,
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
        grepl(drug_name_string, medication_name, ignore.case = TRUE) |
        grepl(drug_name_string, generic_name, ignore.case = TRUE) |
        grepl(drug_name_string, simple_generic_name, ignore.case = TRUE)
      )
    } else{
      # Else doing a single column soft search
     summary <- summary %>%
        filter(
          grepl(drug_name_string, .data[[col]], ignore.case = TRUE)
        )
    }
  }else{
    # Strict search
    summary <- summary %>%
      filter(.data[[col]] == drug_name_string)
  }
  return(summary)
}






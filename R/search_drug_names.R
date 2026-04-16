#' Helper function to search the MAR
#'
#' There is often inconsistency in how a medication name, formulation and group is stored. This may represent old formulations being removed, or not listed correctly. This functions helps you to understand what your search string is doing and which medications may be captures for your feature extraction.
#'
#' @param MAR The arrow table of the MAR of interest. Pick either `IntraOpMar` or `HospitalMar`
#' @param medication String of the medication to search. It is case insensitive for the soft search
#' @param strict_search TO do exact string and column matching. Useful for testing arguments for `extract_medication()` function
#' @param search_column Defaults to all three columns to do a soft search (return rows where there is a match in any). Specify one of `"medication_name", "generic_name", "simple_generic_name"` to do a hard search only in that column.
#' @param verbose Print output
#'
#' @return A dataframe summarising the medications mathcing the search criteria and their freqency.
#' @export
#' @template setup-data
#'
#' @examples
#' search_drug_names(IntraOpMar, "morphine") #Notice that we capture Diamorph as well
#' search_drug_names(IntraOpMar, "morphine sulfate") # We have now refined our string to remove diamorph but we have also lost the `Morphine 10mg for recovery`
#' search_drug_names(IntraOpMar, "MORPHINE") #Notice that the search is case insenstive becuase capitilisation is not consistent
#'
#' search_drug_names(IntraOpMar, "fentanyl")
#' search_drug_names(IntraOpMar, "fentanyl", search_column="simple_generic_name")
#' # We can use the strict search to emulate the medications search functions
#' search_drug_names(IntraOpMar, "fentanyl", search_column="simple_generic_name", strict_search=TRUE)
search_drug_names <- function(MAR = c(IntraOpMar, HospitalMar),
                              medication,
                              strict_search = FALSE,
                              search_column = c("medication_name", "generic_name", "simple_generic_name"),
                              verbose=TRUE){

  if(verbose){
    cli::cli_h1("Starting drug search tool")
    cli::cli_inform("Use this function to help understand the names that drugs use within the the diffrent naming columns")
  }

  # If multiple strings attached then concat with bar for OR statement in GREPL
  if(length(medication)> 1){
    medication <- paste0(medication, collapse = "|")
  }

  if(strict_search & missing(search_column)){
    Possible_col = c("medication_name", "generic_name", "simple_generic_name")
    cli::cli_abort("You gave tried to do a strict search but not included a column to search in. \n \n Provide the argument {.arg col} with one of following column names: {.val {Possible_col}} ")
  }

  # Generate the summary for all columns
  summary <- MAR %>% group_by(medication_name,generic_name, simple_generic_name ) %>% summarise(rows = n(), .groups = "drop") %>% collect()

  if(!strict_search){
    # If doing soft search then do OR for the grep
    if(all(search_column %in%  c("medication_name", "generic_name", "simple_generic_name"))){
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
          grepl(medication, .data[[search_column]], ignore.case = TRUE)
        )
    }
  }else{
    # Strict search
    summary <- summary %>%
      filter(.data[[search_column]] == medication)
  }
  return(summary)
}

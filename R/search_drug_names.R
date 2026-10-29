#' Helper function to search the MAR
#'
#'  There is often inconsistency in how a medication name, formulation and group is stored.
#'  This may represent old formulations being removed, or not listed correctly.
#'  This functions helps you to understand what your search string is doing and
#'   which medications may be captures for your feature extraction.
#'
#' @param MAR The arrow table of the MAR of interest. Pick either `IntraOpMar` or `HospitalMar`
#' @param medication String of the medication to search. It is case insensitive for the soft search
#' @param strict_search TO do exact string and column matching. Useful for testing arguments for `extract_medication()` function
#' @param search_column Defaults to all three columns to do a soft search (return rows where there is a match in any). Specify one of `"medication_name", "generic_name", "simple_generic_name", "pharmaceutical_class", pharmaceutical_subclass"` to do a hard search only in that column. Note this is *only* enforced when `strict_search = TRUE`; for a soft search `search_column` is informational only and matches from every column are still returned (so a typo in `search_column`, or a term that happens to match a different column, is still surfaced to help you debug your search) - if the term doesn't match within the `search_column` you named, a warning tells you so before showing the broader results.
#' @param verbose Print output
#'
#' @return A dataframe summarising the medications matching the search criteria and their frequency.
#' @export
#' @template setup-data
#'
#' @examples
#' search_drug_names(HospitalMar, "morphine") #Notice that we capture Diamorph as well
#' search_drug_names(HospitalMar, "morphine sulfate") # We have now refined our string to
#' # remove diamorph but we have also lost the `Morphine 10mg for recovery`
#' search_drug_names(HospitalMar, "MORPHINE") #Notice that the search is case
#' # insenstive becuase capitilisation is not consistent
#'
#' search_drug_names(HospitalMar, "fentanyl")
#' search_drug_names(HospitalMar, "fentanyl", search_column="simple_generic_name")
#' # We can use the strict search to emulate the medications search functions
#' search_drug_names(HospitalMar, "fentanyl", search_column="simple_generic_name", strict_search=TRUE)
search_drug_names <- function(MAR = HospitalMar,
                              medication,
                              strict_search = FALSE,
                              search_column = c("medication_name", "generic_name", "simple_generic_name"),
                              verbose=TRUE){

  if(verbose){
    cli::cli_h1("Starting drug search tool")
    cli::cli_inform("Use this function to help understand the names that drugs use within the the diffrent naming columns")
  }


  if(strict_search & missing(search_column)){
    Possible_col = c("medication_name", "generic_name", "simple_generic_name")
    cli::cli_abort("You gave tried to do a strict search but not included a column to search in. \n \n Provide the argument {.arg search_column} with one of following column names: {.val {Possible_col}} ")
  }

  # Generate the summary for all columns
  summary <- MAR %>% group_by(medication_name,generic_name, simple_generic_name, pharmaceutical_class, pharmaceutical_subclass) %>% summarise(rows = n(), .groups = "drop") %>% collect()

  if(!strict_search){
    # If multiple strings attached then concat with bar for OR statement in GREPL
    if(length(medication)> 1){
      medication <- paste0(medication, collapse = "|")
    }

    if(all(search_column %in%  c("medication_name", "generic_name", "simple_generic_name", "pharmaceutical_class", "pharmaceutical_subclass"))){

      # Soft search always looks across all 5 columns regardless of search_column
      # (this is intentional - it helps surface typos/variants a caller might
      # otherwise miss). If the caller did specify a search_column, let them
      # know when their term didn't actually match within it, so they're not
      # misled about which column their result came from.
      if(!missing(search_column)){
        matched_in_search_column <- any(vapply(search_column, function(col){
          any(grepl(medication, summary[[col]], ignore.case = TRUE))
        }, logical(1)))

        matched_anywhere <- any(
          grepl(medication, summary$medication_name, ignore.case = TRUE) |
          grepl(medication, summary$generic_name, ignore.case = TRUE) |
          grepl(medication, summary$simple_generic_name, ignore.case = TRUE) |
          grepl(medication, summary$pharmaceutical_class, ignore.case = TRUE) |
          grepl(medication, summary$pharmaceutical_subclass, ignore.case = TRUE)
        )

        if(matched_anywhere && !matched_in_search_column){
          cli::cli_alert_warning("No match for {.val {medication}} found in the specified {.arg search_column} ({.val {search_column}}). Soft search still returns matches from all columns below - set {.arg strict_search = TRUE} to enforce searching only within {.arg search_column}.")
        }
      }

      summary <- summary %>%
        filter(
            grepl(medication, medication_name, ignore.case = TRUE) |
            grepl(medication, generic_name, ignore.case = TRUE) |
            grepl(medication, simple_generic_name, ignore.case = TRUE) |
            grepl(medication, pharmaceutical_class, ignore.case = TRUE) |
            grepl(medication, pharmaceutical_subclass, ignore.case = TRUE)
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
      filter(.data[[search_column]] %in% medication)
  }

  if(nrow(summary)==0){
    cli::cli_alert_danger("No rows returned, try different search settings")
  }

  return(summary)
}

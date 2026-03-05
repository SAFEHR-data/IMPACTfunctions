#' Check eligibility criteria dataframe
#'
#' @return A dataframe `eligibility_criteria` to the global environment if it doesn't yet exist.
#' @export
#' @template setup-data
#' @examples
#' # Not run
eligibility_criteria_check <- function(){
  if(exists("eligibility_criteria")){
    #the dataframe already exists, do nothing.
  }else{
    #dataframe doesn't exist, initialise it
    eligibility_criteria <<- data.frame(type = character(),
                                        text = character(),
                                        function_name = character(),
                                        function_filter = character(),
                                        n_prior = integer(),
                                        n_filtered = integer())
  }

  # Error checking

  if(any(duplicated(eligibility_criteria$function_name))){
    cli::cli_alert_warning("Caution: It looks like you have applied the same eligability criteria twice.\nThis may occur if you are building and testing your cohorting functions. \n \nRun the whole cohorting functions from the top to check this warning disappears ")
  }

}

#' Add a new criteria to the `eligibility_criteria` dataframe
#'
#' @param type If the row is an rule of inclusion or exclusion.
#' @param text String description of the exclusion criteria.
#' @param function_name The name of the `cohort_enforce_...` level function that did the filtering.
#' @param function_filter The specific line of code used.
#' @param n_prior The number of procedure prior to applying the criterion.
#' @param n_filtered The number of procedures removed as a result of the criterion.
#'
#' @return A dataframe `eligibility_criteria` to the global environment with the updated row.
#' @export
#'
#' @examples
#' # Not run
eligibility_criteria_add <- function(type,
                                     text,
                                     function_name,
                                     function_filter,
                                     n_prior,
                                     n_filtered){

  eligibility_criteria_check()

  new_row = data.frame(type = type,
                       text = text,
                       function_name = function_name,
                       function_filter = function_filter,
                       n_prior = n_prior,
                       n_filtered = n_filtered)

  eligibility_criteria <<- rbind(eligibility_criteria, new_row)
}

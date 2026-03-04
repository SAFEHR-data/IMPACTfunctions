dataset <- Procedures
medication <- c("morphine sulfate", "propofol")
MAR <- IntraOpMar
search_column = "simple_generic_name"


#' Title
#'
#' To search the relevant MAR to return features/variables related to the medications
#'
#'\section{Search time windows}
#'
#' The default behavior of the function is to return the metric of all of the medications on the MAR of intrest, for the admission of intrest. We may want to search with specific timepoints to establise the medication use during a sepcifed periood such as Post op.
#'
#' There are 3 easy to use time frames by default but they can be customised further.
#' If *timeframe* = *"whole_mar* (**The default behavior**) then it will return the feature of the whole MAR for the patients admission.
#' If *timeframe* = *pre_op* then it will return the sections of the Post-op MAR (Even though it is called post-op it is really the ward MAR which is all medications given outside of theatre) between the admission datetime and the operation datetime.
#' If *timeframe* = *post_op* then it will return the sections of the Post-op MAR between the admission operation datetime and the discharge datetime.
#' If *timeframe* = *custom* then you may supply the vector of values for t0 and either t1 or 1_int to set a custom time window.
#'
#'
#' @param dataset The procedures set of which to search across.
#' @param MAR The medication administration record (MAR) to search across, Can be either Intra-Op or Post-Op MAR.
#' @param medication Name of medication to search for. Can be single medication or vector
#' @param search_column The column which to search the medication string against. See help above.
#' @param timeframe A set of time frames of which to search for, See help above for options
#' @param t0 If subsetting by time, what is the start time to filter by
#' @param t_int If subsetting by time, the time interval from t0 to search from
#' @param t1 If subsetting by time, what is the end time to filter by
#' @param format
#' @param verbose
#' @param feature_name
#'
#' @return
#' @export
#'
#' @examples
extract_medication <- function(dataset,
                               MAR = c(IntraOpMar, PostOpMar),
                               medication,
                               search_column = "simple_generic_name",
                               timeframe = c("whole_mar", "pre_op", "post_op", "custom"),
                               t0,
                               t_int,
                               t1,
                               format,
                               verbose=TRUE,
                               feature_name){

  # Step 1 - Error checking

  if(verbose){
    cli::cli_h1("Starting medication feature extraction")
    cli::cli_h2("Checking data and variable quality")
  }

  ## Argument checking
  if(nrow(dataset)< 1 ){
    cli::cli_abort("No rows in {.arg dataset} to extract, \n \n Ensure you have not filtered away all patients")}
  if(missing(medication)){
    cli::cli_abort("No medication specifed {.arg medication} to search for, \n \n Please provide a medication to search for")}

  ## Timeframe checking
  if(identical(MAR, IntraOpMar)){
    if(timeframe %in% c("pre_op", "post_op")){cli::cli_abort("Requested {.val timeframe} of {.val {timeframe}} but the {.arg MAR} supplied is the {.val IntraOpMar}. \n \n Please either change the MAR or use {.arg timeframe} = {.val 'whole_mar'} or {.val 'specific'}")}
    if(verbose){cli::cli_alert_success("Searching in the Intra-Op MAR")}

    #Set the return vector against the anaesthesia record
    ## NOTE: this vector is intentionally not exposed to the arguments, its seems the custom use of this may be too advanced.
    csn_vector_name <- "anaesthesia_csn"

  }else if(identical(MAR, PostOpMar)){
    if(verbose){cli::cli_alert_success("Searching in the Post-Op MAR")}
    csn_vector_name <- "admission_csn"

  }else{
    cli::cli_abort("Unknown MAR passed. TO use this function you should pass either {.val IntraOpMar} or {.val PostOpMar} to the argument {.arg MAR}. \n \n If you would like to use a different MAR (EG a cimmunity MAR) please  {.href [click here](https://github.com/SAFEHR-data/IMPACTfunctions/issues)} to open a github issue. ")
  }

  ## Assigning time points if used timeframe argument
  if(missing(timeframe)){
    if(verbose){cli::cli_alert_success("No timeframe provided, searching whole MAR per encounter.")}
    timeframe <- "whole_mar"
  }else if(timeframe == "whole_mar"){
    if(verbose){cli::cli_alert_success("{.arg timeframe} set to {.val {timeframe}}, searching whole MAR per encounter.")}
  }else if(timeframe == "pre_op"){
    if(verbose){cli::cli_alert_success("{.arg timeframe} set to {.val {timeframe}}, searching only pre-operative period.")}
    t0 <- dataset %>% pull(admission_date)
    t1 <- dataset %>% pull(operation_date)
  }else if(timeframe == "post_op"){
    if(verbose){cli::cli_alert_success("{.arg timeframe} set to {.val {timeframe}}, searching only post-operative period.")}
    t0 <- dataset %>% pull(operation_date)
    t1 <- dataset %>% pull(discharge_date)
  }else if(timeframe == "custom"){
    if(verbose){cli::cli_alert_success("{.arg timeframe} set to {.val {timeframe}}, searching only post-operative period.")}
    #If timeframe custom, ensure propry t0,t1 and t_int set.
    if(!missing(t_int) & !missing_t_1){
      cli::cli_abort("You have tried to supply both a time interval ({.arg t_int}) and a end time ({.arg t_1}). \n \n Supply only one end time variable")
    }else if(!missing(t0) & (mising(t1) & missing(t_int))){
    cli::cli_alert_danger("CAUTION: you have supplied a start time to subset by but not an end time. Will return from the {.arg t0} timepoint onwards.")
    }
  }

  duplicate_rows <- any(dataset %>% pull(admission_csn) %>% duplicated())
  if(duplicate_rows & identical(MAR, IntraOpMar)){
    cli:cli_alert_danger("CAUTION: There are multiple procedures within the dataset provided, this may cause errors as you are searching intra-op MAR's. This is because the variable returned will be a sum/avg across mutiple anaesthetic MAR's. Suggest enforcing filter with one procedure per encounter")
  }else if(duplicate_rows & identical(MAR, PostOpMar)){
    cli:cli_alert_danger("CAUTION: There are multiple procedures within the dataset provided, this may cause errors as you are searching post-op MAR's. Ajny timeframe filtering may return varying windows. Variable may be duplicated across these duplicated rows.")
  }

  if(verbose){cli::cli_alert_success("Arguments provided to function correct")}

  # Data quality checking

  #First do a soft check to ensure no misspelling
  med_check_soft <- search_drug_names(MAR, medication)
  if(nrow(med_check_soft) < 1){
    cli::cli_abort("The {.arg medication} provided ({.val {medication}}) isnt't in the dataset in any column, \n \n Check for spelling. Use the function {.fun search_drug_names()} to help you check the correct string.")
  }else{
    if(verbose){cli::cli_alert_success("{.val {medication}} found in the MAR")}
  }

  med_check_strict <- search_drug_names(MAR, medication, search_column = search_column, strict_search = TRUE)
  if(nrow(med_check_strict) < 1){
    cli::cli_abort("The {.arg medication} provided ({.val {medication}}) didn't have any exact matched in the {.arg search_column} = {.val {search_column}},
                   \n \nThere were some rows of the MAR that matched! Use the function {.fun search_drug_names} to help you check the variables to pass or see the table below:")
    print(med_check_soft)
  }else{
    if(verbose){cli::cli_alert_success("{.val {medication}} matched values in the column {.val {search_column}}")}
  }


  # Step 2 - Subsetting


  # We concatenate the medications strings here to allow the search functions above to recive a vector.
  if(length(medication)>1){
    if(verbose){cli::cli_alert_info("You have provided a vector of medications, will do an \"or\" search to return any matching rows.")}
    medication <- paste0(medication, collapse = "|")
  }

  # 2a - Subset for medication

  subset <- MAR %>% filter(get({{search_column}}) %in% medication)

  # 2b - Establishing the value to return against.
  ## Uses the vector that is set above given the type of MAR you are extracting against.

  csv_vector <- dataset %>% pull(.data[[csn_vector_name]]) %>% na.omit()
  subset <- subset %>% filter(.data[[csn_vector_name]] %in% csv_vector)

  # 2c - Subsetting for the patients in the dataset



  dataset %>%

    ## Need to work out this subsettign by patietn,
    ##Issue of if you search for post-op mar then return the variable vector against the admission CSN but if searching intra-op mar then to return agaisnt the aanaesthetis/case CSN
    ## Issue of how to then extrac the variable timepoints.
    ##Could use a case_When? or an embedded lambda function or a left join

  # 2d - Subsetting for timeframe

 if(timeframe == "whole_mar"){dwhit
   NULL
 }else{
   #We need unique row indexes to do our filtering. Arrow doesnt support row indexing (by its nature of parrallel reading, etc). We could try a to_duckdb() but we will just call to memory as the subset should be small by now.
    subset <- subset %>% collect()
    subset <- subset %>% filter(administration_date_time > t0) %>% filter(administration_date_time < t1)
 }

  subset %>% collect()




  # Step 3 - Calculating the feature vector





}



#' Helper function to search the MAR
#'
#' There is often inconsistency in how a medication name, formulation and group is stored. This may represent old formulations being removed, or not listed correctly. This functions helps you to understand what your search string is doing and which medications may be captures for your feature extraction.
#'
#' @param MAR The arrow table of the MAR of interest. Pick either `IntraOpMar` or `PostOpMar`
#' @param medication String of the medication to search. It is case insensitive for the soft search
#' @param search_column Defaults to all three columns to do a soft search (return rows where there is a match in any). Specify one of `"medication_name", "generic_name", "simple_generic_name"` to do a hard search only in that column.
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
#' search_drug_names(IntraOpMar, "fentanyl", search_column="simple_generic_name")
#' # We can use the strict search to emulate the medications search functions
#' search_drug_names(IntraOpMar, "fentanyl", search_column="simple_generic_name", strict_search=TRUE)
search_drug_names <- function(MAR = c(IntraOpMar, PostOpMar),
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






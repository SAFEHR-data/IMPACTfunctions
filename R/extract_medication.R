#' Extract Medication administrations
#'
#' To search the MAR to return features/variables related to the medications.
#'
#'  MAR Source
#'
#' There are 3 places medications can be recorded as administered (MAR) in the hospital:
#' 1) `mar_source = "Hospital Encounter"` -  The general MAR where the majority of medications are recorded.
#' 2) `mar_source = "Anaesthesia"` - The anaesthetic MAR that contains the medications recorded by the anaesthetist.
#' 3) `mar_source = "Operation"` - The operation MAR that contains medications recorded by the scrub team (often injections drawn up by the scrub team and given by the surgeon).
#'
#' Generally you want to use `mar_source = NA` which will not apply any filtering.
#'
#' To select a specific MAR source see the parameter options below. Ensure you validate the specific medication used as documentation practices may vary (eg being double recorded)
#'
#' Search time windows
#'
#' The default behaviour of the function is to return the metric of all of the medications on the MAR, for the admission of interest. We may want to search with specific time points to establish the medication use during a specified period such as 3hrs Post op.
#'
#' There are 3 easy to use time frames by default but they can be customised further.
#' If *timeframe* = *"whole_mar* (**The default behaviour**) then it will return the feature of the whole MAR for the patients admission.
#' If *timeframe* = *pre_op* then it will return the section of the MAR between the admission datetime and the operation datetime.
#' If *timeframe* = *post_op* then it will return the section of the Post-op MAR between the operation datetime and the discharge datetime.
#' If *timeframe* = *custom* then you may supply the vector of values for t0 and either t1 or 1_int to set a custom time window.
#'
#'
#' @param dataset The procedures set of which to search across.
#' @param MAR The MAR to extract from. Will default to `HospitalMar`.
#' @param mar_source The source MAR to filter from to filter across both then leave this as `NA`. See section above. Multiple sources can be combined (`mar_source = c("Anaesthesia", "Operation"))`.
#' @param medication Name of medication to search for. Can be single medication or vector. To return all medications fitting the other filter criteria then set to NULL.
#' @param search_column The column which to search the medication string against. See help above.
#' @param check_search Boolean, Check if the medications and search column provided are valid.
#' @param timeframe A set of time frames of which to search for, See help above for options
#' @param t0 If subsetting by time, a column name of a datetime column to use. This column can be from any table in the dataset.
#' @param t_int If subsetting by time, the time interval from t0 to search from.
#' @param t1 If subsetting by time, what is the end time to filter by
#' @param filter_route The strings of the routes to be filtered by.
#' @param route_column The column which to apply the route strings to filter upon. Select from **"route"** or **"simple_route"**.
#' @param verbose Print the stwps as the function runs? Helpful for setting up and debugging
#' @param apply_preproccess_MAR Allow the function to do the preprocessing for you. Turn to **FALSE** if you have used `preproccess_MAR()` in your workflow already.
#'
#' @return A arrow query of the subset of medications of intrest.
#' @export
#' @template setup-data
#' @examples
#'
#' #dataset <- cohort_complete_admissions(Procedures, add_to_eligability_criteria = FALSE)
#' #extract_medication(dataset, MAR = IntraOpMAR, ...)
#'
#'
extract_medication <- function(dataset,
                               MAR = HospitalMar,
                               mar_source = NA,
                               medication = NA,
                               search_column,
                               check_search = TRUE,
                               timeframe = c("whole_mar", "pre_op", "post_op", "custom"),
                               t0 = NA,
                               t_int = NA,
                               t1 = NA,
                               filter_route,
                               route_column,
                               verbose=TRUE,
                               apply_preproccess_MAR = TRUE
                               ){

  # Step 1 - Error checking

  if(verbose){
    cli::cli_h1("Starting medication feature extraction")
    cli::cli_h2("Checking data and variable quality")
  }

  allowed_mar_source <- c("Hospital Encounter", "Anaesthesia", "Operation")

  if(any(!is.na(mar_source))){
    if (!any(mar_source %in% allowed_mar_source)) cli::cli_abort("You have provided {.arg mar_source} = {.val {mar_source}}. {.arg mar_source} must contain at least one of: {.val {allowed_mar_source}}.")
    if (!all(mar_source %in% allowed_mar_source)) cli::cli_abort("You have provided {.arg mar_source} = {.val {mar_source}}. {.arg mar_source} contains invalid values. Only {.val {allowed_mar_source}} are permitted, check spelling.")
  }
  # We concatenate the medications strings here to allow the search functions above to recive a vector.
  if(length(medication)>1){
    if(verbose){cli::cli_alert_info("You have provided a vector of medications, will do an \"or\" search to return any matching rows.")}
    medication_vector <- paste0(medication, collapse = "|")
  }

  ## Argument checking
  if(nrow(compute(dataset)) < 1 ){
    cli::cli_abort("No rows in {.arg dataset} to extract, \n \n Ensure you have not filtered away all patients")}

  if(all(is.null(medication))){
    if(verbose){cli::cli_alert_info("No medications given to {.arg medication}, Returning all medications.")}
    check_search <- FALSE
    #Turn the meds search off so not to throw an error.
  }else if(all(is.na(medication))){
    cli::cli_abort("No medication specifed {.arg medication} to search for. Please provide a medication to search for. \n\n If you intended to return all meds then set {.arg medication} = {.val NULL}. ")
  }


  ## Assigning time points if used timeframe argument
  if(missing(timeframe)){
    if(verbose){cli::cli_alert_success("No timeframe provided, searching whole MAR per encounter.")}
    timeframe <- "whole_mar"
  }else if(timeframe == "whole_mar"){
    if(verbose){cli::cli_alert_success("{.arg timeframe} set to {.val {timeframe}}, searching whole MAR per encounter.")}
  }else if(timeframe == "pre_op"){
    if(verbose){cli::cli_alert_success("{.arg timeframe} set to {.val {timeframe}}, searching only pre-operative period.")}
    t0_col <- dataset %>% select(admission_csn,anaesthesia_csn, t0 = admission_date)
    t1_col <- dataset %>% select(admission_csn,anaesthesia_csn,, t1 = operation_date)
    timeframe_df <- inner_join(t0_col, t1_col) %>% collect()
  }else if(timeframe == "post_op"){
    if(verbose){cli::cli_alert_success("{.arg timeframe} set to {.val {timeframe}}, searching only post-operative period.")}
    t0_col <- dataset %>% select(admission_csn,anaesthesia_csn,, t0 = operation_date)
    t1_col <- dataset %>% select(admission_csn,anaesthesia_csn,, t1 = discharge_date)
    timeframe_df <- inner_join(t0_col, t1_col) %>% collect()
  }else if(timeframe == "custom"){
    ## Error checking that the timestamps are set properly
    if(verbose){cli::cli_alert_success("{.arg timeframe} set to {.val {timeframe}}, searching custom time period.")}
    #If timeframe custom, ensure propry t0,t1 and t_int set.
    if(!is.na(t_int) & !is.na(t1)){
      cli::cli_abort("You have tried to supply both a time interval ({.arg t_int}) and a end time ({.arg t_1}). \n \n Supply only one end time variable")
    }else if( is.na(t0) & is.na(t1) & is.na(t_int) ){
    cli::cli_alert_danger("CAUTION: you have supplied a start time to subset by but not an end time. Will return from the {.arg t0} timepoint onwards.")
    }

    #Assuming test above are passed then we can check if have passed a t_int and then calcualte the t_1
    if(!is.na(t_int)){

      timeframe_df <- dataset %>%
        select(admission_csn,anaesthesia_csn, t0 = all_of(t0)) %>%
        collect() %>%
        mutate(
          # Perform R-side arithmetic, then force UTC to match the Arrow schema
          t1 = lubridate::with_tz(t0 + t_int, tzone = "Etc/UTC")
        )

    }else if(!is.na(t1)){
      timeframe_df <- dataset %>% select(admission_csn,anaesthesia_csn,,  t0 = all_of(t0), t1 = all_of(t1))
    }else{
      cli::cli_abort("Hmm, you should not have been able to trigger this error message. Please {.href [leave a issue on the github](https://github.com/SAFEHR-data/IMPACTfunctions/issues)} called: \"Check timeframe logic of extract_medications()\ \n \n ")
      }

  }else{
    cli::cli_abort("Incorrect value supplied to {.arg timeframe}. See {.code ?extract_medication} for assistance. ")

  }


  if(verbose){cli::cli_alert_success("Arguments provided to function correct")}

  # Data quality checking

  if(apply_preproccess_MAR){
    MAR <- preproccess_MAR(MAR)
  }


  if(check_search){
  #First do a soft check to ensure no misspelling
  med_check_soft <- search_drug_names(MAR, medication, verbose = FALSE)
  if(nrow(med_check_soft) < 1){
    cli::cli_abort("The {.arg medication} provided ({.val {medication}}) isnt't in the dataset in any column, \n \n Check for spelling. Use the function {.fun search_drug_names()} to help you check the correct string.")
  }else{
    if(verbose){cli::cli_alert_success("Soft search: The string {.val {medication}} was found in the MAR")}
  }

  if(missing(search_column)){
    if(verbose){cli::cli_alert_info("No {.arg search_column} specifed.\n Defaulting to strict search in {.val {search_column}} ")}
    search_column = "simple_generic_name"
    }

  med_check_strict <- search_drug_names(MAR, medication, search_column = search_column, strict_search = TRUE, verbose = FALSE)
  if(nrow(med_check_strict) < 1){
    cli::cli_abort("The {.arg medication} provided ({.val {medication}}) didn't have any exact matched in the {.arg search_column} = {.val {search_column}},
                   \n \nThere were some rows of the MAR that matched! Use the function {.fun search_drug_names} to help you check the variables to pass or see the table below:")
    print(med_check_soft)
  }else{
    if(verbose){cli::cli_alert_success("Strict search: {.val {medication}} matched exct values in the column {.val {search_column}}")}
  }
  }

  # Step 2 - Subsetting


  if(all(!is.na(mar_source))) {
    subsets <- list()
    if ("Hospital Encounter"        %in% mar_source){
      subsets[["ward"]]        <- MAR %>% filter(encounter_type == "Hospital Encounter")
      if(verbose){cli::cli_alert_success("Removing medications given on the anesthetic or operation MAR")}
    }
    if ("Anaesthesia" %in% mar_source){
      subsets[["anaesthetic"]] <- MAR %>%filter(encounter_type == "Anaesthesia")
      if(verbose){cli::cli_alert_success("Selecting only medications given on the anesthetic MAR")}
    }
    if ("Operation"   %in% mar_source){
      subsets[["operation"]]  <- MAR %>% filter(encounter_type == "Operation")
      if(verbose){cli::cli_alert_success("Selecting only medications given on the operation MAR")}
    }
    MAR <- purrr::reduce(subsets, union_all)
  }else{
    if(verbose){cli::cli_alert_success("Searching all medications administered during inpatient stay")}
  }



  # 2a - Subset for medication

  if(is.null(medication)){
    subset <- MAR
  }else{
    subset <- MAR %>% filter(get({{search_column}}) %in% medication)
  }

  # 2b - Subsetting for the patients in the dataset
  mrn_vector <- dataset %>% pull(mrn) %>% na.omit()
  subset <- subset %>% filter(mrn %in% mrn_vector)
  if(verbose){cli::cli_alert_success("Removing MAR from patients not in dataset.")}


  # 2c - Subsetting for timeframe

 if(timeframe == "whole_mar"){
   df <- subset
 }else{
   #We need unique row indexes to do our filtering. Arrow doesnt support row indexing (by its nature of parrallel reading, etc).
   ##Instead we pull our timepoints in the logic section above to make a fixed dataset object where the t0 and t1 times are on the same row as the CSN.

   ## Rgrettably the arrow dpplyr doesnt have the kernals to deal with timeframe periods (as generated by t0 + t_int), thus we now call into memory.
    df <- left_join(subset, timeframe_df, copy = TRUE, relationship = "one-to-many")
    df <- df %>% filter(administration_date_time > t0) %>% filter(administration_date_time < t1)
    if(verbose){cli::cli_alert_success("Removing Medications given outside of specified timeframe")}
 }

  # 2d filtering by route

  if(!missing(filter_route)){
    if(missing(route_column)){
      cli::cli_abort("You have tried to filter by route but have not included a route column to filter against. \n \n Set {.arg filter_column} to one of {.val route} or {.val route_simple}")
    }

    if(verbose){cli::cli_alert_info("Filtering to only include medications where the route is {.val {filter_route}} in the column {.val {route_column}}.")}

    df <- df %>% filter(.data[[route_column]] %in% filter_route)

    }

   #return the query
    return(df)


}

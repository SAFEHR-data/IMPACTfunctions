#' Title
#'
#' To search the relevant MAR to return features/variables related to the medications
#'
#' Search time windows
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
                               MAR = c(IntraOpMar, PostOpMar),
                               medication,
                               search_column = "simple_generic_name",
                               check_search = TRUE,
                               timeframe = c("whole_mar", "pre_op", "post_op", "custom"),
                               t0,
                               t_int,
                               t1,
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

  ## Argument checking
  if(nrow(compute(dataset)) < 1 ){
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
    cli::cli_abort("Unknown MAR passed. TO use this function you should pass either {.val IntraOpMar} or {.val PostOpMar} to the argument {.arg MAR}. \n \n If you would like to use a different MAR (EG a community MAR) please  {.href [click here](https://github.com/SAFEHR-data/IMPACTfunctions/issues)} to open a github issue. ")
  }

  ## Dataset checking
  duplicate_rows <- any(dataset %>% pull(any_of(csn_vector_name)) %>% duplicated())
  if(duplicate_rows & identical(MAR, IntraOpMar)){
    cli::cli_alert_danger("CAUTION: There are rows within the dataset provided that have a duplicated value for {.arg {csn_vector_name}}, this may cause errors as you are searching intra-op MAR's. This is because the variable returned will be a sum/avg across mutiple anaesthetic MAR's. Suggest enforcing filter with one procedure per encounter")
  }else if(duplicate_rows & identical(MAR, PostOpMar)){
    cli::cli_alert_danger("CAUTION: There are  rows within the dataset provided that have a duplicated value for {.arg {csn_vector_name}}, this may cause errors as you are searching post-op MAR's. Ajny timeframe filtering may return varying windows. Variable may be duplicated across these duplicated rows.")
  }

  na_csn_rows <- any(dataset %>% pull(any_of(csn_vector_name)) %>% is.na())

  if(na_csn_rows){
    cli::cli_alert_danger("CAUTION: There are rows within the dataset provided that have {.val NA} in the column {.arg {csn_vector_name}}, this may cause errors as you are searching against a {.val NA} CSN. Reccommend you run {.fn cohort_complete_admissions} in your workflow. \n \n Removing these rows for now. ")
    dataset <- dataset %>% filter(!is.na(csn_vector_name))
  }



  ## Assigning time points if used timeframe argument
  if(missing(timeframe)){
    if(verbose){cli::cli_alert_success("No timeframe provided, searching whole MAR per encounter.")}
    timeframe <- "whole_mar"
  }else if(timeframe == "whole_mar"){
    if(verbose){cli::cli_alert_success("{.arg timeframe} set to {.val {timeframe}}, searching whole MAR per encounter.")}
  }else if(timeframe == "pre_op"){
    if(verbose){cli::cli_alert_success("{.arg timeframe} set to {.val {timeframe}}, searching only pre-operative period.")}
    t0_col <- dataset %>% select(all_of(csn_vector_name), t0 = admission_date)
    t1_col <- dataset %>% select(all_of(csn_vector_name), t1 = operation_date)
    timeframe_df <- inner_join(t0_col, t1_col) %>% collect()
  }else if(timeframe == "post_op"){
    if(verbose){cli::cli_alert_success("{.arg timeframe} set to {.val {timeframe}}, searching only post-operative period.")}
    t0_col <- dataset %>% select(all_of(csn_vector_name), t0 = operation_date)
    t1_col <- dataset %>% select(all_of(csn_vector_name), t1 = discharge_date)
    timeframe_df <- inner_join(t0_col, t1_col) %>% collect()
  }else if(timeframe == "custom"){
    ## Error checking that the timestamps are set properly
    if(verbose){cli::cli_alert_success("{.arg timeframe} set to {.val {timeframe}}, searching only post-operative period.")}
    #If timeframe custom, ensure propry t0,t1 and t_int set.
    t0_col <- dataset %>% select(all_of(csn_vector_name, t0))
    if(!missing(t_int) & !missing_t_1){
      cli::cli_abort("You have tried to supply both a time interval ({.arg t_int}) and a end time ({.arg t_1}). \n \n Supply only one end time variable")
    }else if(!missing(t0) & (mising(t1) & missing(t_int))){
    cli::cli_alert_danger("CAUTION: you have supplied a start time to subset by but not an end time. Will return from the {.arg t0} timepoint onwards.")
    }

    #Assuming test above are passed then we can check if have passed a t_int and then calcualte the t_1
    if(!missing(t_int)){
      t0_col <- dataset %>% select(all_of(csn_vector_name), t0 = all_of(t0))
      timeframe_df <- t0_col %>% collect() %>% mutate(t1 = t0 + t_int)
    }else if(!missing(t1)){
      t0_col <- dataset %>% select(all_of(csn_vector_name),  t0 = all_of(t0))
      t1_col <- dataset %>% select(all_of(csn_vector_name),  t1 = all_of(t1))
      timeframe_df <- full_join(t0_col, t1_col) %>% collect()
    }else{
      cli::cli_abort("Hmm, you should have been able to trigger this error message. Please {.href [leave a issue on the github](https://github.com/SAFEHR-data/IMPACTfunctions/issues)} called: \"Check timeframe logic of extract_medications()\ \n \n ")
      }

  }

  if(csn_vector_name == "admission_csn" & timeframe != "whole_mar" ){
    # ERROR CHECKING - See Github Issue: #21
    if(typeof(timeframe_df$admission_csn) != typeof(MAR %>% pull(admission_csn))){
      cli::cli_alert_danger("KNOWN BUG: Datatype dont match for {.var admission_csn}. See {.href [github issue for more info](https://github.com/SAFEHR-data/IMPACTfunctions/issues/21) }")
      timeframe_df$admission_csn <- as.numeric(timeframe_df$admission_csn)
    }else{
      cli::cli_alert_success("KNOWN BUG FIXED!!: Please close {.href [github issue](https://github.com/SAFEHR-data/IMPACTfunctions/issues/21) }")
      }
    }

  if(verbose){cli::cli_alert_success("Arguments provided to function correct")}

  # Data quality checking

  if(apply_preproccess_MAR){
    MAR <- preproccess_MAR(MAR)
  }

  if(check_search){
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
  }

  # Step 2 - Subsetting

  # We concatenate the medications strings here to allow the search functions above to recive a vector.
  if(length(medication)>1){
    if(verbose){cli::cli_alert_info("You have provided a vector of medications, will do an \"or\" search to return any matching rows.")}
    medication <- paste0(medication, collapse = "|")
  }

  # 2a - Subset for medication

  subset <- MAR %>% filter(get({{search_column}}) %in% medication)

  # 2b - Subsetting for the patients in the dataset
  csn_vector <- dataset %>% pull(.data[[csn_vector_name]]) %>% na.omit()
  subset <- subset %>% filter(get({{csn_vector_name}}) %in% csn_vector)

  # 2c - Subsetting for timeframe

 if(timeframe == "whole_mar"){
   return(subset)
 }else{
   #We need unique row indexes to do our filtering. Arrow doesnt support row indexing (by its nature of parrallel reading, etc).
   ##Instead we pull our timepoints in the logic section above to make a fixed dataset object where the t0 and t1 times are on the same row as the CSN.

   ## Regrettably the arrow dpplyr doesnt have the kernals to deal with timeframe periods (as generated by t0 + t_int), thus we now call into memory.
    df <- left_join(subset, timeframe_df, copy = TRUE, relationship = "one-to-many")
    df <- df %>% filter(administration_date_time > t0) %>% filter(administration_date_time < t1)

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



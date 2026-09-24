

#' Calculate features across medications
#'
#' For more details see the help vignette:
#' \code{vignette("extracting_features", package = "IMPACTfunctions")}
#'
#' @param dataset The dataset of patients of intrest to return the feature against.
#' @param MAR The subsetted MAR from the output of the `extract_medication()` function.
#' @param format The format you want the function to return, choose either: **"joined"** or **"table"**
#' @param feature If returning a vector, what type of feature do you want extracted? Choose: **"binary"**, "sum_dose", "sum_ome","count"
#' @param feature_name if returning a vector, what do you want the feature column called?
#' @param link_column Which column to use as the primary source of return. Select one option from: `link_column = c(admission_csn, anaesthesia_csn, operation_csn)`
#' @param override_dose_check The function checks the dose units before returning a summed feature (`sum_dose`/`sum_ome`). If `MAR` has a standardised `dose_mg` column (see `preproccess_MAR(standardise_units = TRUE)`) the check passes silently. Otherwise: a single `dose_units` value of `"mg"` passes silently, a single non-`"mg"` value passes with a warning that the sum is returned unstandardised, and multiple different `dose_units` values trigger a hard error (this is the case most likely to cause a silent, incorrect summed value). Separately, once summing, if any of the rows being summed have a `NA` `dose_mg`/`ome` (eg an unconvertible unit such as `"Units"` or `mL`) a warning is issued naming the affected units, since the sum may not be representative. Set `override_dose_check = TRUE` to bypass the units check entirely at the risk of returning a summed dose across different units.
#' @param override_route_check The function will check to ensure all the of the routes are the same before returning the feature. Set to `TRUE` to override this at the risk of returning a summed dose across different routes
#' @param verbose Print the outputs of the function explicility.
#' @return Either a table of the vector of interest of the joined dataset left joined.
#' @template setup-data
#' @export
#' @examples
#' \dontrun{
#'  dataset <- cohort_complete_admissions(Procedures) %>%
#'  cohort_procedure_codes( cohort_name = "Hip Arthroplasty", include_OPCS = c("W46", "W47", "W48"))
#'
#'  medication <- "morphine sulfate"
#'  MAR <- HospitalMar

#'  post_op_morphine <- extract_medication(dataset,
#'  MAR = HospitalMar,
#'  medication = medication,
#'  timeframe = "custom",
#'  t0 = "operation_date",
#'  t1 = "discharge_date",
#'  verbose = TRUE,
#'  check_search=FALSE)
#'
#' dataset <- calculate_medication_feature(dataset,
#' MAR = post_op_morphine,
#' format = "joined",
#' feature = "sum_dose",
#' feature_name = "morphine_post_op",
#' override_dose_check= TRUE)
#'
#'
#' morphine_table <- calculate_medication_feature(dataset,
#' MAR = post_op_morphine,
#' format = "table",
#' feature = "binary",
#' feature_name = "morphine_post_op",
#' override_dose_check= TRUE)
#' }
#'
calculate_medication_feature <- function(dataset,
                                         MAR,
                                         format,
                                         feature,
                                         feature_name,
                                         link_column = "admission_csn",
                                         override_dose_check = FALSE,
                                         override_route_check = FALSE,
                                         verbose = TRUE){

  ## Step 3 further filtering and cleaning of data


  # We return the join on the csn of choice determined by the `link_column` entery

  if(link_column == "admission_csn"){
    csn_vector_name <- "admission_csn"
    if(any(duplicated(dataset %>% pull(admission_csn)))){
      cli::cli_abort("Duplication error. In the dataset provided there is duplicated entries for {.val admission_csn}. \n This will cause an issue when trying to left join and cause duplicaton. \n \n Change {.arg link_column} to a different csn or de-duplicate the rows in {.val dataset} ")
    }

  }else if(link_column == "anaesthesia_csn"){
    csn_vector_name <- "anaesthesia_csn"
    if(any(duplicated(dataset %>% pull(anaesthesia_csn)))){
      cli::cli_abort("Duplication error. In the dataset provided there is duplicated entries for {.val anaesthesia_csn}. \n This will cause an issue when trying to left join and cause duplicaton. \n \n Change {.arg link_column} to a different csn or de-duplicate the rows in {.val dataset} ")
    }

  }else if(link_column == "operation_csn"){
    csn_vector_name <- "operation_csn"
    if(any(duplicated(dataset %>% pull(operation_csn)))){
      cli::cli_abort("Duplication error. In the dataset provided there is duplicated entries for {.val operation_csn}. \n This will cause an issue when trying to left join and cause duplicaton. \n \n Change {.arg link_column} to a different csn or de-duplicate the rows in {.val dataset} ")
    }

  }else{
    cli::cli_abort("The {.arg link_column} is not one of {.val admission_csn}, {.val anaesthesia_csn} or {.val operation_csn}. Check input arguments")
  }

  ## DETECT PCA dataframes being passed

  if(all(c( "cumulative_sum","binned_value") %in% names(MAR))){
    PCA_DATA <- TRUE
    cli::cli_bullets(c(
      "i" = "Detected a cleaned PCA dataframe, this may change some of the options available.",
      "*" = "Dose check unavailable",
      "*" = "Route check unavailable"
    ))
    override_dose_check <- TRUE
    override_route_check <- TRUE

  }else{
    PCA_DATA <- FALSE
  }


  ## Users may want to return a feature (such as an avg or sum dose) across medications that are not comparable. For example (sum dose of an infusion and a discreate medications)
  ## We will do some error checking on `MAR` to warn against this. We can bypass some error checking is we want OME for example.


  ## Error checking: Before pooling ensure, same drug, same route, same units.

  has_dose_mg <- "dose_mg" %in% names(MAR)
  dose_units_present <- if(!PCA_DATA) unique(MAR %>% pull(dose_units)) else NA_character_

  if(override_dose_check){
    cli::cli_alert_success("Dose check overidden.")
  }else if(has_dose_mg){
    if(verbose){cli::cli_alert_success("Standardised {.val dose_mg} column detected, proceeding using standardised mg doses.")}
  }else if(length(dose_units_present) == 1 && dose_units_present == "mg"){
    if(verbose){cli::cli_alert_success("Dose units are all the same (mg)!")}
  }else if(length(dose_units_present) == 1 && dose_units_present != "mg"){
    cli::cli_alert_warning("Dose units are all {.val {dose_units_present}}, not {.val mg}. The summed value will be returned in {.val {dose_units_present}}, not standardised. \n \n Run {.fn preproccess_MAR} with {.arg standardise_units = TRUE} first for a standardised dose in mg.")
  }else{
      cli::cli_abort("Not all dose units the same.\n In {.val MAR} there are mutiple units of the dose ({.val {dose_units_present}}).\n\n This will cause a silent failure when summing across incompatible units. Run {.fn preproccess_MAR} with {.arg standardise_units = TRUE} first, or set {.arg override_dose_check} to {.val TRUE} to bypass at your own risk.")
    }

  if(override_route_check){
    cli::cli_alert_success("Route check overidden.")
  }else if(length(unique(pull(MAR, route))) == 1){
    if(verbose){cli::cli_alert_success("Routes are all the same!")}
  }else{
        cli::cli_abort("Not all medication routes are the same.\n In {.val MAR} there are mutiple routes ({.val {unique(MAR$route)}}).\n\n If this will not impact your feature then change {.arg override_route_check} to {.val TRUE}. \n Otherwise review your arguments in {.fn extract_medication} ")
  }

  # 2b - Establishing the value to return against.
  ## Uses the vector that is set above given the type of MAR you are extracting against.

  csn_vector <- dataset %>% pull(.data[[csn_vector_name]]) %>% na.omit()

  mar_csn_vector <- MAR %>% pull(.data[[csn_vector_name]]) %>% na.omit()

  if(any(!csn_vector %in% mar_csn_vector)){
    missing_length <- sum(!csn_vector %in% mar_csn_vector)
    cli::cli_alert_danger("Warning, for {.val {missing_length}} {.val {csn_vector_name}} there were no correspending MAR entries found. This may lead to silent failures where the returned value may be FALSE/0 but rather the csn provided and the CSN on the MAR do not match. ")

  }

  if(feature == "binary"){

    MAR_df <- MAR %>% collect()

    result <-  data.frame(csn_vector) %>%
      mutate(match_status = csn_vector %in% MAR_df[[csn_vector_name]])

    if(!all(result$match_status)){
      cli::cli_alert_warning("No patients have a positive result. Check your arguments.")
    }


  }else if(feature == "sum_dose"){

    if((length(unique(MAR %>% pull(simple_generic_name))) > 1) | (length(unique(MAR %>% pull(route_simple))) > 1)){
      cli::cli_alert_danger("DANGER: The MAR supplied more than 1 medication or more than 1 route in it but your are requesting a summed dose. This may return a sum acorss different medications or routes.\n \n Consdier revising the arguement {.arg medication} in the {.fn extract_medication} function. (There may be situations when you want to proceed, such if you wanted to sum the total paracetmol doses regardless of route")
    }

    if(PCA_DATA){dose_col = "binned_value"}else if(has_dose_mg){dose_col = "dose_mg"}else{dose_col = "dose"}

    if(!PCA_DATA && has_dose_mg && any(is.na(MAR %>% pull(.data[[dose_col]])))){
      na_units <- MAR %>% filter(is.na(.data[[dose_col]])) %>% distinct(dose_units) %>% pull(dose_units)
      cli::cli_alert_warning("Some rows have a {.val NA} {.val {dose_col}} (dose units: {.val {na_units}}) which could not be standardised. Because {.arg na.rm = FALSE}, any patient with one of these rows will get a {.val NA} result rather than a silently incomplete sum. \n \n Check {.fn preproccess_MAR}'s dose unit dictionary ({.arg dictionary_dose_units}) if these units should be standardised.")
    }

    result <- MAR %>% group_by(.data[[csn_vector_name]]) %>%
      collect() %>%
      summarise(result = sum(.data[[dose_col]], na.rm = FALSE))


    if(nrow(result) == 1){
      cli::cli_alert_warning("No patients have a summed dose result. Check your arguments.")
    }

  }else if(feature == "sum_ome"){

    # If requesting and OME, check there is more than one medication

   if(length(unique(MAR %>% pull(simple_generic_name))) == 1){
     cli::cli_alert_danger("DANGER: The MAR supplied only has 1 medication in it but your are requesting and OME.\n \n Consdier remvoing the arguement {.arg medication} in the {.fn extract_medication} function. (There may be situations when you want to proceed, such as if you want to calcualute a OME of IV and Oral morphine. ")
   }

   if(!PCA_DATA && !("ome" %in% names(MAR))){
     cli::cli_abort("You have requested {.val sum_ome} but {.arg MAR} has no {.val ome} column. \n \n Run {.fn preproccess_MAR} first (or call {.fn filter_medication} with {.arg apply_preproccess_MAR = TRUE}).")
   }

   if(PCA_DATA){dose_col = "binned_value_ome"}else{dose_col = "ome"}

    if(!PCA_DATA && any(is.na(MAR %>% pull(.data[[dose_col]])))){
      cli::cli_alert_warning("Some rows have a {.val NA} {.val ome} (eg an unmatched medication, or a dose unit that could not be standardised) and will be excluded from the sum by {.arg na.rm = TRUE}, so affected patients' totals may be an underestimate. \n \n Check {.fn preproccess_MAR}'s dose unit dictionary ({.arg dictionary_dose_units}) if these should be standardised.")
    }

    result <- MAR %>% group_by(.data[[csn_vector_name]]) %>%
      collect() %>%
      summarise(result = sum(.data[[dose_col]], na.rm = TRUE))

    if(nrow(result) == 1){
      cli::cli_alert_warning("No patients have a summed dose result. Check your arguments.")
    }

  }else if(feature == "count"){

    result <- MAR %>% group_by(.data[[csn_vector_name]]) %>%
      summarise(result = n()) %>%
      collect()

    if(nrow(result) == 1){
      cli::cli_alert_warning("No patients have a summed dose result. Check your arguments.")
    }

  }else{
    cli::cli_abort("{.arg feature} not recognised, you have supplied {.arg feature} as {.val {feature}}. \n \n
                   Ensure that {.arg feature} is one of {.val {c('binary', 'sum_dose', 'sum_ome','count')}}")

  }

  names(result) <- c(csn_vector_name, feature_name)



  # Return either the feature vector or the feature joined back to the origional dataframe
  if(format == "table"){
    return(result)
  }else if (format == "joined"){

    if(feature_name %in% names(dataset)){
      if(verbose){ cli::cli_alert_warning("Your dataset already has a column called {.val {feature_name}}. Overwriting with the new column data. If this was not intended check the value of {.arg feature_name} and rename as needed.")}
        dataset <- dataset %>% select(-.data[[feature_name]])
    }

    return(left_join(dataset, result))
  }else{
    cli::cli_abort("ERROR: Non-valid value of {.arg format} supplied. You gave {.arg format} = {.val {format}}. Valid options include {.val table} or {.val joined}.")
  }

}




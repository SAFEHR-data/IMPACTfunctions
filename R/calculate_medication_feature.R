

#' Title
#'
#' @param dataset
#' @param MAR
#' @param format The format you want the function to return, choose either: **"table"** or **"vector"**
#' @param feature If returning a vector, what type of feature do you want extracted? Choose: **"binary"**, "Min", "Max", "Mean", "Mode"
#'
#' @return
#' @export
#'
#' @examples
calculate_medication_feature <- function(dataset,
                                         MAR,
                                         format,
                                         feature,
                                         feature_name){

  ## Step 3 further filtering and cleaning of data
  df <- df %>% collect()

  # We determine the MAR type by the columns. Only the Post-Op MAR has `admissions_csn` and vice versa for anaesthetisa CSN and Intra-opMar

  if(all("admission_csn" %in% names(df),"anaesthesia_csn" %in% names(df))){
    cli::cli_abort("The {.arg MAR} has both the anaesthesia and admission_csn as columns. You must use {.fun preproccess_MAR} or review GH issues #23 ")
  }else if("admission_csn" %in% names(df)){
    MAR_type <- "PostOp"
    csn_vector_name <- "admission_csn"
  }else if ("anaesthesia_csn" %in% names(df)){
    MAR_type <- "IntraOp"
    csn_vector_name <- "anaesthesia_csn"
  }else{
    cli::cli_abort("The {.arg MAR} has neither the anaesthesia and admission_csn as columns. Check input data")
  }

  # 2b - Establishing the value to return against.
  ## Uses the vector that is set above given the type of MAR you are extracting against.

  csn_vector <- dataset %>% pull(.data[[csn_vector_name]]) %>% na.omit()

  if(format == "binary"){


  }



  ## Users may want to return a feature (such as an avg or sum dose) across medications that are not comparable. For example (sum dose of an infusion and a discreate medications)
  ## We will do some error checking on `df` to warn against this. We can bypass some error checking is we want OME for example.

  ## Error checking: Before pooling ensure, same drug, same route, same units.

  if(length(unique(df$dose_units, incomparables = TRUE)) == 1){
    if(verbose){cli::cli_alert_success("Dose units are all the same!")}
  }else{
    cli::cli_abort("Not all dose units the same, unable to return feature")
  }

  if(length(unique(df$route, incomparables = TRUE)) == 1){
    if(verbose){cli::cli_alert_success("Routes are all the same!")}
  }else{
    cli::cli_abort("Not all routes the same, unable to return feature")
  }

  if(length(medication) > 1){
    cli::cli_abort("It looks like you are search for more than 1 medication ({.val {medication}}). To return a feature only use one medication")
  }


  ## Now process the features




  dataset %>% pull()

  ## Route checking


  df %>% group_by(.data[[csn_vector_name]]) %>% summarise(n=n())


}




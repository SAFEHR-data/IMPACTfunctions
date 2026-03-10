

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
#' @return Either a table of the vector of interest of the joined dataset left joined.
#' @export
#' @template setup-data
#'
#' @examples
#'
#'   dataset <- cohort_complete_admissions(Procedures) %>%
#'  cohort_procedure_codes( cohort_name = "Hip Arthroplasty", include_OPCS = c("W46", "W47", "W48"))
#'
#'  medication <- "morphine sulfate"
#'  MAR <- PostOpMar

#'  post_op_morphine <- extract_medication(dataset,
#'  MAR = PostOpMar,
#'  medication = medication,
#'  timeframe = "custom",
#'  t0 = "operation_date",
#'  t1 = "discharge_date",
#'  verbose = TRUE)
#'
#' dataset <- calculate_medication_feature(dataset,
#' MAR = post_op_morphine,
#' format = "joined",
#' feature = "sum_dose",
#' feature_name = "morphine_post_op")
#'
#'
#' morphine_table <- calculate_medication_feature(dataset,
#' MAR = post_op_morphine,
#' format = "table",
#' feature = "binary",
#' feature_name = "morphine_post_op")
#'
calculate_medication_feature <- function(dataset,
                                         MAR,
                                         format,
                                         feature,
                                         feature_name,
                                         overide_dose_check = FALSE,
                                         overide_route_check = FALSE){

  ## Step 3 further filtering and cleaning of data


  # We determine the MAR type by the columns. Only the Post-Op MAR has `admissions_csn` and vice versa for anaesthetisa CSN and Intra-opMar

  if(all("admission_csn" %in% names(MAR),"anaesthesia_csn" %in% names(MAR))){
    cli::cli_abort("The {.arg MAR} has both the anaesthesia and admission_csn as columns. You must use {.fun preproccess_MAR} or review GH issues #23 ")
  }else if("admission_csn" %in% names(MAR)){
    MAR_type <- "PostOp"
    csn_vector_name <- "admission_csn"
  }else if ("anaesthesia_csn" %in% names(MAR)){
    MAR_type <- "IntraOp"
    csn_vector_name <- "anaesthesia_csn"
  }else{
    cli::cli_abort("The {.arg MAR} has neither the anaesthesia and admission_csn as columns. Check input data")
  }

  ## Users may want to return a feature (such as an avg or sum dose) across medications that are not comparable. For example (sum dose of an infusion and a discreate medications)
  ## We will do some error checking on `MAR` to warn against this. We can bypass some error checking is we want OME for example.

  ## Error checking: Before pooling ensure, same drug, same route, same units.

  if(length(unique(MAR$dose_units, incomparables = TRUE)) == 1){
    if(verbose){cli::cli_alert_success("Dose units are all the same!")}
  }else{
    if(overide_dose_check){if(verbose){cli::cli_alert_success("Dose unit check overidden. Proceding to return vector with mutiple dose units.")}
    else{
      cli::cli_abort("Not all dose units the same.\n In {.val MAR} there are mutiple units of the dose ({.val {unique(MAR$dose_units)}}).\n\n If this will not impact your feature then change {.arg overide_unit_check} to {.val TRUE}. \n Otherwise review your arguments in {.fn extract_medication} ")
      }
    }
  }

  if(length(unique(pull(MAR, route))) == 1){
    if(verbose){cli::cli_alert_success("Routes are all the same!")}
  }else{
    if(overide_route_check){if(verbose){cli::cli_alert_success("DRoute check overidden. Proceding to return vector with mutiple routes.")
      }else{
        cli::cli_abort("Not all routes are the same.\n In {.val MAR} there are mutiple routes ({.val {unique(MAR$route)}}).\n\n If this will not impact your feature then change {.arg overide_route_check} to {.val TRUE}. \n Otherwise review your arguments in {.fn extract_medication} ")
      }
    }
  }

  # 2b - Establishing the value to return against.
  ## Uses the vector that is set above given the type of MAR you are extracting against.

  csn_vector <- dataset %>% pull(.data[[csn_vector_name]]) %>% na.omit()

  if(feature == "binary"){

    MAR_df <- MAR %>% collect()

    result <-  data.frame(csn_vector) %>%
      mutate(match_status = csn_vector %in% MAR_df[[csn_vector_name]])


  }else if(feature == "sum_dose"){

    if((length(unique(MAR %>% pull(simple_generic_name))) > 1) | (length(unique(MAR %>% pull(route_simple))) > 1)){
      cli::cli_alert_danger("DANGER: The MAR supplied more than 1 medication or more than 1 route in it but your are requesting a summed dose. This may return a sum acorss different medications or routes.\n \n Consdier revising the arguement {.arg medication} in the {.fn extract_medication} function. (There may be situations when you want to proceed, such if you wanted to sum the total paracetmol doses regardless of route")
    }

    result <- MAR %>% group_by(.data[[csn_vector_name]]) %>%
      summarise(result = sum(dose, na.rm = TRUE)) %>%
      collect()

  }else if(feature == "sum_ome"){

    # If requesting and OME, check there is more than one medication

   if(length(unique(MAR %>% pull(simple_generic_name))) == 1){
     cli::cli_alert_danger("DANGER: The MAR supplied only has 1 medication in it but your are requesting and OME.\n \n Consdier remvoing the arguement {.arg medication} in the {.fn extract_medication} function. (There may be situations when you want to proceed, such as if you want to calcualute a OME of IV and Oral morphine. ")
   }

    result <- MAR %>% group_by(.data[[csn_vector_name]]) %>%
      summarise(result = sum(ome, na.rm = TRUE)) %>%
      collect()

  }else if(feature == "count"){

    result <- MAR %>% group_by(.data[[csn_vector_name]]) %>%
      summarise(result = n()) %>%
      collect()

  }

  names(result) <- c(csn_vector_name, feature_name)


  if(csn_vector_name == "admission_csn"){
    # ERROR CHECKING - See Github Issue: #21
    if(typeof(result$admission_csn) != typeof(dataset %>% pull(admission_csn))){
      cli::cli_alert_danger("KNOWN BUG: Datatype dont match for {.var admission_csn}. See {.href [github issue for more info](https://github.com/SAFEHR-data/IMPACTfunctions/issues/21) }")
      result$admission_csn <- as.character(result$admission_csn)
    }else{
      cli::cli_alert_success("KNOWN BUG FIXED!!: Please close {.href [github issue](https://github.com/SAFEHR-data/IMPACTfunctions/issues/21) }")
    }
  }


  # Return either the feature vector or the feature joined back to the origional dataframe
  if(format == "table"){
    return(result)
  }else if (format == "joined"){
    return(left_join(dataset, result))
  }

}




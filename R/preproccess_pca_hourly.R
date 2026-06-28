#' Preprocessing of pcam hourly
#'
#' Pre-proccesing of tables to allow other feature extractions functions to run. This left joins the dynamic dictionaries and calculate a value for OME.
#'
#' If you want to use custom values then replace the dictionary with your custom dictionary. Do not change the col names and they are used for left joining.
#'
#' If there are medications for which a opioid-equivelence is not being generated this may be due to the lack of dose unit harmonisation. Please log a github issue to get the dose unit added.
#'
#' @param pca_hourly The arrow file of the cleaned pca_hourly.
#' @param route_dict The route dictionary, defaults to `dictionary_medication_route` from the google sheets link below.
#' @param opioid_dict The opioid dictionary, defaults to `dictionary_opioid` from the google sheets link below.
#' @param equivelence_dict The equivalence dictionary, defaults to `dictionary_equivelence` from the google sheets link below.
#' @param administered_only Filter to only keep the medications administered. Note: this will filter out infusions or any medication that are  `"given"` or `"Dose Documentation"`. The latter is used to capture to total doses of TIVA medications.
#'
#' @source <https://docs.google.com/spreadsheets/d/1u1p7TlS8tmzGzNCIWHRg2Nn85YzLGfeF1bR5bbnQLbE/edit?gid=0#gid=0>
#' @return The original MAR arrow table with additional columns and cleaned rows.
#' @export
#' @template setup-data
#'
#' @examples
#' \dontrun{
#' pca_hourly <- preproccess_pca_hourly(pca_hourly)
#' }
preproccess_pca_hourly <- function(pca_hourly,
                                   route_dict = dictionary_medication_route,
                                   opioid_dict = dictionary_opioid,
                                   equivelence_dict = dictionary_equivelence
){

  # Check col names match
  if(! identical(names(route_dict), names(dictionary_medication_route))){cli::cli_abort("You have passed a custom dict to {.arg route_dict} but the column names don't match. \n \n Change the column names of your custom dict to {.val {paste0(names(dictionary_medication_route), collapse = ', ')}}")}
  if(! identical(names(opioid_dict), names(dictionary_opioid))){cli::cli_abort("You have passed a custom dict to {.arg opioid_dict} but the column names don't match. \n \n Change the column names of your custom dict to {.val {paste0(names(dictionary_opioid), collapse = ', ')}}")}
  if(! identical(names(equivelence_dict), names(dictionary_equivelence))){cli::cli_abort("You have passed a custom dict to {.arg equivelence_dict} but the column names don't match. \n \n Change the column names of your custom dict to {.val {paste0(names(dictionary_equivelence), collapse = ', ')}}")}

  # Check for pre-proccess mar already
  if(all(! names(pca_hourly) %in% c("route_simple", "opioid_name","equivelence"))){
    NULL
  }else{
    cli::cli_abort("{.val pca_hourly} passed to arguement {.arg pca_hourly} has columns from the dictonaries already \n \n Have you preproccessed it already?")
  }

  ## Note, we currently only have support for PCA delivered IV. This means we cannot add PCEA and the opioid equivelence therein.
  ## WE will add and error check and then a forced conversion.

  if(any(names(pca_hourly) %in% c("administration_route"))){
    cli::cli_abort("Columns {.val {c('administration_route')}} found in the {.arg pca_hourly}. \n Currently the function unable to handle this. ")
  }

  pca_hourly <- pca_hourly %>%
    mutate(administration_route = "Intravenous") %>%
    left_join(route_dict, by = "administration_route") %>%
    left_join(opioid_dict) %>%
    left_join(equivelence_dict)


  if(all(unique(pca_hourly$units) != "mg")){
    cli::cli_alert_danger("Not all units are in MG. Corcering mcg to mg and any NA to mg. ")

    pca_hourly <- pca_hourly %>%
      mutate(dose = case_when(
        units == "mcg" ~ dose_best_estimate_hourly/1000,
        .default = dose_best_estimate_hourly
      )) %>%
      mutate(units = "mg")

  }else{
    pca_hourly <- pca_hourly %>%
      mutate(dose =  dose_best_estimate_hourly)
  }

  pca_hourly <- pca_hourly %>%
    mutate(ome = dose * equivelence) %>%
    select(mrn, admission_csn, simple_generic_name, dose, units, ome, time_hourly_start, time_hourly_end)

  return(pca_hourly)

}

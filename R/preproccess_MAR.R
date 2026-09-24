#' Preprocessing of MAR
#'
#' Pre-proccesing of tables to allow other feature extractions functions to run. This left joins the dynamic dictionaries and calculate a value for OME.
#'
#' If you want to use custom values then replace the dictionary with your custom dictionary. Do not change the col names and they are used for left joining.
#'
#' If there are medications for which a opioid-equivelence is not being generated this may be due to the lack of dose unit harmonisation. Please log a github issue to get the dose unit added.
#'
#' @param MAR The arrow file of the MAR. defaults to `HospitalMar`.
#' @param route_dict The route dictionary, defaults to `dictionary_medication_route` from the google sheets link below.
#' @param opioid_dict The opioid dictonary, defaults to `dictionary_opioid` from the google sheets link below.
#' @param equivelence_dict The equivilence dictonary, defaults to `dictionary_equivelence` from the google sheets link below.
#' @param dose_units_dict The dose unit conversion dictionary, defaults to `dictionary_dose_units` from the google sheets link below. Used to standardise `dose` into a common `dose_mg` column when `standardise_units = TRUE`. See `standardise_units` below.
#' @param administered_only Filter to only keep the medications administered. Note: this will filter out infusions or any medication that are  `"given"` or `"Dose Documentation"`. The latter is used to capture to total doses of TIVA medications.
#' @param standardise_units Standardise `dose` into a common `dose_mg` column (in milligrams) so that doses/OME can be safely summed across medications recorded in different units. This is driven by a left join against `dose_units_dict`, which maps every known `dose_units` string to a `multiplication_factor` (`dose * multiplication_factor = dose_mg`). Units that cannot be safely standardised have a `NA` `multiplication_factor` in the dictionary and are intentionally left as `NA` in `dose_mg`: this includes count/container units (eg `"tablet"`, `"vial"`), biological-activity units (eg `"Units"` of insulin, which has no fixed mass equivalent), rate units (eg `"mg/hr"` - a rate is not directly comparable to a one-off dose even though its mass prefix could be converted), and any `mL`-based dose (these depend on a variable/unknown concentration and possibly a multi-medication combination, so are not addressed here). If a `dose_units` value is not present in `dose_units_dict` at all (eg a new or misspelt unit appearing in the source data) it is also left as `NA`, and a warning lists the unmatched values so they can be triaged and added to the dictionary. When `FALSE`, no `dose_mg` column is created and the OME calculation falls back to only covering medications already recorded in `"mg"`.
#'
#' @source <https://docs.google.com/spreadsheets/d/1u1p7TlS8tmzGzNCIWHRg2Nn85YzLGfeF1bR5bbnQLbE/edit?gid=0#gid=0>
#' @return The origonal MAR arrow table with additional columns and cleaned rows.
#' @export
#' @template setup-data
#'
#' @examples
#'
#' cleaned_MAR <- preproccess_MAR(MAR = HospitalMar)
#' head(cleaned_MAR %>% collect())
#'
preproccess_MAR <- function(MAR = HospitalMar,
                            route_dict = dictionary_medication_route,
                            opioid_dict = dictionary_opioid,
                            equivelence_dict = dictionary_equivelence,
                            dose_units_dict = dictionary_dose_units,
                            administered_only = TRUE,
                            standardise_units = TRUE){

  # Check col names match
  if(! identical(names(route_dict), names(dictionary_medication_route))){cli::cli_abort("You have passed a custom dict to {.arg route_dict} but the column names don't match. \n \n Change the column names of your custom dict to {.val {paste0(names(dictionary_medication_route), collapse = ', ')}}")}
  if(! identical(names(opioid_dict), names(dictionary_opioid))){cli::cli_abort("You have passed a custom dict to {.arg opioid_dict} but the column names don't match. \n \n Change the column names of your custom dict to {.val {paste0(names(dictionary_opioid), collapse = ', ')}}")}
  if(! identical(names(equivelence_dict), names(dictionary_equivelence))){cli::cli_abort("You have passed a custom dict to {.arg equivelence_dict} but the column names don't match. \n \n Change the column names of your custom dict to {.val {paste0(names(dictionary_equivelence), collapse = ', ')}}")}
  if(! identical(names(dose_units_dict), names(dictionary_dose_units))){cli::cli_abort("You have passed a custom dict to {.arg dose_units_dict} but the column names don't match. \n \n Change the column names of your custom dict to {.val {paste0(names(dictionary_dose_units), collapse = ', ')}}")}

  # Check for pre-proccess mar already
  if(identical(MAR, HospitalMar)){
    MAR_choice <- "HospitalMar"
  }else{
    cli::cli_abort("MAR passed to arguement {.arg MAR} is not identical to the base MARs. \n \n Have you preproccessed it already?")
  }

  # Add dictonaries

  MAR <- MAR %>%
    left_join(route_dict) %>%
    left_join(opioid_dict) %>%
    left_join(equivelence_dict)

  ## Dose unit standardisation
  ## Converts `dose` into a common `dose_mg` column via a left join against
  ## `dose_units_dict`. Units with no `multiplication_factor` in the
  ## dictionary (or not present in the dictionary at all) are left as NA -
  ## see the `standardise_units` parameter documentation for why.

  if(standardise_units){

    MAR <- MAR %>%
      left_join(dose_units_dict) %>%
      mutate(dose_mg = dose * multiplication_factor)

    unmatched_units <- MAR %>%
      filter(!(dose_units %in% dose_units_dict$dose_units)) %>%
      distinct(dose_units) %>%
      collect() %>%
      pull(dose_units)

    if(length(unmatched_units) > 0){
      cli::cli_alert_warning("The following {.arg dose_units} were not found in {.arg dose_units_dict} and will have {.val NA} {.val dose_mg}: {.val {unmatched_units}}. \n \n Add these to the dose unit dictionary if they can be safely standardised.")
    }

  }

  ## Calculate OME
  ## OME = dose in mg multiplied by the opioid's morphine-equivalence factor
  ## (eg fentanyl parenteral equivelence = 200 means 1mg fentanyl ~ 200mg morphine, Eq.1 TOB = dose * conversion value).

  if(standardise_units){

    MAR <- MAR %>%
      mutate(
        ome = if_else(!is.na(equivelence) & !is.na(dose_mg), dose_mg * equivelence, NA_real_)
      )

  }else{

    cli::cli_alert_warning("{.arg standardise_units} is {.val FALSE}. OME will only be calculated for medications already recorded in {.val mg}; medications in other units will have {.val NA} OME. Set {.arg standardise_units = TRUE} for full coverage.")

    MAR <- MAR %>%
      mutate(
        ome = if_else(!is.na(equivelence) & dose_units == "mg", dose * equivelence, NA_real_)
      )

  }

  ## Need to add admnisatation action == "given"

  if(administered_only){
  MAR <- MAR %>% filter(administration_action %in% c("Given", "Dose Documentation"))
  }

  ## De-duplicate rows
  if(any(duplicated(MAR %>% pull(mar_id, as_vector = TRUE)))){
    cli::cli_abort("DATA ISSUE: There is duplication of the `mar_id` column. This may cause double counting of medications.")
  }

 return(MAR)
}

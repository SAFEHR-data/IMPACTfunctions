#' Preprocessing of MAR
#'
#' Preproccesing of tables to allow other feature extractions functions to run. This left joins the dynamic dictionaries and calculate a value for OME.
#'
#' If you want to use custom values then replace the dictonary with your custom dictonary. Do not change the col names and they are used for left joining.
#'
#' @param MAR The arrow file of the MAR
#' @param route_dict The route dictionary, defaults to `dictionary_medication_route` from the google sheets link below.
#' @param opioid_dict The opioid dictonary, defaults to `dictionary_opioid` from the google sheets link below.
#' @param equivelence_dict The equivilence dictonary, defaults to `dictionary_equivelence` from the google sheets link below.
#' @param deduplicate_rows KEEP TRUE, see GH issue #24
#' @param administered_only Filter to only keep the medications administered. Note: this will filter out infusions or any medication that are `!= "given"`
#'
#' @source <https://docs.google.com/spreadsheets/d/1u1p7TlS8tmzGzNCIWHRg2Nn85YzLGfeF1bR5bbnQLbE/edit?gid=0#gid=0>
#' @return
#' @export
#'
#' @examples
preproccess_MAR <- function(MAR,
                            route_dict = dictionary_medication_route,
                            opioid_dict = dictionary_opioid,
                            equivelence_dict = dictionary_equivelence,
                            deduplicate_rows = TRUE,
                            administered_only = TRUE){

  # Check col names match
  if(! identical(names(route_dict), names(dictionary_medication_route))){cli::cli_abort("You have passed a custom dict to {.arg route_dict} but the column names don't match. \n \n Change the column names of your custom dict to {.val {paste0(names(dictionary_medication_route), collapse = ', ')}}")}
  if(! identical(names(opioid_dict), names(dictionary_opioid))){cli::cli_abort("You have passed a custom dict to {.arg opioid_dict} but the column names don't match. \n \n Change the column names of your custom dict to {.val {paste0(names(dictionary_opioid), collapse = ', ')}}")}
  if(! identical(names(equivelence_dict), names(dictionary_equivelence))){cli::cli_abort("You have passed a custom dict to {.arg equivelence_dict} but the column names don't match. \n \n Change the column names of your custom dict to {.val {paste0(names(dictionary_equivelence), collapse = ', ')}}")}

  # Add dictonaries

  MAR <- MAR %>%
    left_join(route_dict) %>%
    left_join(opioid_dict) %>%
    left_join(equivelence_dict)

  ## Dose harmonisation

  # THis is a placeholder for github issue #23.
  # The dose haromisation is being done for two doses onlu: MG and MCG as below in the case when

  ## Calculate OME

  MAR_processed <- MAR %>%
    # Perform mutation before collect() if possible to optimize DB-side execution
    mutate(
      # Standardize dose to mg first, then apply equivalence factor
      ome = case_when(
        dose_units == "mg"         ~ dose / equivelence,
        dose_units == "micrograms" ~ (dose / 1000) / equivelence,
        TRUE                       ~ NA_real_
      )
    )

  ## Need to add admnisatation action == "given"

  MAR <- MAR %>% filter(administration_action == "Given")

 return(MAR)

}

#' Recode Ethnicity
#'
#' A helper function to collapse the ~20 ethnicity groups provided by epic into the 5 group ONS ethnicity. EPIC coding of ethnicty doesn't exactly match the ONS grouping and in addition there are mutiple strings that represent NA (such as "refused to give" or "Not asked")
#'
#' To see the values used in the conversion run `recode_ethnicity` to see the function code.
#'
#' Use the below text in manuscripts to explain how this function works:
#'
#' Ethnicity data was collated and recoded using the Office for National Statistics (ONS) 5 ethnic group classification.
#' Where Ethnicity data was not provided or collected values were marked as missing.
#'
#' @param ethnicity_vector The vector or datafame column to convert
#' @param short_names Print short name groups
#'
#' @return A vector of recoded ethnicities
#' @export
#' @source https://www.ons.gov.uk/census/census2021dictionary/variablesbytopic/ethnicgroupnationalidentitylanguageandreligionvariablescensus2021/ethnicgroup/classifications
#'
#' @examples
#' demo <- demo %>%
#' mutate(
#' ethnicity_5_group = recode_ethnicity(ethnicity)
#' )
#'
#'
recode_ethnicity <- function(ethnicity_vec, short_names = FALSE) {
  known_inputs <- c(
    "White British", "Other White Background", "White Irish",
    "Asian Bangladeshi", "Other Asian Background", "Asian Pakistani", "Asian Indian", "Chinese",
    "Black African", "Other Black Background", "Black Caribbean", "Black British",
    "Other Mixed Background", "Mixed White and Asian", "Mixed White and Black African", "Mixed White and Black Caribbean",
    "Other Ethnic Group",
    "Not Yet Asked", "Refused to Give", "Not Stated / Unknown"
  )

  # 2. Extract unique non-NA values from the input data
  observed_inputs <- unique(na.omit(ethnicity_vec))

  # 3. Detect unmapped strings using set operations
  unmapped_values <- setdiff(observed_inputs, known_inputs)

  if (length(unmapped_values) > 0) {
    cli::cli_abort(c(
      "x" = "Data validation failed: Unmapped ethnicity string(s) detected.",
      "i" = "The pipeline has been halted to prevent silent data drop or misclassification.",
      "*" = "Please update the mapping function to handle: {.val {unmapped_values}}"
    ))
  }

  # 4. Deterministic mapping pipeline
  result <-   case_when(
    ethnicity_vec %in% c("White British", "Other White Background", "White Irish") ~ "White",

    ethnicity_vec %in% c("Asian Bangladeshi", "Other Asian Background", "Asian Pakistani",
                         "Asian Indian", "Chinese") ~ "Asian, Asian British or Asian Welsh",

    ethnicity_vec %in% c("Black African", "Other Black Background", "Black Caribbean", "Black British") ~ "Black, Black British, Black Welsh, Caribbean or African",

    ethnicity_vec %in% c("Other Mixed Background", "Mixed White and Asian",
                         "Mixed White and Black African", "Mixed White and Black Caribbean") ~ "Mixed or Multiple ethnic groups",

    ethnicity_vec %in% c("Other Ethnic Group") ~ "Other ethnic group",

    ethnicity_vec %in% c("Not Yet Asked", "Refused to Give", "Not Stated / Unknown", "") ~ NA_character_,

    # --- Legitimate NA Values ---
    # Prevents true missing data from cascades into the error condition
    is.na(ethnicity_vec) ~ NA_character_
  )

  if(short_names){
   result <-  dplyr::recode_factor(result,
                         "White" = "White",
                         "Asian, Asian British or Asian Welsh" = "Asian",
                         "Black, Black British, Black Welsh, Caribbean or African" = "Black",
                         "Mixed or Multiple ethnic groups" = "Mixed",
                         "Other ethnic group" = "Other",
                         .ordered=FALSE)
  }

  return(result)
}

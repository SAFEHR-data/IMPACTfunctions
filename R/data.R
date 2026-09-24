#' URL of the online dynamic dictionary
#'
#' We currently host the dynamic dictionaries in a google sheets location.
#' @format ## `dict_url`
#' A string of the URL.
#' @source <https://docs.google.com/spreadsheets/d/1u1p7TlS8tmzGzNCIWHRg2Nn85YzLGfeF1bR5bbnQLbE/edit?gid=0#gid=0>
"dict_url"

#' URL of the github repo
#'
#' We stroe this for helping with error messages
#' @format ## `github_url` and `github_url_issues`
#' A string of the URL.
#' @source <https://github.com/SAFEHR-data/IMPACTfunctions>
"github_url"

#' URL of the github repo
#'
#' We stroe this for helping with error messages
#' @format ## `github_url` and `github_url_issues`
#' A string of the URL.
#' @source <https://github.com/SAFEHR-data/IMPACTfunctions>
"github_url_issues"



#' Dynamic Dicts - IMPACT Schema
#'
#' We source 5 dictonaries from the online google sheets.
#' This table is the schema for which we do dictonary cehcks agaisnt.
#'
#' @format ## `dictionary_IMPACT_schema`
#' A dataframe with 3 columns:
#' \describe{
#'  \item{table_name}{The approved name of the tables}
#'  \item{column_name}{The names of the columns within that dataset, long format}
#'  \item{data_level}{Attributing the data level to that table}
#' }
#'
#' @source "https://docs.google.com/spreadsheets/d/1u1p7TlS8tmzGzNCIWHRg2Nn85YzLGfeF1bR5bbnQLbE/edit?gid=0#gid=0"
"dictionary_IMPACT_schema"

#' Dynamic Dicts - Medications Route
#'
#' We source 5 dictonaries from the online google sheets.
#' This table is the schema for which we do route conversions against.
#'
#' @format ## `dictionary_medication_route`
#' A dataframe with 3 columns:
#' \describe{
#'  \item{administration_route }{The route of administation from the MAR}
#'  \item{route}{The grouped routes into broad catagories}
#'  \item{route_simple}{A very simple route set using only Enteral or Parenteral }
#' }
#'
#' @source <https://docs.google.com/spreadsheets/d/1u1p7TlS8tmzGzNCIWHRg2Nn85YzLGfeF1bR5bbnQLbE/edit?gid=0#gid=0>
"dictionary_medication_route"

#' Dynamic Dicts - Opioids conversion
#'
#' We source 5 dictonaries from the online google sheets.
#' This table is the schema for which we do opioid summarisation.
#'
#' @format ## `dictionary_opioid`
#' A dataframe with 3 columns:
#' \describe{
#'  \item{simple_generic_name}{The medication name from the MAR}
#'  \item{opioid_name}{The name of the active opioid with the medication}
#' }
#'
#' @source <https://docs.google.com/spreadsheets/d/1u1p7TlS8tmzGzNCIWHRg2Nn85YzLGfeF1bR5bbnQLbE/edit?gid=0#gid=0>
"dictionary_opioid"

#' Dynamic Dicts - Opioids equivalence
#'
#' We source 5 dictonaries from the online google sheets.
#' This table is the schema for which we do equivelence joins
#'
#' @format ## `dictionary_equivelence`
#' A dataframe with 3 columns:
#' \describe{
#'  \item{opioid_name}{The name of the active opioid with the medication}
#'  \item{route}{The route of the medication}
#'  \item{equivelence}{Equivilence value}
#' }
#'
#' @source <https://docs.google.com/spreadsheets/d/1u1p7TlS8tmzGzNCIWHRg2Nn85YzLGfeF1bR5bbnQLbE/edit?gid=0#gid=0>
"dictionary_equivelence"

#' Dynamic Dicts - Dose unit standardisation
#'
#' We source 5 dictonaries from the online google sheets.
#' This table maps every known `dose_units` string seen in the MAR to a
#' `multiplication_factor` used to standardise `dose` into a common
#' `dose_mg` column in \code{\link{preproccess_MAR}}. Units that cannot be
#' safely standardised (eg count/container units, biological-activity
#' units, rate units, or volume-based doses) have a `NA`
#' `multiplication_factor` and are intentionally left unconverted. A
#' `dose_units` value not present in this dictionary at all is also left
#' unconverted, and flagged with a warning so it can be triaged and added.
#'
#' @format ## `dictionary_dose_units`
#' A dataframe with 4 columns:
#' \describe{
#'  \item{dose_units}{The dose unit string exactly as it appears in the MAR `dose_units` column}
#'  \item{multiplication_factor}{The factor `dose` is multiplied by to give `dose_mg`. `NA` if the unit cannot be safely standardised}
#'  \item{unit_category}{A short label describing the kind of unit (eg `mass`, `count`, `volume`, `rate`, `molar`, `biological_activity`), to help maintain the dictionary}
#'  \item{notes}{Free text rationale, especially for units left unconverted}
#' }
#'
#' @source <https://docs.google.com/spreadsheets/d/1u1p7TlS8tmzGzNCIWHRg2Nn85YzLGfeF1bR5bbnQLbE/edit?gid=0#gid=0>
"dictionary_dose_units"

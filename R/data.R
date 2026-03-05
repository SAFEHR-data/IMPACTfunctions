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
#' We source 4 dictonaries from the online google sheets.
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

#' Dynamic Dicts - Medications Routes
#'
#' We source 4 dictonaries from the online google sheets.
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
#' We source 4 dictonaries from the online google sheets.
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
#' We source 4 dictonaries from the online google sheets.
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

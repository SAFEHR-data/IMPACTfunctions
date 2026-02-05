#' Impact dataset schema
#'
#' A data dictonary or scheme to map against the imported parquet file to check for completeness.
#'
#' In Version 1 this is just a vector of accepted table names
#' In future versions we hope to have column and datatypes to validate and additional a hosted live dictionary that can be dynamically updated
#'
#' @format ## `IMPACT_dictionary`
#' A dataframe of table names of 43 rows and 1 column:
#' \describe{
#'   \item{table_name}{String of the accepted tables names}
#' }
#' @source Hand written
#'
"IMPACT_dictionary"

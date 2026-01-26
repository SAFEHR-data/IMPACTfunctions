#' Import a IMPACT dataset into the R session
#'
#' `load_dataset` Takes the folder path of IMPACT dataset and uploaded the tables to the current R session as arrow objects.
#'
#' @details
#' The `dictionary_check` argument checks the files within the target folder against a dictionary of known accepted table names to ensure dataset quality. Whilst the dataset is in development this may become out of date. If persistent issue then please leave a github issue.
#'
#'
#' @param folder_path Path to the folder containing the parquet files to be loaded
#' @param metadata_path If a metadata file has been provided then this is the path to it. FEATURE NOT YET BUILT
#' @param prefix A string to remove from the start of the parquet file names
#' @param suffix A string to remove from the end of the parquet file names
#' @param dictionary_check Check the dataset being loaded against the IMPACT dictionary to ensure data completeness
#' @param verbose Print the output of each step
#'
#' @return A series of tables for the IMPACT dataset in the arrow object binding
#' @export
#'
#' @examples
#' \dontrun{
#' load_dataset(folder_path = "~/data/IMPACT/full_extract",
#'             prefix = "SCB_Periop_",
#'             suffix = "_20260122.parquet",
#'             dictionary_check = TRUE,
#'             verbose = TRUE)}
load_dataset <- function(
    folder_path ,
    metadata_path,
    prefix,
    suffix,
    dictionary_check = TRUE,
    verbose = FALSE){

  if(verbose){cli::cli_h1("Importing dataset")}

  # Stage 1 - Data discovery and metadata discovery

  files <- list.files(folder_path, pattern = "\\.parquet$")
  if(length(files) == 0){cli::cli_abort("No parquet files found at {.path {folder_path}}. \nCheck folder path and file formats.")}

  if(verbose){cli::cli_alert("{.val {length(files)}} parquet files found in {.path {folder_path}} \n Files: {.val {head(files, n=3)}}, ...")}

  # Stage 2 - Naming tables and check against dictionary

  #If suffix or prefix are supplied use these to strip chars from file names to generate names vector.
  if(!missing(prefix) & !missing(suffix)){
    if(verbose){cli::cli_alert("Cleaning file names by removing the {.arg prefix} {.val {prefix}} and the {.arg suffix} {.val {suffix}}")}
    names <- gsub(prefix, "",  gsub(suffix, "", files))
    }else if(!missing(prefix)){
      names <- gsub(prefix, "", files)
    }else if(!missing(suffix)){
      names <- gsub(suffix, "", files)
    }else{
    names <- files
  }

  if(dictionary_check){
    if(verbose){cli::cli_alert("Checking cleaned file names against the dictonary.\n Names: {.val {head(names, n=3)}}, ...")}
    #Check any missing
    if(any(!IMPACT_dictionary$table_name %in% names)){
      missing_tables <-  paste0(IMPACT_dictionary[which(!IMPACT_dictionary$table_name %in% names), "table_name"], sep=", ")
      cli::cli_abort("The table {.val {missing_tables}} is in the IMPACT dictionary but is not found in the dataset provided.
        \nThis may be because the dataset has been updated by the dictonary itself has not been updated.
        If you think this is the case then you can avoid the check with {.arg dictionary_check} = {.val FALSE}")}
      #Check for additional tables
    if(any(!names %in% IMPACT_dictionary$table_name)){
     additional_tables <-  paste0(names[which(!names %in% IMPACT_dictionary$table_name)], sep=", ")
    cli::cli_abort("The table {.val {additional_tables}} is not in the IMPACT dictionary but is found in the dataset provided.
        \nThis may be because the dataset has been updated by the dictonary itself has not been updated.
        If you think this is the case then you can avoid the check with {.arg dictionary_check} = {.val FALSE}")}
  }


  # Stage 3 - Loading data into current R session

  if(verbose){cli::cli_alert("Uploading {.val {length(files)}} tables to the R session enviroment")}
  for(i in 1:length(files)){
    path <- paste0(folder_path,"/", files[i])
    df <- arrow::open_dataset(path)
    assign(names[i], df, envir = globalenv())
  }

  # Stage 4 - Load metadata if file provided

  if(missing(metadata_path)){
    #No metadata provided
    cli::cli_abort("METADATA function not yet build. \nPlease remove {.arg metadata_path} from function and try again.")
    assign("dataset_metadata", metadata, envir = globalenv())
  }

  if(verbose){cli::cli_alert_success("Dataset imported!")}

}

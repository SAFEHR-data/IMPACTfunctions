#' Import a IMPACT dataset into the R session
#'
#' `load_dataset` Takes the folder path of IMPACT dataset and uploaded the tables to the current R session as arrow objects.
#'
#' @details
#' The `dictionary_check` argument checks the files within the target folder against a dictionary of known accepted table names to ensure dataset quality. Whilst the dataset is in development this may become out of date. If persistent issue then please leave a github issue.
#'
#'
#' @param folder_path Path to the folder containing the parquet files to be loaded
#' @param prefix A string to remove from the start of the parquet file names. If you do not know the suffix, the function may suggest it for you.
#' @param suffix A string to remove from the end of the parquet file names, this should include the '.parquet'
#' @param dictionary_check Check the dataset being loaded against the IMPACT dictionary to ensure data completeness
#' @param verbose Print the output of each step
#'
#' @return A series of tables for the IMPACT dataset in the arrow object binding
#' @import dplyr
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
    prefix,
    suffix = ".parquet",
    dictionary_check = TRUE,
    verbose = FALSE){

  if(verbose){cli::cli_h1("Importing dataset")}

  # Stage 1 - Data discovery and metadata discovery

  files <- list.files(folder_path, pattern = "\\.parquet$")
  if(length(files) == 0){cli::cli_abort("No parquet files found at {.path {folder_path}}. \nCheck folder path and file formats.")}

  if(verbose){cli::cli_alert("{.val {length(files)}} parquet files found in {.path {folder_path}} \n Files: {.val {head(files, n=3)}}, ...")}

  # Stage 2 - Naming tables and check against dictionary

  if(verbose){cli::cli_h1("Cleaning table names with Prefix and Suffix")}
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

  #Suffix and Prefix check - To check if our provided Suffix and Prefix are suitable
  #We test with the first value of the Dictionary that we can produce it.

  if(any(IMPACT_dictionary[1,] %in% names)){
    #We can cleaned to correctly match the first dict name (usually Demographics)
  }else{
    cli::cli_alert_warning("Struggaling to clean the file names using the {.arg prefix} {.val {prefix}} and the {.arg suffix} {.val {suffix}} provided... ")
    demographic_file_name <- files[grepl(IMPACT_dictionary[1,], names)]
    pre_suf_strings <- stringr::str_split(demographic_file_name,IMPACT_dictionary[1,] )
    cli::cli_alert_info("Looks like your {.arg prefix} should be {.val {pre_suf_strings[[1]][1]}} and the {.arg suffix} {.val {pre_suf_strings[[1]][2]}}. \n \n Without this you will either get non-standard table tables (that will fail many of the onward functions, or trigger the missing table errors with the {.arg dictionary_check=TRUE})    ")
    }

  #Check the cleaned table names against the dictonary
  if(dictionary_check){
    if(verbose){cli::cli_h1("Dictonary Check ")}
    if(verbose){cli::cli_alert("Checking table names from dataset ({.val {head(names, n=2)}}, ...) against the dictonary.")}
    #Check any missing
    if(any(!IMPACT_dictionary$table_name %in% names)){
      missing_tables <-  paste0(IMPACT_dictionary[which(!IMPACT_dictionary$table_name %in% names), "table_name"], sep=", ")
      cli::cli_abort("The table {.val {missing_tables}} is in the IMPACT dictionary but is not found in the dataset you provided.
        \n This may be for a number of reasons:
        \n - You have not set the prefix and suffix correctly (scroll up to see warning message if so)
        \n- The dictonary is out of date. If you think this is the case then you can avoid the check with {.arg dictionary_check = FALSE}")}
      #Check for additional tables
    if(any(!names %in% IMPACT_dictionary$table_name)){
     additional_tables <-  paste0(names[which(!names %in% IMPACT_dictionary$table_name)], sep=", ")
    cli::cli_abort("The table {.val {additional_tables}} is not in the IMPACT dictionary but is found in the dataset provided.
        \nThis may be because the dataset has been updated by the dictonary itself has not been updated.
        If you think this is the case then you can avoid the check with {.arg dictionary_check} = {.val FALSE}")}
  }


  # Check for column name integrity
  ## THis is a placeholder until github issue: https://github.com/SAFEHR-data/IMPACTfunctions/issues/1 is resolved
  ## When we have a updateable schema we can match table AND column names in the logic above.
  ## In the meantime we will apply overwritting to ensure common format.
  ## Missing columns are dellt with using the vignettes and the synthetic dataset (If a col isnt in the synth data, the vignette building will fail. )

  ## Format: NEW NAME = OLD NAME
  col_rename <- c("anaesthesia_csn" = "anesthesia_csn")

  # Stage 3 - Loading data into current R session

  if(verbose){cli::cli_alert("Uploading {.val {length(files)}} tables to the R session enviroment")}
  for(i in 1:length(files)){
    path <- paste0(folder_path,"/", files[i])
    df <- arrow::open_dataset(path)


    # If a col name issue is presne then rename
    if(col_rename %in% names(df)){
      #Apply the column renaming
      cli::cli_alert_warning("WARNING: In the table {.val {names[i]}} there is a typo in the column {.val {names(df)[which(names(df) %in% col_rename)]}}. Overwriting to {.val {names(col_rename)[col_rename == names(df)[names(df) %in% col_rename]]}}. ")
      df <- df %>% rename(col_rename)
    }

    assign(names[i], df, envir = globalenv())
  }





  if(verbose){cli::cli_alert_success("Dataset imported!")}

}


#' Loading synthetic data
#'
#'  A function to check if there is Procedure tables in the environment, if not then it will read in the synthetic dataset.
#'
#'  This is used for testing of functions
#'
#' @return Procedures arrow table
#' @export
#' @import dplyr
#'
#' @examples
#' load_synthetic_dataset()
#'
load_synthetic_dataset <- function() {
  if(!exists("Procedures")){
  path <- system.file("extdata", "procedures_synthetic.parquet", package = "IMPACTfunctions")
  Procedures <<- arrow::read_parquet(path, as_data_frame = FALSE) # Returns Arrow Table
  }
  if(!exists("Demographics")){
    path <- system.file("extdata", "demographic_synthetic.parquet", package = "IMPACTfunctions")
    Demographics <<- arrow::read_parquet(path, as_data_frame = FALSE) # Returns Arrow Table
  }
}

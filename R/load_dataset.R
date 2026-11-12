#' Import a IMPACT dataset into the R session
#'
#' `load_dataset` takes the folder path of an IMPACT dataset, reads each parquet file as an arrow object, and returns them together as an [impact_dataset].
#'
#' @details
#' The function returns a value, in the same way that `read_csv()` does, so assign it:
#'
#' ```r
#' impact <- load_dataset("~/data/IMPACT/full_extract", prefix = "SCB_Periop_")
#' impact$Procedures
#' ```
#'
#' By default the tables are *also* attached to your environment as free-standing
#' objects, so that `Procedures`, `HospitalMar` and the rest can be used by name.
#' This is what most of the package's functions expect by default. Set
#' `attach = FALSE` if you would rather work only through the returned object, and
#' see [attach_dataset()] to attach it later.
#'
#' The `dictionary_check` argument checks the files within the target folder against a dictionary of known accepted table names to ensure dataset quality. Whilst the dataset is in development this may become out of date. If persistent issue then please leave a github issue.
#'
#'
#' @param folder_path Path to the folder containing the parquet files to be loaded
#' @param prefix A string to remove from the start of the parquet file names. If you do not know the suffix, the function may suggest it for you.
#' @param suffix A string to remove from the end of the parquet file names, this should include the '.parquet'
#' @param dictionary_check Check the dataset being loaded against the IMPACT dictionary to ensure data completeness
#' @param attach Also bind each table as a free-standing object in `envir`. See [attach_dataset()].
#' @param envir The environment to attach the tables to, if `attach = TRUE`.
#' @param verbose Print the output of each step
#'
#' @return An [impact_dataset]: a named list of arrow bindings, one per table.
#' @import dplyr
#' @export
#' @seealso [attach_dataset()], [impact_dataset]
#'
#' @examples
#' \dontrun{
#' impact <- load_dataset(folder_path = "~/data/IMPACT/full_extract",
#'             prefix = "SCB_Periop_",
#'             suffix = "_20260122.parquet",
#'             dictionary_check = TRUE,
#'             verbose = TRUE)
#'
#' impact$Procedures
#'
#' # Without touching the global environment
#' impact <- load_dataset(folder_path = "~/data/IMPACT/full_extract",
#'             prefix = "SCB_Periop_",
#'             attach = FALSE)}
load_dataset <- function(
    folder_path ,
    prefix,
    suffix = ".parquet",
    dictionary_check = TRUE,
    attach = TRUE,
    envir = globalenv(),
    verbose = FALSE){

  if(verbose){cli::cli_h1("Importing dataset")}

  # Stage 1 - Data discovery and metadata discovery

  files <- data.frame(path = list.files(folder_path, full.names = TRUE))
  # extract the folder/parquet names from the full path
  files$name <- str_extract(files$path,"[^/]+$")

  if(nrow(files) == 0){cli::cli_abort("No Dataset files found at {.path {folder_path}}. \nCheck folder path and file formats.")}

  if(verbose){cli::cli_alert("{.val {nrow(files)}} parquet files/folders found in {.path {folder_path}} \n First 3 Files: {.val {head(files$name, n=3)}}, ...")}

  # Stage 2 - Naming tables and check against dictionary

  if(verbose){cli::cli_h1("Cleaning table names with Prefix and Suffix")}
  #If suffix or prefix are supplied use these to strip chars from file names to generate names vector.
  if(!missing(prefix) & !missing(suffix)){
    if(verbose){cli::cli_alert("Cleaning file names by removing the {.arg prefix} {.val {prefix}} and the {.arg suffix} {.val {suffix}}")}
    files$name <- gsub(prefix, "",  gsub(suffix, "", files$name), fixed = TRUE)
    }else if(!missing(prefix)){
      files$name <- gsub(prefix, "", files$name, fixed = TRUE)
    }else if(!missing(suffix)){
      files$name <- gsub(suffix, "", files$name, fixed = TRUE)
    }else{
      files$name <- files$name
  }

  #Suffix and Prefix check - To check if our provided Suffix and Prefix are suitable
  #We test with the first value of the Dictionary that we can produce it.

  approved_tables_names <- dictionary_IMPACT_schema[!is.na(dictionary_IMPACT_schema$table_name),"table_name"]

  if(any(dictionary_IMPACT_schema$table_name[1] %in% files$name)){
    #We can cleaned to correctly match the first dict name (usually Demographics)
  }else{
    cli::cli_alert_warning("Struggaling to clean the file names using the {.arg prefix} {.val {prefix}} and the {.arg suffix} {.val {suffix}} provided... ")
    demographic_file_name <- files[grepl(dictionary_IMPACT_schema$table_name[1], files$name),"name"]
    pre_suf_strings <- stringr::str_split(demographic_file_name, dictionary_IMPACT_schema$table_name[1] )
    cli::cli_alert_info("Looks like your {.arg prefix} should be {.val {pre_suf_strings[[1]][1]}} and the {.arg suffix} {.val {pre_suf_strings[[1]][2]}}. \n \n Without this you will either get non-standard table tables (that will fail many of the onward functions, or trigger the missing table errors with the {.arg dictionary_check=TRUE})    ")
    }

  #Check the cleaned table names against the dictonary
  if(dictionary_check){
    if(verbose){cli::cli_h2("Dictonary Check ")}
    if(verbose){cli::cli_alert("Checking table names from dataset ({.val {head(files$name, n=2)}}, ...) against the IMPACT dictonary.")}
    #Check any missing


    if(any(!approved_tables_names$table_name %in% files$name)){
      missing_tables <- setdiff(approved_tables_names$table_name, files$name)
      cli::cli_abort("The table {.val {missing_tables}} is in the IMPACT dictionary but is not found in the dataset you provided.
        \n This may be for a number of reasons:
        \n - You have not set the prefix and suffix correctly (scroll up to see warning message if so)
        \n- The dictonary is out of date. If you think this is the case then you can look at the {.href [online IMPACT Schema](dict_url)} and open a {.href [github issue.](github_url_issues)}\n Alternativly, avoid the check with {.arg dictionary_check = FALSE}")
      }
      #Check for additional tables
    if(any(!files$name %in% approved_tables_names$table_name)){
     additional_tables <-  files$name[which(!files$name %in% approved_tables_names$table_name)]
    cli::cli_abort("The table {.val {additional_tables}} is not in the IMPACT dictionary but is found in the dataset provided.
        \nThis may be because the dataset has been updated by the dictonary itself has not been updated.
        If you think this is the case then you can avoid the check with {.arg dictionary_check} = {.val FALSE}")
    }
  }


  # Check for column name integrity
  ## THis is a placeholder until github issue: https://github.com/SAFEHR-data/IMPACTfunctions/issues/1 is resolved
  ## When we have a updateable schema we can match table AND column names in the logic above.
  ## In the meantime we will apply overwritting to ensure common format.
  ## Missing columns are dellt with using the vignettes and the synthetic dataset (If a col isnt in the synth data, the vignette building will fail. )

  ## Format: NEW NAME = OLD NAME
  col_rename <- c("anaesthesia_csn" = "anesthesia_csn")

  # Stage 3 - Loading data into current R session

  if(verbose){cli::cli_alert("Reading {.val {nrow(files)}} tables")}

  tables <- vector("list", nrow(files))
  names(tables) <- files$name

  for(i in 1:nrow(files)){
    df <- arrow::open_dataset(files[i, "path"])

    # If a col name issue is presne then rename
    if(any(col_rename %in% names(df))){
      #Apply the column renaming
      to_rename <- col_rename[col_rename %in% names(df)]
      cli::cli_alert_warning("WARNING: In the table {.val {files$name[i]}} there is a typo in the column {.val {unname(to_rename)}}. Overwriting to {.val {names(to_rename)}}. ")
      df <- df %>% rename(all_of(to_rename))
    }

    tables[[i]] <- df
  }

  dataset <- new_impact_dataset(tables, source = folder_path)

  # Stage 4 - Optionally bind the tables in the caller's environment.
  # Many functions in this package default to bare table names (for example
  # `extract_medication(MAR = HospitalMar)`), so this is on by default.
  if(attach){
    attached <- attach_dataset(dataset, envir = envir)
    if(verbose){cli::cli_alert("Attached {.val {length(attached)}} tables to the R session enviroment")}
  }

  if(verbose){cli::cli_alert_success("Dataset imported!")}

  dataset
}



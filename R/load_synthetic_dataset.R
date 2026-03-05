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
#'
#' ## Note that you need to assign the synth_data to the global Env for the functions to work.
#' synth_data <- load_synthetic_dataset()
#' list2env(synth_data, envir = .GlobalEnv)
#'
load_synthetic_dataset <- function() {

  # Ensure the data exists in the cache
  generate_synthetic_dataset(clear_cache = FALSE)

  cache_dir <- get_synthetic_cache_dir()

  file_map <- c(
    "Procedures" = "procedures_synthetic.parquet",
    "Demographics" = "demographics_synthetic.parquet",
    "PostOpMAR" = "postopmar_synthetic.parquet",
    "IntraOpMAR" = "intraopmar_synthetic.parquet"
  )

  # 2. Iterate over names(file_map) to ensure the list is named correctly
  synthetic_datasets <- lapply(names(file_map), function(obj_name) {

    # Retrieve the filename using the object name key
    current_file <- file_map[obj_name]
    target_path  <- file.path(cache_dir, current_file)

    if (file.exists(target_path)) {
      # Return the Arrow Table
      return(arrow::read_parquet(target_path, as_data_frame = FALSE))
    } else {
      warning(paste("File for", obj_name, "not found at:", target_path))
      return(NULL)
    }
  })

  # 3. Assign names to the list elements so list2env() works later
  names(synthetic_datasets) <- names(file_map)

  return(synthetic_datasets)
}

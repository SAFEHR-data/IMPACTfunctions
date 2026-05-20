#' get cache
#'
#' @return a directory of the cache location
#' @export
#'
get_synthetic_cache_dir <- function() {
  cache_dir <- tools::R_user_dir("IMPACTfunctions", which = "cache")
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }
  return(cache_dir)
}

#' Generate synthetic dataset
#'
#' This function generates a minimum viable IMPACT dataset that is used for package testing and showcase.
#'
#' The data produces is purely synthetic and uses uniform distributions (such as picking a OPCS code between A01 and Z99) even when values are not feasiable or clinically meaningful.
#' The tables are then saved as parquet format within the R User cache.
#'
#' @param clear_cache Clear the cache and generate synthetic data again.
#'
#' @return A series of Parquet files in the cache directory
#' @export
#'
generate_synthetic_dataset<- function(clear_cache = FALSE){

  #### SYNTHETIC DATASET
  cache_dir <- get_synthetic_cache_dir()

  if(clear_cache){
    unlink(paste0(cache_dir,"/*"))
    cli::cli_alert_info("Generating synthetic dataset, please wait...")
  }

  ## The procedures table is generated first, then the others from there

  path_procedures <- file.path(cache_dir, "procedures_synthetic.parquet")

  if (!file.exists(path_procedures)){
    # If the parquet file cannot be found then generate and save to cache.

  # Generate 10,000 procedures
  n <- 10000
  # Generate a list of CNS to populate the various columns
  csn_list <- sample(c(1000000:2000000), n*4, replace = FALSE)

  #Date ranges
  start_date = "2020-01-01 00:00:00" %>% as.POSIXct()
  end_date = "2025-12-31 23:00:00"%>% as.POSIXct()
  dob_start = "1970-01-01" %>% as.Date()
  dob_end = "2025-01-01" %>% as.Date()
  time_to_surgery = 3
  time_to_discharge = 24

  #Coded_procedures
  code_numbers <- c(paste0(0, 1:9), 10:99)
  procedure_codes <- apply(expand.grid(LETTERS, code_numbers), 1, paste0, collapse="")
  procedure_codes <- apply(expand.grid(procedure_codes, 1:9), 1, paste0, collapse=".")


  synthetic_procedures <- data.frame(mrn = as.character(40000000+sample.int(n*0.8, size = n, replace = TRUE)),
                                     admission_csn = csn_list[1:n],
                                     anaesthesia_csn = csn_list[(n+1):(2*n)],
                                     operation_csn = csn_list[((2*n)+1):(3*n)],
                                     case_id = 1:n,
                                     admission_date = lubridate::with_tz(sample(c(start_date:end_date), n, replace = TRUE) %>% as.POSIXct(), tzone = "UTC"),
                                     operation_date = lubridate::with_tz(NA, tzone = "UTC"),
                                     discharge_date = lubridate::with_tz(NA, tzone = "UTC"),
                                     primary_coded_procedure = sample(procedure_codes, n),
                                     combined_coded_opcs_procedures=NA)

  # Apply column relationships


  synthetic_procedures <- synthetic_procedures %>%
    mutate(operation_date = admission_date + lubridate::minutes(as.integer(rnorm(n = n, mean = time_to_surgery)*60 ))) %>%
    mutate(discharge_date = operation_date + lubridate::minutes(as.integer(rnorm(n = n, mean = time_to_discharge, sd=10)*60))
    ) %>% rowwise() %>%
    mutate(combined_coded_opcs_procedures = {
      # Sample between 0 and 10 codes independently for this row
      additional_codes <- sample(procedure_codes, size = sample.int(10,1, replace=TRUE), replace = TRUE)
      # Combine the primary code with the new samples
      paste(c(primary_coded_procedure, additional_codes), collapse = " | ")
    }) %>%
    ungroup()


  # Apply random drop-out to simulate missing data

  synthetic_procedures <- synthetic_procedures %>%
    mutate(anaesthesia_csn = if_else(runif(n()) < 0.005, NA, anaesthesia_csn)) %>%
    mutate(admission_date = if_else(runif(n()) < 0.005, NA, admission_date)) %>%
    mutate(discharge_date = if_else(runif(n()) < 0.01, NA, discharge_date))

    arrow::write_parquet(synthetic_procedures, path_procedures)

  }

  ##### SYNTHETIC DEMOGRAPHICS
  path_demographics <- file.path(cache_dir, "demographics_synthetic.parquet")

  if (!file.exists(path_demographics)){
  n_unique_mrn <- length(unique(synthetic_procedures$mrn))

  #Taken from ONS 2021 Census - https://www.ethnicity-facts-figures.service.gov.uk/style-guide/ethnic-groups/
  ethnicity_list <- c("Indian", "Pakistani", "Bangladeshi", "Chinese", "Any other Asian background",
                      "Caribbean", "African", "Any other Black, Black British, or Caribbean background",
                      "White and Black Caribbean", "White and Black African", "White and Asian", "Any other Mixed or multiple ethnic background",
                      "English, Welsh, Scottish, Northern Irish or British","Irish", "Gypsy or Irish Traveller","Roma", "Any other White background",
                      "Arab", "Any other ethnic group")

  #Seed the table
  synthetic_demographics <- data.frame(
    mrn = unique(synthetic_procedures$mrn),
    restricted_patient_flag = sample(c("1", "0"), n_unique_mrn, replace = TRUE, prob = c(0.01, 0.99)),
    opt_out_flag = sample(c("1", "0"), n_unique_mrn, replace = TRUE, prob = c(0.01, 0.99)),
    dob = as.Date(sample(c(dob_start:dob_end),n_unique_mrn, replace = TRUE)),
    sex = sample(c("Male", "Female"),n_unique_mrn, replace = TRUE),
    ethnicity = sample(ethnicity_list, n_unique_mrn, replace=TRUE)
  )

  synthetic_demographics <- synthetic_demographics %>%
    mutate(ethnicity = if_else(runif(n()) < 0.2, NA, ethnicity))

    arrow::write_parquet(synthetic_demographics, path_demographics)
  }

  ### SYNTHETIC MAR

  path_hospitalmar <- file.path(cache_dir, "hospital_synthetic.parquet")

  if (!file.exists(path_hospitalmar)){

    intra_op_medications <- c("paracetamol", "sodium lactate compound", "propofol", "fentanyl", "ondansetron", "dexamethasone", "remifentanil", "morphine sulfate" )
    pharmaceutical_class_list <- c("Analgeisa", "Antibiotic")
    pharmaceutical_subclass_list <- c("Simple analgeisa", "Opioid", "NSAID", "Penicillin", "cephalasporin")

  # Each patient will get 5 intra-op meds
  synthetic_intraopmar  <- synthetic_procedures %>%
    slice_sample(n = n*5, replace = TRUE)%>%
    rowwise() %>%
    mutate(
      simple_generic_name = sample(intra_op_medications,size=1, replace = TRUE),
      pharmaceutical_class = sample(pharmaceutical_class_list,size=1, replace = TRUE),
      pharmaceutical_subclass = sample(pharmaceutical_subclass_list,size=1, replace = TRUE),
      dose = sample(c(5,10,20,50,100,500), size = 1, replace = TRUE),
      dose_units = sample(c("mg", "mg/ml"), size=1, prob = c(0.99, 1)),
      administration_route = sample(dictionary_medication_route$administration_route, size=1,  replace = TRUE),
      administration_action = sample(c("Given", "See Alternative", "Held", "Dose documented", NA), size = 1, prob = c(0.7, 0.05, 0.05, 0.1, 0.05)),
    )%>%
    ungroup()%>%
    mutate(
      # Use n() to match the dataframe row count
      # Ensure time_to_surgery is numeric (representing hours)
      administration_offset_minutes = as.integer(rnorm(n = n(), mean = 1, sd=0.3) * 60),
      administration_date_time = lubridate::with_tz(operation_date + lubridate::minutes(administration_offset_minutes), tzone = "UTC")
    )%>%
    select(-c(primary_coded_procedure, combined_coded_opcs_procedures, administration_offset_minutes)) %>%
    mutate(generic_name = toupper(simple_generic_name),
           medication_name = paste(generic_name, dose, dose_units),
           operation_csn = NA)

  post_op_medications <- c("paracetamol", "enoxaparin","ondansetron", "morphine sulfate", "ibuprofen", "dihydrocodeine","fentanyl", "dihydrocodeine", "lactulose")

  synthetic_postopmar  <- synthetic_procedures %>%
    slice_sample(n = n*20, replace = TRUE)%>%
    rowwise() %>%
    mutate(
      simple_generic_name = sample(intra_op_medications,size=1, replace = TRUE),
      pharmaceutical_class = sample(pharmaceutical_class_list,size=1, replace = TRUE),
      pharmaceutical_subclass = sample(pharmaceutical_subclass_list,size=1, replace = TRUE),
      dose = sample(c(5,10,20,50,100,500), size = 1, replace = TRUE),
      dose_units = sample(c("mg", "mg/ml"), size=1, prob = c(0.99, 1)),
      administration_route = sample(dictionary_medication_route$administration_route, size=1,  replace = TRUE),
      administration_date_time = as.POSIXct(
        runif(1, min = as.numeric(admission_date), max = as.numeric(discharge_date)),
        origin = "1970-01-01", tz="UTC"
      ),
      administration_action = sample(c("Given", "See Alternative", "Held", NA), size = 1, prob = c(0.8, 0.05, 0.05, 0.05)),
    )%>%
    ungroup()%>%
    select(-c(primary_coded_procedure, combined_coded_opcs_procedures)) %>%
    na.omit()%>%
    mutate(generic_name = toupper(simple_generic_name),
           medication_name = paste(generic_name, dose, dose_units),
          anaesthesia_csn = NA,
           operation_csn = NA)

  operative_medications <- c("lidocaine", "prilocain","lidocaine with adrenaline", "saline", "keterolac")

  synthetic_operative_mar  <- synthetic_procedures %>%
    slice_sample(n = 0.5*n, replace = TRUE)%>%
    rowwise() %>%
    mutate(
      simple_generic_name = sample(operative_medications,size=1, replace = TRUE),
      pharmaceutical_class = sample(pharmaceutical_class_list,size=1, replace = TRUE),
      pharmaceutical_subclass = sample(pharmaceutical_subclass_list,size=1, replace = TRUE),
      dose = sample(c(5,10,20), size = 1, replace = TRUE),
      dose_units = "mg",
      administration_route = sample(c("local", "intra-articular"), size=1,  replace = TRUE),
      administration_action = sample(c("Given", "See Alternative", "Held", NA), size = 1, prob = c(0.8, 0.05, 0.05, 0.05))
    )%>%
    ungroup()%>%
    mutate(  # Use n() to match the dataframe row count
      # Ensure time_to_surgery is numeric (representing hours)
      administration_offset_minutes = as.integer(rnorm(n = n(), mean = 1, sd=0.3) * 60),
      administration_date_time = lubridate::with_tz(operation_date + lubridate::minutes(administration_offset_minutes), tzone = "UTC")) %>%
    select(-c(primary_coded_procedure, combined_coded_opcs_procedures,administration_offset_minutes)) %>%
    na.omit()%>%
    mutate(generic_name = toupper(simple_generic_name),
           medication_name = paste(generic_name, dose, dose_units),
           anaesthesia_csn = NA)

 synthetic_hospitalmar <- rbind(synthetic_intraopmar, synthetic_postopmar, synthetic_operative_mar)

 synthetic_hospitalmar <- synthetic_hospitalmar %>%
   mutate(mar_id = sample(c(1000000:2000000), size=nrow(synthetic_hospitalmar), replace = FALSE))

  arrow::write_parquet(synthetic_hospitalmar, path_hospitalmar)
  }


 # synthetic_measurments

  path_measurements <- file.path(cache_dir, "measurements_synthetic.parquet")

  if (!file.exists(path_measurements)){

  flowsheet_question_name <- c(
    "Verbal pain score - at rest", "Verbal pain score - on movement",
    "BP", "Pulse", "Size", "Laterality", "Skin decontamination",
    "Reason for Art Line", "Orientation", "Placement Date"
  )

  flowsheet_probs <- c(3021570, 2342216, 6519997, 30411508, 89598, 38876, 66744, 3883, 355959, 930516)
  flowsheet_probs <- flowsheet_probs / sum(flowsheet_probs)

  flowsheet_id <- 10000000:10000009
  flowsheet_row_epic_id <- as.character(50000000:50000009)

  flowsheet_id_df <- data.frame(flowsheet_question_name, flowsheet_id, flowsheet_row_epic_id)

  synthetic_measurements <- synthetic_procedures %>%
    slice_sample(n = n * 20, replace = TRUE) %>%
    rowwise() %>%
    mutate(
      lda_id = NA,
      flowsheet_question_name = sample(flowsheet_question_name, size = 1, prob = flowsheet_probs, replace = TRUE),
      value = sample(0:200, size=1),
      measurement_type_id = sample(1:10, size=1),
      ) %>%
    ungroup() %>%
    mutate(recorded_date = as.POSIXct(
      runif(1, min = as.numeric(admission_date), max = as.numeric(discharge_date)),
      origin = "1970-01-01", tz="UTC"
    )) %>%
    select(-c(primary_coded_procedure, combined_coded_opcs_procedures, admission_date, operation_date, discharge_date, mrn))%>%
    left_join(flowsheet_id_df)

  synthetic_measurements <- synthetic_measurements %>%
    mutate(anaesthesia_csn = if_else(runif(n()) < 0.8, NA, anaesthesia_csn)) %>%
    mutate(operation_csn  = if_else(runif(n()) < 0.9, NA, operation_csn)) %>%
    mutate(case_id  = if_else(runif(n()) < 0.9, NA, case_id )) %>%
    mutate(lda_id   = if_else(runif(n()) < 0.5, NA, lda_id  ))

  arrow::write_parquet(synthetic_measurements, path_measurements)
  }

}

#### SYNTHETIC DATASET

# Generate 10,000 procedures
n <- 10000
# Generate a list of CNS to populate the various columns
csn_list <- sample(c(1000000:2000000), n*4, replace = FALSE)

#Date ranges
start_date = "2020-01-01 00:00:00" %>% as.POSIXct()
end_date = "2025-12-31 23:00:00"%>% as.POSIXct()
time_to_surgery = 3
time_to_discharge = 24

#Coded_procedures
code_numbers <- c(paste0(0, 1:9), 10:99)
procedure_codes <- apply(expand.grid(LETTERS, code_numbers), 1, paste0, collapse="")
procedure_codes <- apply(expand.grid(procedure_codes, 1:9), 1, paste0, collapse=".")


synthetic_data <- data.frame(mrn = as.character(40000000+sample.int(n*0.8, size = n, replace = TRUE)),
                             admission_csn = csn_list[1:n],
                             anaesthesia_csn = csn_list[(n+1):(2*n)],
                             operation_csn = csn_list[((2*n)+1):(3*n)],
                             case_id = 1:n,
                             admission_date = sample(c(start_date:end_date), n, replace = TRUE) %>% as.POSIXct(),
                             operation_date = NA,
                             discharge_date = NA,
                             primary_coded_procedure = sample(procedure_codes, n),
                             combined_coded_opcs_procedures=NA)

# Apply column relationships


synthetic_data <- synthetic_data %>%
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

synthetic_data <- synthetic_data %>%
  mutate(anaesthesia_csn = if_else(runif(n()) < 0.005, NA, anaesthesia_csn)) %>%
  mutate(admission_date = if_else(runif(n()) < 0.005, NA, admission_date)) %>%
  mutate(discharge_date = if_else(runif(n()) < 0.01, NA, discharge_date))


arrow::write_parquet(synthetic_data, "inst/extdata/procedures_synthetic.parquet")


n_unique_mrn <- length(unique(synthetic_data$mrn))

synthetic_demographics <- data.frame(
  mrn = unique(synthetic_data$mrn),
  restricted_patient_flag = sample(c("1", "0"), n_unique_mrn, replace = TRUE, prob = c(0.01, 0.99)),
  opt_out_flag = sample(c("1", "0"), n_unique_mrn, replace = TRUE, prob = c(0.01, 0.99))
)

arrow::write_parquet(synthetic_demographics, "inst/extdata/demographic_synthetic.parquet")


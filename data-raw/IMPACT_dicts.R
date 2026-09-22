### This dataset is a placeholder for a agreeded common dataset.
## Google sheets offers an easy to access and edit format across a range of applications.
## It is hosted at: https://docs.google.com/spreadsheets/d/1u1p7TlS8tmzGzNCIWHRg2Nn85YzLGfeF1bR5bbnQLbE/edit?gid=0#gid=0

dict_url <-  "https://docs.google.com/spreadsheets/d/1u1p7TlS8tmzGzNCIWHRg2Nn85YzLGfeF1bR5bbnQLbE/edit?gid=0#gid=0"

token <- googlesheets4::gs4_token()
googledrive::drive_auth(token = token)
googledrive::drive_has_token()

### Once authentication approved, read the Google sheets and package as data.
###

dictionary_IMPACT_schema <- googlesheets4::read_sheet(dict_url, sheet = "dictionary_IMPACT_schema")
dictionary_flowsheet_pca <- googlesheets4::read_sheet(dict_url, sheet = "dictionary_flowsheet_pca")
dictionary_medication_route <- googlesheets4::read_sheet(dict_url, sheet = "dictionary_medication_route")
dictionary_opioid <- googlesheets4::read_sheet(dict_url, sheet = "dictionary_opioid")
dictionary_equivelence <- googlesheets4::read_sheet(dict_url, sheet = "dictionary_equivelence")

usethis::use_data(dict_url, overwrite = T)

usethis::use_data(dictionary_IMPACT_schema, overwrite = T)
usethis::use_data(dictionary_flowsheet_pca, overwrite = T)
usethis::use_data(dictionary_medication_route, overwrite = T)
usethis::use_data(dictionary_opioid, overwrite = T)
usethis::use_data(dictionary_equivelence, overwrite = T)

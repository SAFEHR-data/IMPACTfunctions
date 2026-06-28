# Frequently Asked Questions and Troubleshooting. 

This is a guide to any issues you may encounter. If you run into an issue and 
solve it, please add below!


## `load_dataset()` function gives error about table names not being in the IMPACT dict

In an attempt to ensure standardisation we have a set of approved table names. 
They are housed on a google sheet: https://docs.google.com/spreadsheets/d/1u1p7TlS8tmzGzNCIWHRg2Nn85YzLGfeF1bR5bbnQLbE/edit?gid=0#gid=0

The error is triggered if tables are missing, added or named wrong. This may be becuase your specific IMPACT extract doesn't conform with the agreed table name. This may also be because the dictonary is out of date. 

To resolve the error you should follow the following steps:

1. Establish the agreed name of the table across the IMPACT network

2. Log a github issue to update the dictionary (This is important as other functions may break if table names change).

3. Whilst waiting for the new version to be released, use the `dictionary_check = FALSE` to overule the check. 

4. When the dictionary is updated, install the latest version of the package and then rerun the function.


> Developer notes
>
> When a github issue request comes, ensure the following:
> 1. Update the names in the google sheet to match the latest copy
> 2. Ensure any function use the updated table name. Use github search to help find the correct files. 
> 3. Run `load_all()` then `generate_synthetic_dataset(clear_cache = TRUE)` to regenerate the synthetic dataset. 
> 4. Open `data-raw/IMPACT_dicts.R` and run the script (including logging into google) to refresh the data files.
> 5. Run `check()` to ensure vignetts still build then `use_version()` and open PR against the issue. 
> 6. Inform users they will need to update the package with the new dictonaries. 

## ERROR MESSAGE: object 'dictionary_IMPACT_schema' not found

This is due to the IMPACT dictionaries not being pulled from the google sheets prior to the build of the version. This means that the package hasnt been build with the dict. 

To resolve the error you should follow the following steps:

1. Log a github urgently - This issue will be effecting everyone who had installed this version of the package

2. Install an older package version as a temporary fix

- Navigate to [the commit history](https://github.com/SAFEHR-data/IMPACTfunctions/commits/main/) for the main branch

- Pick a older committ that will likley work. Look for "Increment version number to ..." and pick one of these

- Copy the full SHA for the commit (the two overlapping boxes to the right)

- Within the `install_github`function add the following and paste the SHA to the `ref` arguement.  

```
remotes::install_github("SAFEHR-data/IMPACTfunctions", force=TRUE, 
                        ref = "67a2a6fe3467adc14bd3288e64dc9f5acef40c28"
                        auth_token = gitcreds::gitcreds_get()$password)
```

3. Await for the issue to be fixed and then install the latest package version.



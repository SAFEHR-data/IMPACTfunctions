
# IMPACTfunctions

<!-- badges: start -->
<!-- badges: end -->

The goal of IMPACTfunctions is to help working with the IMPACT peri-operative dataset. 

The dataset was built at UCLH with input from CUH NHS trust and others...

## Installation

You can install the development version of IMPACTfunctions from [GitHub](https://github.com/) with:

``` r

# As it is a private repo within the SAFEHR-data group you will need a PAT. 
# Follow this guide to get one: https://uclh.slab.com/posts/git-hub-integration-6s9bw63t

# Then set that to your R session:
gitcreds::gitcreds_set()

# Install package from github:
remotes::install_github("SAFEHR-data/IMPACTfunctions", force=TRUE, 
                        auth_token = gitcreds::gitcreds_get()$password)


```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(arrow)
library(cli)
library(dplyr)
library(IMPACTfunctions)

load_dataset(folder_path = "~/data/dwhitake/data/IMPACT/20260122",
             prefix = "SCB_Periop_",
             suffix ="_20260122.parquet",
             verbose = TRUE)

Demographics %>% glimpse()

```



<!-- README.md is generated from README.Rmd. Please edit that file -->

# dataRC <img src="man/figures/logo.png" align="right" height="138" /></a>

<!-- badges: start -->
<!-- badges: end -->

`dataRC` is an R package designed to bring efficient data management
technologies to everyone. It aims to enhance efficiency in data handling
by providing easy-to-use tools for converting files to Apache Parquet
format, unifying heterogeneous databases, providing templates for data
processing and more. Whether you have little to none programming
experience or are an advanced user, `dataRC` simplifies repetitive
processes and boosts your productivity.

## Installation

At present, installation of the package is only supported from GitHub.
However, as the package adheres to CRAN’s best practices, we anticipate
its availability on CRAN in the near future.

``` r
# install.packages("devtools")
devtools::install_github("jdrengifoc/dataRC")
```

## Usage

``` r
library(dataRC)

# Convert all the .dta, .txt, and .csv files in the current folder into Parquet
# format and store them in the folder ./parquet_files.
# convert_files(
#   folder = ".", files = list.files(pattern = '(dta|txt|csv)$'), 
#   new_extension = "parquet", new_folder = '/parquet_files')

# Create a partial dictionary to ease data homogenization without making
# unexpected changes to original data.
# dict_path <- 'dict.xlsx'
# create_partial_dictionary(
#   folder = '/parquet_files', files = list.files(), dict_path, verbose = F)
# Add descriptive statistics and sort the partial dictionary for final manual
# review.
# sort_partial_dictionary(dict_path, overwrite = T)
```

## Getting help

If you encounter a clear bug, please file an issue with a minimal
reproducible example on
[GitHub](https://github.com/jdrengifoc/dataRC/tree/main). If you don’t
know how to do this or have a suggestion, please feel free to write an
email to <jdrengifoc@eafit.edu.co>.
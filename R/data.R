# Recurrent Event Sample Data ====

#' Recurrent event sample data
#'
#' Data is from a outcomes study on cardiovascular outcomes.
#' It contains the first visit date, the last known date, and
#' times of various events that have happened. They document death
#' at right censoring as well. These events are non-ordered.
#'
#' @docType data
#' @format An tibble data frame
#' @keywords datasets
"stress"

# Clinical Variables and Dyx Hourly Data {{{ ====

#' Hourly time series data with clinical covariates
#'
#' Data is from an algorithm that generates a summary HRV measure using the Poincare phase-space plot, generated from kurtoses of the x and y axis. Clinical data is also available for visualization and comparison. There are repeat rows for each hour that Dyx was taken.
#'
#' @docType data
#' @format An tibble data frame
#' @keywords datasets
"twins"

# Clinical Variables and Dyx Hourly Data ====

#' Hourly time series data with clinical covariates
#'
#' Clinical data is also available for visualization and comparison. Other HRV measures are used here for comparison and testing out functions.
#'
#' @docType data
#' @format A `tbl_df`
#' @keywords datasets
"triplets"

# Global Electrical Heterogeneity Data ====

#' GEH parameters in a large clinical cohort
#'
#' Used in the model-building examples for repeat testing.
#'
#' @docType data
#' @format A tibble
#' @keywords datasets
"geh"

# Output from MATLAB HRV Toolbox ====

#' Output from MATLAB HRV Toolbox
#'
#' Data is a single patient data output from HRV Toolbox. It contains granular data of calculated HRV in 5-second sliding windows.
#'
#' @docType data
#' @format An tibble data frame
#' @keywords datasets
"hrv"

# Zipcodes with Associated Latitude and Longitude ====

#' Zipcodes with Associated Latitude and Longitude
#'
#' This is a dataset from the archived/orphaned {zipcode} package.
#'
#' @docType data
#' @format A data frame with character vector zipcodes and latitude/longitude
#' @keywords datasets
"zipcode"

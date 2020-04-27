# Recurrent Event Sample Data {{{ ====

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
"mims"

# }}}

# Clinical Variables and Dyx Hourly Data {{{ ====

#' Hourly time series data with clinical covariates
#'
#' Data is from an algorithm that generates a summary HRV measure using the Poincare phase-space plot, generated from kurtoses of the x and y axis. Clinical data is also available for visualization and comparison. There are repeat rows for each hour that Dyx was taken.
#'
#' @docType data
#' @format An tibble data frame
#' @keywords datasets
"twins"

# }}}

# Output from MATLAB HRV Toolbox {{{ ====

#' Output from MATLAB HRV Toolbox
#'
#' Data is a single patient data output from HRV Toolbox. It contains granular data of calculated HRV in 5-second sliding windows.
#'
#' @docType data
#' @format An tibble data frame
#' @keywords datasets
"hrv"

# }}}
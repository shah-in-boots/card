# Process Toolbox HRV {{{ ====

#' @title Process Toolbox HRV
#' @description `read_matlab_hrv` takes the output from HRV Toolbox and
#'   converts it for analysis. Uses the package [data.table::fread()]
#'   for reading in data due to size/speed.
#' @details The data is taken sequentially (sliding windows), and
#'   summarized over the course of certain time lengths. The data comes
#'   in a standardized pattern from the toolbox. It requires processing
#'   due to its large file sizes (e.g. 24 hours of data for a single
#'   patient can be up to 2 MB in size).
#' @param loc Location of the folder that contains all of the patients
#'   that were analyzed by the Main_HRV_Analysis.m function from the
#'   Toolbox.
#' @param name Name of the patient/ID. There should exist a folder with
#'   the name inside the `loc` folder. Inside this folder are all the
#'   Toolbox parameters and HRV results in CSV format.
#' @param time Number of seconds to group the HRV data by. Defaults to
#'   3600 seconds (which is 1 hour)
#' @return Data frame of HRV summarized by the grouping variable (e.g.
#'   3600 seconds = 1 hour). Also returns an additional column of
#'   percent missing (e.g. 20.0% missing data) by time group.
#' @import data.table
#' @examples
#' proc_hrv_matlab(location, patid, hrv_variables, 3600)
#'
#' # Assuming in "root/code" folder, need to get to data in STUDY
#' parent <- dirname(getwd())
#' loc <- file.path(parent, 'data', 'proc_hrv', 'STUDY')
#' vars <- Hmisc::Cs(SDNN, RMSSD, pnn50, ulf, vlf, lf, hf, lfhf, ttlpwr, ac, dc, SampEn, ApEn)
#' time <- 3600 # number of seconds in an hour
#' names <- list.files(path = loc)
#'
#' # Create for loop to eventually make data frame
#' d <- list()
#' for(i in 1:length(names)) {
	#' d[[paste(names[i])]] <- proc_hrv_matlab(loc, names[i], vars, time)
#' }
#' df <- rbindlist(d) %>% as_tibble()
#' @export
proc_hrv_matlab <- function(loc, name, time = 3600) {

	# Selected variables from HRV analysis
	svar <- c("NNmean", "SDNN", "RMSSD", "pnn50", "ulf", "vlf", "lf", "hf", "lfhf", "ttlpwr", "ac", "dc", "SampEn", "ApEn")
	vars <- c("patID", "t_start", "t_end", svar)

	# Read in the HRV data
	dt <-
		list.files(file.path(loc, name), pattern = "HRV", full.name = TRUE) %>%
		fread()

	# Time grouping variable
	### Need to add different increments
	tvar <- ifelse(time/3600 == 1, "hour")

	# Create a summary data set by grouping times
	x <-
		dt[, vars, with = FALSE
		   ][, paste0("t_",tvar) := floor(t_start/time)
		     ][, lapply(.SD, mean, na.rm=T), by=eval(paste0("t_", tvar)), .SDcols=svar]

	# Identify missing percent by group (for quality)
	y <-
		dt[, vars, with = FALSE
		   ][, paste0("t_", tvar) := floor(t_start/time)
		     ][, list(missing=(sum(is.na(SDNN)) / .N * 100)), by=eval(paste0("t_", tvar))]

	# Keys for merging
	setkeyv(x, paste0("t_", tvar))
	setkeyv(y, paste0("t_", tvar))
	z <- na.omit(x[y])

	# Add patid
	z <- cbind(patid = name, z)

	# Return data table
	return(z)
}

# }}}

# Read in Toolbox HRV {{{ ====


#' @title Read in Toolbox HRV
#' @description `read_hrv_matlab` takes the output from HRV Toolbox and
#'   reads it in for an individual patient. Unlike
#'   \code{\link{proc_hrv_matlab}}, this does not process or summarize
#'   the data, it just reads it in raw analysis. Uses the package
#'   [data.table::fread()] for reading in data due to size/speed.
#' @details The data is taken sequentially (sliding windows), and is not processed. It is reported and is likely a large file.
#' @param loc Location of the folder that contains all of the patients
#'   that were analyzed by the Main_HRV_Analysis.m function from the
#'   Toolbox
#' @param name Name of the patient/ID. There should exist a folder with
#'   the name inside the `loc` folder. Inside this folder are all the
#'   Toolbox parameters and HRV results in CSV format.
#'   3600 seconds (which is 1 hour)
#' @return Data frame of HRV summarized by the grouping variable (e.g.
#'   3600 seconds = 1 hour). Also returns an additional column of
#'   percent missing (e.g. 20.0% missing data) by time group.
#' @import data.table
#' @examples
#' # Assuming in "root/code" folder, need to get to data in STUDY
#' parent <- dirname(getwd())
#' loc <- file.path(parent, 'data', 'proc_hrv', 'STUDY')
#' vars <- Hmisc::Cs(SDNN, RMSSD, pnn50, ulf, vlf, lf, hf, lfhf, ttlpwr, ac, dc, SampEn, ApEn)
#' time <- 3600 # number of seconds in an hour
#' names <- list.files(path = loc)
#'
#' # Create for loop to eventually make data frame
#' d <- list()
#' for(i in 1:length(names)) {
	#' d[[paste(names[i])]] <- read_hrv_matlab(loc, names[i], vars, time)
#' }
#' df <- rbindlist(d) %>% as_tibble()
#' @export
read_hrv_matlab <- function(loc, name) {

	# Selected variables from HRV analysis
	svar <- c("NNmean", "SDNN", "RMSSD", "pnn50", "ulf", "vlf", "lf", "hf", "lfhf", "ttlpwr", "ac", "dc", "SampEn", "ApEn")
	vars <- c("patID", "t_start", svar)

	# Read in the HRV data
	dt <-
		list.files(file.path(loc, name), pattern = "HRV", full.name = TRUE) %>%
		fread()

	# Create a summary data set by grouping times
	x <- dt[, vars, with = FALSE]

	# Return data table
	return(x)
}


# }}}

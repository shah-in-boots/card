# Process Toolbox HRV {{{ ====

#' @title Process Toolbox HRV
#' @description
#' `matlab_process_hrv` takes the output from HRV Toolbox and converts it for analysis. Uses the \code{\link[data.table]} for reading in data due to size/speed.
#' @details The data is taken sequentially (sliding windows), and summarized over the course of certain time lengths. The data comes in a standardized pattern from the toolbox. It requires processing due to its large file sizes (e.g. 24 hours of data for a single patient can be up to 2 MB in size).
#' @param loc Location of the folder that contains all of the patients that were analyzed by the Main_HRV_Analysis.m function from the Toolbox.
#' @param name Name of the patient/ID. There should exist a folder with the name inside the `loc` folder. Inside this folder are all the Toolbox parameters and HRV results in CSV format.
#' @param vars Vector of the HRV variables to be collected
#' @param time Number of seconds to group the HRV data by
#' @return Data frame of HRV summarized by the grouping variable (e.g. 3600 seconds = 1 hour). Also returns an additional column of percent missing (e.g. 20.0% missing data) by time group.
#' @examples
#' matlab_process_hrv(location, patid, hrv_variables, 3600)
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
	#' d[[paste(names[i])]] <- matlab_process_hrv(loc, names[i], vars, time)
#' }
#' df <- rbindlist(d) %>% as_tibble()
#' @export
matlab_process_hrv <- function(loc, name, vars, time) {

	# Selected HRV vars
	svar <- c("t_start", "t_end", vars)

	# Read in the HRV data
	dt <-
		list.files(file.path(loc, name), pattern = "HRV", full.name = TRUE) %>%
		fread()

	# Create a summary data set by grouping times
	x <-
		dt[, svar, with = FALSE
		   ][, paste0("t_",time) := floor(t_start/time)
		     ][, lapply(.SD, mean, na.rm=T), by=eval(paste0("t_", time)), .SDcols=vars]

	# Identify missing percent by group (for quality)
	y <-
		dt[, svar, with = FALSE
		   ][, paste0("t_",time) := floor(t_start/time)
		     ][, list(missing=(sum(is.na(SDNN)) / .N * 100)), by=eval(paste0("t_",time))]

	# Keys for merging
	setkeyv(x, paste0("t_", time))
	setkeyv(y, paste0("t_", time))
	z <- x[y]

	# Add patid
	z <- cbind(patid = name, z)

	# Return data table
	return(z)
}


# }}}

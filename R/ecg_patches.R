# Read in VivaLNK Patch Data {{{ ====

#' @title Read in VivaLNK Patch Data
#' @description
#' `read_vivalnk_patch` Reads in VivaLNK log files, raw, and helps to organize the summary data.
#' @details Does not obtain the ECG signal, just the summary data from the log file for diagnostic purposes.
#' @param name Name of the patient that the log file was created on. The variable does NOT have an extension.
#' @param loc The raw, unprocessed data folder containining the log file (which is in a ".txt" format)
#' @return Returns a simple data frame wiith the start and endtime of the ECG data. Has several additional markers available internally, but currently not called.
#' @example
#' # Apply fn to each element of vector
#' for(i in seq_along(patid)) {
#'
#' 	# Print out line
#' 	print(patid[i])
#'
#' 	# Extraction of data
#' 	x <- patch_read_vivalnk(patid[i])
#'
#' 	# Make data frame
#' 	df <- data.frame(
#' 		'patid' = patid[i],
#' 		'Start' = x[[1]],
#' 		'End' = x[[2]],
#' 		'Duration' = x[[3]]
#' 	)
#'
#' 	# Write this to a file, appending as we go
#' 	write_csv(df, file.path(proc_folder, 'vivalnk_data.csv'), append = TRUE)
#' }
#' @export
read_vivalnk_patch <- function(name, loc) {
	# Read in file name
	tmp <- read_delim(file.path(loc, paste0(name, '.txt')), delim = '\n', col_names = FALSE)

	# Structure is a very tall, tall tibble. Extract only relevant rows
	time <- tmp$X1[grep('Sample', tmp$X1)]
	rr <- tmp$X1[grep('RRI', tmp$X1)]

	# Combine into dataframe
	# Split into columns to help extract time stamps
	df <-
		# combine into a single data frame
		inner_join(enframe(time, value = 'time'), enframe(rr, value = 'RR'), by = 'name') %>%
		# Split strings into components
		separate(time, sep = ',',
				 into = c('index', 'datetime', 'lead', 'flash', 'hr', 'resp', 'activity', 'mag'),
				 remove = TRUE) %>%
		# Extract time
		separate(index, into = c('sample', 'index'), sep = '=', remove = TRUE, convert = TRUE) %>%
		# Convert date time column later
		separate(datetime, into = c('trash', 'datetime'), sep = '=', remove = TRUE, convert = TRUE) %>%
		# Pull HR into BPM
		separate(hr, into = c('hr', 'bpm'), sep = '=', remove = TRUE, convert = TRUE)  %>%
		# Respiratory rate
		separate(resp, into = c('rr', 'resp'), sep = '=', remove = TRUE, convert = TRUE)

	# Convert date time format, but need to preserve miliseconds
	options(digits.secs = 3)
	df$datetime %<>% ymd_hms()

	# Extract the RR intervals as well
	df$RR <- str_extract(df$RR, '\\d+') %>% as.integer(.)

	# Select relevant columns
	df <- df[c('index', 'datetime', 'bpm', 'resp', 'RR')]

	# Final form of vivalnk raw text information
	df <- df[order(df$index),]

	# Return start time, end time, and length in list
	startTime <- head(df$datetime, 1)
	endTime <- tail(df$datetime, 1)
	lengthECG <-
		interval(startTime, endTime) %>%
		time_length(., 'hours')
	x <- list(startTime, endTime, lengthECG)
	return(x)
}

# }}}

# }}}
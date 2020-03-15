# Script to define function for recurrent survival events
# Will require dates of events, types of recurrent table, etc

# Input
	# patient ID column
	# enrollment date (first date available on left)
	# last known date of follow up
	# dates of events
	# type of survival table to create (marginal, conditional, etc)
	# Optional = death parameter
# Output
	# Survival table

#' Create data table of recurrent events for survival analysis
#' @param data A dataframe containing the subsequent parameters
#' @param id Column in dataframe that contains unique IDs for each row
#' @param first Column with left/enrollment dates
#' @param last Column with right/censoring time point, or last contact
#' @param event.dates Vector of columns that contain event dates
#' @param model.type Character/string = c("marginal", "pwptt", "pwpgt")
#' @param death Column created if death is known (0 or 1), Must include in original dataframe (e.g. can add column of zeroes prn)
#' @return A data frame organized into a survival table format
#' @examples
#' recurrent_survival_table(tibble, id, first.date, last.known.date, c(event.dates), "marginal", death.status)
#' @export
recurrent_survival_table <- function(data, id, first, last, event.dates, model.type, death=NULL) {

	# Check for missing optional parameter of death
	# Creates minimally required table
	if(is.null(death)) {
		df <- data[c(id, first, last, event.dates)]
		df$death <- 0
	} else {
		df <- data[c(id, first, last, event.dates, death)]
	}

	# Identify number of events
	n <- 0:length(event.dates)
	events <- paste0("EVENT_DATE_", c(1:(length(n) - 1)))

	# Event dates need to be organized by actual values
	x <- df[c(id, event.dates)] %>%
		pivot_longer(-c(id), names_to = "EVENT", values_to = "DATE") %>%
		group_by_(id) %>%
		arrange(DATE) %>%
		arrange_(id) %>%
		nest()

	# Unique and sorted events and correct event numbering
	for(i in 1:length(x[[id]])) {
		# Make duplicate events NA values
		# Replace duplicates with NA if duplicates exist
		if (!empty(x[[i,2]][duplicated(x[[i,2]]$DATE),])) {
			x[[i,2]][duplicated(x[[i,2]]$DATE),]$DATE <- NA
		}

		# Arrange in correct order
		x[[i,2]] %<>% arrange(DATE)

		# Call each tibble for each ID number and rename the events
		x[[i,2]]$EVENT <- events
	}

	# Make table wide again and add back other columns
	df <-
		unnest(x) %>%
		pivot_wider(names_from = "EVENT", values_from = "DATE") %>%
		inner_join(df[c(id, first, last, death)], ., by = id)

	# Rename for cleaner manipulations
	names(df) <- c("ID", "FIRST", "LAST", "DEATH", events)

	# EVENT_DATE_0 = most recent event (could be enrollment date)
	df$EVENT_DATE_0 <- df$FIRST

	# For loop for identifying most recent event per patient
	for(i in 1:length(n)) {

		# Identify most recent event by taking non-NA dates
		df$EVENT_DATE_0[!is.na(df[paste0("EVENT_DATE_", n[i])])] <-
			df[[paste0("EVENT_DATE_", n[i])]][!is.na(df[paste0("EVENT_DATE_", n[i])])]

	}

	# Each row should be an individual event (or non-event)
	x <-
		df[c("ID", paste0("EVENT_DATE_", n))] %>%
		pivot_longer(-c("ID"), names_to = "EVENT", values_to = "DATE") %>%
		na.omit() %>%
		left_join(df, ., by = "ID")

	# THis leads to "x" which is both long and wide (events by name, and in each column)
	# This data set can be moved into the switch operations to create appropriate recurrent model

	# Now switch to options based on type of data
	switch(
		model.type,

		# Marginal order
		marginal = {
			# Initialize survival table data
			x$STATUS <- x$TSTART <- x$TSTOP <- 0

			### TSTOP

			# Start time is always at beginning for marginal model
			# Stop time needs to be generated as for EVENTS (>0)
			# TIME = DATE OF EVENT - FIRST
			for(i in 2:length(n)) {
				x$TSTOP[x$EVENT == paste0("EVENT_DATE_", n[i])] <-
					x[[paste0("EVENT_DATE_", n[i])]][x$EVENT == paste0("EVENT_DATE_", n[i])] - x$FIRST[x$EVENT == paste0("EVENT_DATE_", n[i])]
			}

			# Stop time from most recent event (EVENT_DATE_0)
			# For no events.. sames as LAST - FIRST
			# Otherwise its LAST - MOST RECENT EVENT DATE
			x$TSTOP[x$EVENT == "EVENT_DATE_0"] <-
				x$LAST[x$EVENT == "EVENT_DATE_0"] - x$FIRST[x$EVENT == "EVENT_DATE_0"]

			### STATUS

			# For those had death as final event in their life
			x$STATUS[x$EVENT == "EVENT_DATE_0" & x$DEATH == 1] <- 1

			# For those that had just an event
			x$STATUS[x$EVENT != "EVENT_DATE_0"] <- 1

		},

		# PWP-TT model (conditional A)
		pwptt = {
			#  Initialize model
			x$STATUS <- x$TSTART <- x$TSTOP <- 0

			### TSTOP

			# For "final" (most recent) event (EVENT_DATE_0)
			x$TSTOP[x$EVENT == "EVENT_DATE_0"] <-
				x$LAST[x$EVENT == "EVENT_DATE_0"] -
				x$FIRST[x$EVENT == "EVENT_DATE_0"]

			# Stop time is the same as marginal stop time
			for(i in 2:length(n)) {
				x$TSTOP[x$EVENT == paste0("EVENT_DATE_", n[i])] <-
					x[[paste0("EVENT_DATE_", n[i])]][x$EVENT == paste0("EVENT_DATE_", n[i])] - x$FIRST[x$EVENT == paste0("EVENT_DATE_", n[i])]
			}

			### TSTART

			# start for very first event = 0
			# Start for subsequent events are stop time of prior


			# For the "most recent event", EVENT_DATE_0, can either..
				# A = person with no events, thus DATE=FIRST,
					# TSTART = DATE - FIRST = 0
				# B = person with some events, last event is DATE
					#  TSTART = DATE - FIRST =/= 0
			x$TSTART[x$EVENT == "EVENT_DATE_0"] <-
				x$EVENT_DATE_0[x$EVENT == "EVENT_DATE_0"] - x$FIRST[x$EVENT == "EVENT_DATE_0"]

			# Determine start time for recurrent events
			for (i in 2:length(n)) {
				# Everything but first event has time-dependent TSTART
				if (n[i] == 1) {
					# Start of first event is always 0
					x$TSTART[x$EVENT == paste0("EVENT_DATE_", n[i])] <- 0
				} else {
					# START of new event is STOP of last event
					x$TSTART[x$EVENT == paste0("EVENT_DATE_", n[i])] <-
						as.numeric(unlist(x[x$EVENT == paste0("EVENT_DATE_", n[i]), paste0("EVENT_DATE_", n[i-1])] - x[x$EVENT == paste0("EVENT_DATE_", n[i]), "FIRST"]))
				}
			}

			### STATUS

			# For those had death as final event in their life
			x$STATUS[x$EVENT == "EVENT_DATE_0" & x$DEATH == 1] <- 1

			# For those that had just an event
			x$STATUS[x$EVENT != "EVENT_DATE_0"] <- 1

		},

		# PWP-GT model (conditional B)
		pwpgt = {
			#  Initialize model
			x$STATUS <- x$TSTART <- x$TSTOP <- 0

			### TSTOP

			# For "final" (most recent) event (EVENT_DATE_0)
			x$TSTOP[x$EVENT == "EVENT_DATE_0"] <-
				x$LAST[x$EVENT == "EVENT_DATE_0"] -
				x$FIRST[x$EVENT == "EVENT_DATE_0"]

			# Stop time is the same as marginal stop time
			for(i in 2:length(n)) {
				x$TSTOP[x$EVENT == paste0("EVENT_DATE_", n[i])] <-
					x[[paste0("EVENT_DATE_", n[i])]][x$EVENT == paste0("EVENT_DATE_", n[i])] - x$FIRST[x$EVENT == paste0("EVENT_DATE_", n[i])]
			}

			### TSTART

			# start for very first event = 0
			# Start for subsequent events are stop time of prior


			# For the "most recent event", EVENT_DATE_0, can either..
				# A = person with no events, thus DATE=FIRST,
					# TSTART = DATE - FIRST = 0
				# B = person with some events, last event is DATE
					#  TSTART = DATE - FIRST =/= 0
			x$TSTART[x$EVENT == "EVENT_DATE_0"] <-
				x$EVENT_DATE_0[x$EVENT == "EVENT_DATE_0"] - x$FIRST[x$EVENT == "EVENT_DATE_0"]

			# Determine start time for recurrent events
			for (i in 2:length(n)) {
				# Everything but first event has time-dependent TSTART
				if (n[i] == 1) {
					# Start of first event is always 0
					x$TSTART[x$EVENT == paste0("EVENT_DATE_", n[i])] <- 0
				} else {
					# START of new event is STOP of last event
					x$TSTART[x$EVENT == paste0("EVENT_DATE_", n[i])] <-
						as.numeric(unlist(x[x$EVENT == paste0("EVENT_DATE_", n[i]), paste0("EVENT_DATE_", n[i-1])] - x[x$EVENT == paste0("EVENT_DATE_", n[i]), "FIRST"]))
				}
			}

			### STATUS

			# For those had death as final event in their life
			x$STATUS[x$EVENT == "EVENT_DATE_0" & x$DEATH == 1] <- 1

			# For those that had just an event
			x$STATUS[x$EVENT != "EVENT_DATE_0"] <- 1


			# This is essentially the PWP-TT model, but collapsed
			# All tstarts are essentially shifted to 0
			# All tstops are reduced to interval
			x$TSTOP <- x$TSTOP - x$TSTART
			x$TSTART <- x$TSTART - x$TSTART

		},
		stop("Need the correct repeat event model: marginal, pwptt, pwpgt")
	)


	y <- x[c("ID", "TSTART", "TSTOP", "STATUS", "EVENT", "DATE")]
	# Return survival dataset
	return(y)
}


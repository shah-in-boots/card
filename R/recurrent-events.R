#' Recurrent Survival Data Format
#'
#' @description Reformats recurrent event data (wide) into different models for
#'   survival analysis, but can also be used for simple survival analysis tables
#'   as well. The general format is how data tends to be collected. There is
#'   left and right censoring date, a labeled event column that contains the
#'   date of the event, and a censoring column for a final censoring event. The
#'   accepted parameter options are listed, with the type of table that will be
#'   generated:
#'
#'   - `traditional`: Traditional survival table that has single censoring event
#'   (`trad`)
#'
#'   - `counting`: Formally called the Andersen and Gill model (`ag`). Counting
#'   process model assumes each event is independent and that a subject
#'   contributes to the risk set during the time under observation. Multiple
#'   events are treated as a new (but delayed) entry that is followed until the
#'   next event. This means subjects under observation are at risk for a second
#'   event, even without having had a prior event. There are thus no *strata* in
#'   the model.
#'
#'   - `marginal`: Marginal model assumes each event is a separate process. Each
#'   subject is at risk for all events. The time for an event starts at the
#'   beginning of follow-up for each subject. Thus, each risk period is
#'   considered a different *strata* (regardless of if subject had an event or
#'   not).
#'
#'   - `conditional A`: Formally called the Prentice, Williams, and Peterson
#'   total time model (`pwptt`). Conditional A models order events by
#'   stratification, based on the number of events prior. All subjects are at
#'   risk for the first *strata*, but only those with a previous event are at
#'   risk for a successive event. The total time to event is used.
#'
#'   - `conditional B`: Formally called the Prentice, Williams, and Peterson gap
#'   time model (`pwpgt`). Conditional B models also order events by strata
#'   (like conditional A), however the time to outcome is defined as the gap
#'   between the time of previous event.
#'
#' @details This function takes every event date, and creates several types of
#'   recurrent event tables. It orders the data chronologically for repeat
#'   events. Currently does normal (first event) and recurrent models (counting,
#'   marginal, and conditional A and B models). Further details can be found at
#'   [IDRE](https://stats.idre.ucla.edu/sas/faq/how-can-i-model-repeated-events-survival-analysis-in-proc-phreg/).
#'
#'   - For recurrent events, the final censoring event can include death, or can
#'   be ignored if its not considered a failure event.
#'
#'   - For traditional survival analysis, `censor` is required and `event_dates`
#'   should be left as NULL. The function will do the rest.
#'
#'   **Performance**: Importantly, for large datasets of recurrent data (>500
#'   rows), this function will show significant slow-down since it uses an
#'   intuitive approach on defining the datasets. Future iterations will create
#'   a vectorized approach that should provide performance speed-ups.
#'
#' @return A data frame organized into a survival table format. Output options
#'   are in **Details**. Generally, the following columns are generated:
#'
#'   - **id**: An ID column is created
#'
#'   - **start**: A formatted start time, usually 0
#'
#'   - **stop**: A formatted stop time, in days, from prior event
#'
#'   - **status**: If event occurred or not
#'
#'   - **strata**: Event strata that is being applied
#'
#' @param data A dataframe containing the subsequent parameters
#' @param model_type Model type that is indicated:
#'
#'   - `trad` makes traditional survival table
#'
#'   - `ag` makes table with risk periods starting at time of prior event
#'   without conditional strata
#'
#'   - `marginal` makes table with risk periods from entry to censorship with
#'   strata per each event
#'
#'   - `pwptt` makes table with risk periods starting at time of prior event
#'   with conditional strata
#'
#'   - `pwpgt` makes table with risk periods of each time interval between
#'   events,  with conditional strata
#'
#' @param id Column in dataframe that contains unique IDs for each row
#' @param first Column with left/enrollment dates
#' @param last Column with right/censoring time point, or last contact
#' @param censor Column that names if death/final censorship is known (0 or 1).
#'   The default is that, if no censorship information is given, that are no
#'   failure events at time of last contact. `censor` is not required for
#'   recurrent event analysis, but is required for traditional survival tables.
#' @param event_dates Vector of columns that contain event dates
#'
#' @examples
#' \donttest{
#' # Data
#' data("mims")
#'
#' # Parameters
#' id <- "patid"
#' first <- "first_visit_date_bl"
#' last <- "ldka"
#' event_dates <- c("mi_date_1", "mi_date_2", "mi_date_3")
#' model_type <- "marginal"
#' censor <- "DEATH_CV_YN"
#'
#' # Run analysis
#' out <- recur(
#'   mims, model_type, id, first, last, censor, event_dates
#' )
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select all_of mutate arrange group_by ungroup
#' @importFrom purrr map
#' @importFrom tidyr nest unnest pivot_longer pivot_wider
#' @export
#' @rdname recur
recur <- function(data, model_type, id, first, last, censor = NULL, event_dates = NULL) {

  # Check to see if censoring column is available
  if (is.null(censor)) {
    data$censor <- 0
    warning("Censorship data was not provided.")
  }

  # Check on event dates
  if (model_type == "trad") {
    if (!is.null(event_dates)) {
      warning("Event dates shouldn't be specified for traditional survival table.")
    }
  } else if (model_type != "trad") {
    if (is.null(event_dates)) {
      stop("Event dates should be specified for recurrent events.", call. = FALSE)
    }
  }

  # Appropriate columns for recurrent data
  if (model_type != "trad") {
    if (
      !id %in% names(data) |
      !first %in% names(data) |
      !last %in% names(data) |
      !censor %in% names(data) |
      length(setdiff(event_dates, names(data))) != 0
    ) {
      stop("The columns required for recurrent event analysis are not contained with the dataframe.", call. = FALSE)
    }
  }

  # Possible strata
  strata <- paste0("strata_", 0:length(event_dates))

  if (model_type == "trad") {
    tbl <-
      data %>%
      select(all_of(c(id, first, last, censor, event_dates))) %>%
      dplyr::rename(
        id = all_of(id),
        first = all_of(first),
        last = all_of(last),
        censor = all_of(censor)
      ) %>%
      mutate(strata = strata) %>%
      mutate(events = sum(!is.na(dplyr::c_across(all_of(event_dates)))))
  } else {
    # Make base table
    tbl <-
      data %>%
      select(all_of(c(id, first, last, censor, event_dates))) %>%
      dplyr::rename(
        id = all_of(id),
        first = all_of(first),
        last = all_of(last),
        censor = all_of(censor)
      ) %>%
      dplyr::rowwise() %>%
      mutate(strata_0 = first) %>%
      # Arrange by date
      pivot_longer(
        cols = c(strata_0, all_of(event_dates)),
        names_to = "strata",
        values_to = "date"
      ) %>%
      arrange(date) %>%
      # Check all ordered events and rename them (removing same day events)
      group_by(id) %>%
      nest(nested = c(strata, date)) %>%
      mutate(nested = map(nested, function(x) {
        # NA values get trimmed as side effect (will be recovered in pivot wider)
        y <- x %>% group_by(date) %>% dplyr::slice(1)
        n <- length(y$strata)
        y$strata <- strata[1:n]
        return(y)
      })) %>%
      unnest(cols = c(nested)) %>%
      arrange(id, date) %>%
      ungroup()
  }

  # Initialize basic columns for survival table output
  tbl$status <- tbl$start <- tbl$stop <- 0

  # Make final table
  tbl <- recur_model_type(tbl, model_type)

  # Return
  tbl
}

#' @description Switch method for model types
#' @noRd
recur_model_type <- function(tbl, model_type) {

  # Modeling switch
  switch(
    model_type,
    # Traditional / simple model
    trad = {
      tbl$status <- tbl$censor
      tbl$stop <- tbl$last - tbl$first
      tbl$date <- tbl$last
      res <- tbl
    },
    # Marginal Model
    marginal = {
      res <-
        tbl %>%
        group_by(id) %>%
        nest() %>%
        mutate(data = map(data, function(x) {
          # Remove missing rows as they have no data
          x <- na.omit(x)

          # Total number of events, excluding censoring events
          x$events <- nrow(x[x$strata != "strata_0",])
          n <- seq(1:max(x$events))

          # Stop time for events
          for (i in n) {
            # Stop time will be event date - first
            x$stop[x$strata == paste0("strata_", i)] <-
              x$date[x$strata == paste0("strata_", i)] -
              x$first[x$strata == paste0("strata_", i - 1)]

            # Status if event occurs
            x$status[x$strata == paste0("strata_", i)] <- 1
          }

          # Set censor date
          x$date[x$strata == "strata_0"] <-
            x$last[x$strata == "strata_0"]

          # Censoring stop time
          x$stop[x$strata == "strata_0"] <-
            x$date[x$strata == "strata_0"] - x$first[x$strata == "strata_0"]

          # Status of censoring events
          x$status[x$strata == "strata_0" & x$censor == 1] <- 1

          # Return
          return(x)

        })) %>%
        unnest(cols = c(data))
    },
    # Conditional A / PWP Total Time Model
    pwptt = {
      res <-
        tbl %>%
        group_by(id) %>%
        nest() %>%
        mutate(data = map(data, function(x) {
          # Remove missing rows as they have no data
          x <- na.omit(x)

          # Total number of events, excluding censoring events
          x$events <- nrow(x[x$strata != "strata_0",])
          n <- seq(1:max(x$events))

          # Time for events
          for (i in n) {
            # Stop time will be event date - first
            x$stop[x$strata == paste0("strata_", i)] <-
              x$date[x$strata == paste0("strata_", i)] -
              x$first[x$strata == paste0("strata_", i - 1)]

            # Status if event occurs
            x$status[x$strata == paste0("strata_", i)] <- 1

            # Start time
            x$start[x$strata == paste0("strata_", i)] <-
              x$stop[x$strata == paste0("strata_", i - 1)]
          }

          # Set censor date
          x$date[x$strata == "strata_0"] <-
            x$last[x$strata == "strata_0"]

          # Censoring stop time
          x$stop[x$strata == "strata_0"] <-
            x$date[x$strata == "strata_0"] - x$first[x$strata == "strata_0"]

          # Status of censoring events
          x$status[x$strata == "strata_0" & x$censor == 1] <- 1

          # Censor start time
          x$start[x$strata == "strata_0" & x$events > 0] <-
            x$stop[x$strata == paste0("strata_", max(n))]

          # Return
          return(x)

        })) %>%
        unnest(cols = c(data))
    },
    # Conditional B / PWP Gap Time Model
    pwpgt = {
      res <-
        tbl %>%
        group_by(id) %>%
        nest() %>%
        mutate(data = map(data, function(x) {
          # Remove missing rows as they have no data
          x <- na.omit(x)

          # Total number of events, excluding censoring events
          x$events <- nrow(x[x$strata != "strata_0",])
          n <- seq(1:max(x$events))

          # Time for events
          for (i in n) {
            # Stop time will be event date - first (to be modified / collapsed)
            x$stop[x$strata == paste0("strata_", i)] <-
              x$date[x$strata == paste0("strata_", i)] -
              x$first[x$strata == paste0("strata_", i - 1)]

            # Status if event occurs
            x$status[x$strata == paste0("strata_", i)] <- 1

            # Start time will be from prior event
            x$start[x$strata == paste0("strata_", i)] <-
              x$stop[x$strata == paste0("strata_", i - 1)]
          }

          # Set censor date
          x$date[x$strata == "strata_0"] <-
            x$last[x$strata == "strata_0"]

          # Censoring stop time
          x$stop[x$strata == "strata_0"] <-
            x$date[x$strata == "strata_0"] - x$first[x$strata == "strata_0"]

          # Status of censoring events
          x$status[x$strata == "strata_0" & x$censor == 1] <- 1

          # Censor start time
          x$start[x$strata == "strata_0" & x$events > 0] <-
            x$stop[x$strata == paste0("strata_", max(n))]

          # Conditional B has the "start times" collapse down to zero
          x$stop <- x$stop - x$start
          x$start <- x$start - x$start

          # Return
          return(x)

        })) %>%
        unnest(cols = c(data))
    },
    # Counting / Anderson & Gill Model
    ag = {
      res <-
        tbl %>%
        group_by(id) %>%
        nest() %>%
        mutate(data = map(data, function(x) {
          # Remove missing rows as they have no data
          x <- na.omit(x)

          # Total number of events, excluding censoring events
          x$events <- nrow(x[x$strata != "strata_0",])
          n <- seq(1:max(x$events))

          # Time for events
          for (i in n) {
            # Stop time will be event date - first
            x$stop[x$strata == paste0("strata_", i)] <-
              x$date[x$strata == paste0("strata_", i)] -
              x$first[x$strata == paste0("strata_", i - 1)]

            # Status if event occurs
            x$status[x$strata == paste0("strata_", i)] <- 1

            # Start time
            x$start[x$strata == paste0("strata_", i)] <-
              x$stop[x$strata == paste0("strata_", i - 1)]
          }

          # Set censor date
          x$date[x$strata == "strata_0"] <-
            x$last[x$strata == "strata_0"]

          # Censoring stop time
          x$stop[x$strata == "strata_0"] <-
            x$date[x$strata == "strata_0"] - x$first[x$strata == "strata_0"]

          # Status of censoring events
          x$status[x$strata == "strata_0" & x$censor == 1] <- 1

          # Censor start time
          x$start[x$strata == "strata_0" & x$events > 0] <-
            x$stop[x$strata == paste0("strata_", max(n))]

          # For counting model, similar to Conditional A, however only 1 stratum
          x$strata <- "strata_0"

          # Return
          return(x)

        })) %>%
        unnest(cols = c(data))

    },
    # Error catch
    stop(
      paste0("The model type `", model_type, "` is not currently supported.")
    )
  )

  # Return clean results
  res %>%
    arrange(id, date) %>%
    select(c(id, status, start, stop, strata, date, events)) %>%
    ungroup() %>%
    mutate(
      stop = as.numeric(stop),
      start = as.numeric(start)
    )

}

#' @noRd
recur_old <- function(data, id, first, last, event_dates, model_type, censor = NULL) {

  # Check for missing optional parameter of death
  # Creates minimally required table
  if (is.null(death)) {
    df <- data[c(id, first, last, event_dates)]
    death <- "null_death"
    df$null_death <- 0
  } else {
    df <- data[c(id, first, last, event_dates, death)]
  }

  # Identify number of events
  n <- 0:length(event_dates)
  events <- paste0("EVENT_DATE_", c(1:(length(n) - 1)))

  # Event dates need to be organized by actual values
  x <- df[c(id, event_dates)] %>%
    tidyr::pivot_longer(-c(dplyr::all_of(id)), names_to = "EVENT", values_to = "DATE") %>%
    dplyr::group_by_(dplyr::all_of(id)) %>%
    dplyr::arrange(DATE) %>%
    dplyr::arrange_(dplyr::all_of(id)) %>%
    tidyr::nest()

  # Unique and sorted events and correct event numbering
  for (i in 1:length(x[[id]])) {
    # Make duplicate events NA values
    # Replace duplicates with NA if duplicates exist
    if (!plyr::empty(x[[i, 2]][[1]][duplicated(x[[i, 2]][[1]]$DATE), ])) {
      x[[i, 2]][[1]][duplicated(x[[i, 2]][[1]]$DATE), ]$DATE <- NA
    }

    # Arrange in correct order
    x[[i, 2]][[1]] %<>% dplyr::arrange(DATE)

    # Call each tibble for each ID number and rename the events
    x[[i, 2]][[1]]$EVENT <- events
  }

  # Make table wide again and add back other columns
  df <-
    tidyr::unnest(x, cols = data) %>%
    tidyr::pivot_wider(names_from = "EVENT", values_from = "DATE") %>%
    dplyr::inner_join(df[c(id, first, last, death)], ., by = id)

  # Rename for cleaner manipulations
  names(df) <- c("ID", "FIRST", "LAST", "DEATH", events)

  # EVENT_DATE_0 = most recent event (could be enrollment date)
  df$EVENT_DATE_0 <- df$FIRST

  # For loop for identifying most recent event per patient
  for (i in 1:length(n)) {

    # Identify most recent event by taking non-NA dates
    df$EVENT_DATE_0[!is.na(df[paste0("EVENT_DATE_", n[i])])] <-
      df[[paste0("EVENT_DATE_", n[i])]][!is.na(df[paste0("EVENT_DATE_", n[i])])]
  }

  # Each row should be an individual event (or non-event)
  x <-
    df[c("ID", paste0("EVENT_DATE_", n))] %>%
    tidyr::pivot_longer(-c("ID"), names_to = "EVENT", values_to = "DATE") %>%
    stats::na.omit() %>%
    dplyr::left_join(df, ., by = "ID")

  # THis leads to "x" which is both long and wide (events by name, and in each column)
  # This data set can be moved into the switch operations to create appropriate recurrent model

  # Now switch to options based on type of data
  switch(
    model_type,

    # Marginal order
    marginal = {
      # Initialize survival table data
      x$STATUS <- x$TSTART <- x$TSTOP <- 0

      ### TSTOP

      # Start time is always at beginning for marginal model
      # Stop time needs to be generated as for EVENTS (>0)
      # TIME = DATE OF EVENT - FIRST
      for (i in 2:length(n)) {
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
      for (i in 2:length(n)) {
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
            as.numeric(unlist(x[x$EVENT == paste0("EVENT_DATE_", n[i]), paste0("EVENT_DATE_", n[i - 1])] - x[x$EVENT == paste0("EVENT_DATE_", n[i]), "FIRST"]))
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
      for (i in 2:length(n)) {
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
            as.numeric(unlist(x[x$EVENT == paste0("EVENT_DATE_", n[i]), paste0("EVENT_DATE_", n[i - 1])] - x[x$EVENT == paste0("EVENT_DATE_", n[i]), "FIRST"]))
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

#' @title Recurrent Event Summary Table by Group
#'
#' @description `recur_summary` Creates a table with summary of
#' recurrent events
#'
#' @details This function allows for taking the output of
#'   [card::recur_survival_table] marginal format repeat event data,
#'   and creates a summary table that describes the number of events by
#'   strata/event.
#'
#' @param data Recurrent event data in marginal format. There must be
#' an ID column. Must merge in the covariate of interest into this data set.
#'
#' @param covar Name of covariate of interest to serve as grouping variable.
#'
#' @return Summary table by grouping variable, can be placed into a latex
#'   environment with kableExtra. Assumes that death events may be
#'   present when most recent non-EVENT has status 1.
#'
#' @export
recur_summary <- function(data, covar) {
  # What is the ID for merging? Using example, should be likely called "ID"

  # Shorten data frame name
  df <- data

  # Length of time between events for all individuals
  df$TIME <- df$TSTOP - df$TSTART

  # Did the last event dlead to fatality?
  # Indicate by ... EVENT_DATE_0 and STATUS = 1
  df$EVENT[df$EVENT == "EVENT_DATE_0" & df$STATUS == 1] <- "EVENT_DATE_DEATH"

  # Create factors
  df[[covar]] %<>% factor()
  df$EVENT %<>% factor()

  # Identify number of events per individual, and add back into DF
  # Column name is "freq"
  # This may not be necessary ... currently unused
  df <-
    dplyr::count(df, ID) %>%
    dplyr::left_join(df, ., by = "ID") %>%
    dplyr::as_tibble()

  # Make a summary table
  tbl <-
    df %>%
    dplyr::group_by(!!rlang::sym(covar), EVENT) %>%
    dplyr::summarise(Count = n(), Time = mean(TSTOP)) %>%
    tidyr::pivot_wider(id_cols = EVENT, names_from = !!rlang::sym(covar), values_from = c(Count, Time), names_sep = paste0("_", covar, "_")) %>%
    dplyr::arrange(EVENT)

  # Return table, can be formatted externally
  return(tbl)
}

#' @title Propensity Score Weighting
#'
#' @description
#' `recurrent_propensity` Adds propensity score to any data set that is being regressed upon.
#'
#' @details
#' Using a logistic regression, will take covariates and create propensity scores, and adds the weights. Uses the standard logistic regression to evaluate the propensity score.
#'
#' @param data Data frame that contains all covariates and outcomes. First column should be ID
#'
#' @param vars Variables used for regression. Outcome variable must be first.
#'
#' @return Returns a modified table from what was originally given with the new columns propensity scores. Essentially original df + 2 columns.
#'
#' @export
recurrent_propensity <- function(data, vars) {

  # Most important columns
  id <- names(data)[1]
  outcome <- vars[1]

  # Decide if linear or logistic model based on outcome type
  linLog <- length(unique(stats::na.omit(data[[outcome]])))

  # Create formula for regression
  f <-
    paste(vars[-1], collapse = " + ") %>%
    paste(vars[1], ., sep = " ~ ") %>%
    stats::as.formula()

  # Create model based on characteristic of outcome variable
  if (linLog == 2) {
    m <- stats::glm(f, data = data, family = binomial())
  } else {
    m <- stats::glm(f, data = data)
  }

  # PS scores
  PROP_SCORE <- stats::predict(m, type = "response")
  x <- cbind(data, PROP_SCORE)
  x$PROP_WEIGHT <- ifelse(x[[outcome]] == 1, 1 / x$PROP_SCORE, 1 / (1 - x$PROP_SCORE))

  # Return new data frame
  return(x)
}

#' @title Recurrent Event Sequential Model Building
#'
#' @description Takes a different covariate groups to generate several models
#'   for recurrent event survival analyses.
#'
#' @details Using the survival models in different types (e.g. marginal, PWP,
#'   etc), to create Cox regressions that are in a sequential order. Using the
#'   covariates given, will create the models on the fly. Need to specify model
#'   type and provide data in a certain format.
#'
#' @param data Data frame that is the survival format, potentially made by the
#'   [card::recur_survival_table]. Has to be merged with the superset of
#'   covariates that are being tested.
#'
#' @param covar.builds This is a vector that names the individual vectors for
#'   each model, likely sequential and additive. The individual vectors contain
#'   the names of the columns in the data frame that will generate regressions.
#'
#' @param model.type Type of recurrent event data, selected from c("marginal",
#'   "pwptt", "pwpgt")
#'
#' @param prop.scores This is a vector of the names of which `covar.builds`
#'   should be performed with propensity weighting. This will call a separate
#'   function [card::recurrent_propensity] that will generate both a PROP_SCORE
#'   column and PROP_WEIGHT column. Optional parameter, defaults to NULL.
#'
#' @return List of models in sequential order.
#'
#' @export
recurrent_model_building <-
  function(data, covar.builds, model.type, prop.scores = NULL) {
    # Important variables / columns
    n <- length(covar.builds)
    names(data)[1:5] <- c("ID", "TSTART", "TSTOP", "STATUS", "EVENT")
    m <- list()

    # Create all the models in sequence
    for (i in 1:n) {
      # Create formulas
      f <-
        paste(get(covar.builds[i]), collapse = " + ") %>%
        paste("Surv(TSTART, TSTOP, STATUS)", ., sep = " ~ ") %>%
        # Different recurrent event models with cluster and strata
        purrr::when(
          model.type == "marginal" ~
          paste(., "cluster(ID)", sep = " + "),
          model.type == "pwptt" ~
          paste(., "cluster(ID)", "strata(EVENT)", sep = " + "),
          model.type == "pwpgt" ~
          paste(., "cluster(ID)", "strata(EVENT)", sep = " + "),
          ~ stats::as.formula()
        ) %>%
        stats::as.formula()

      # Assess need for propensity weighting
      # Dynamically save the models
      if (covar.builds[i] %in% prop.scores) {
        # Uses the recurrent_propensity function
        x <- recurrent_propensity(data, get(covar.builds[i]))
        m[[i]] <-
          survival::coxph(
            f,
            method = "breslow",
            data = x,
            weights = x$PROP_WEIGHT
          )
      } else {
        m[[i]] <- survival::coxph(f, method = "breslow", data = data)
      }
    }

    # Return output
    return(m)
  }

#' @title Initial and Final Visit Table
#'
#' @description Makes a before/after dataset using a unique ID that follows
#'   patients between studies, to allow for comparison over time.
#'
#' @details Currently functions by taking two input IDs, one being a ID that is
#'   the same between studies (a true key ID) and an ID that is unique to that
#'   study itself. It will arrange by dates, and and slice data into an initial
#'   visit and the most recent visit. Each row should have a KEY ID and a STUDY
#'   ID. The data is in a long format, such that the STUDY IDs are unique / not
#'   duplicated.
#'
#' @param data Data frame containing all clinical covariates of interest
#'
#' @param studyid Should be one ID for every study date/visit. Can have
#'   multiples ONLY if there were several data points gathered on a single visit
#'   (e.g. heart rate measured multiple times on the same day).
#'
#' @param keyid Should be the ID that corresponds to each studyid throughout
#'   each visit
#'
#' @param date Name of column containing the date of each visit
#'
#' @return Returns list of initial and most recent data sets. These can easily
#'   be merged after with any naming nomenclature as chosen, or with any merging
#'   keys as chosen (in case there are several merging variables, like keyid +
#'   hour of day for circadian data).
#'
#' @importFrom magrittr %>%
#'
#' @export
recur_followup_table <- function(data, studyid, keyid, date) {

  # Make sure in tibble/tidy format
  data <- dplyr::as_tibble(data)

  # Find which rows / IDs are repeated based on the keyid
  # Should have multiple rows per keyid, but only 1 per studyid
  repeats <-
    data[c(keyid, studyid, date)] %>%
    unique() %>%
    dplyr::group_by(!!rlang::sym(keyid)) %>%
    dplyr::tally() %>%
    subset(n > 1)

  # Initial visit IDs
  beforeid <-
    data[c(keyid, studyid, date)] %>%
    unique() %>%
    dplyr::filter(!!rlang::sym(keyid) %in% repeats[[keyid]]) %>%
    dplyr::group_by(!!rlang::sym(keyid)) %>%
    dplyr::arrange(!!rlang::sym(date)) %>%
    dplyr::slice(1) %>%
    .[studyid]

  # Final visit IDs
  afterid <-
    data[c(keyid, studyid, date)] %>%
    unique() %>%
    dplyr::filter(!!rlang::sym(keyid) %in% repeats[[keyid]]) %>%
    dplyr::group_by(!!rlang::sym(keyid)) %>%
    dplyr::arrange(!!rlang::sym(date)) %>%
    dplyr::slice(n()) %>%
    .[studyid]

  # Before data set (initial visit)
  before <-
    data %>%
    dplyr::filter(!!rlang::sym(studyid) %in% beforeid[[studyid]])

  # After data set (most recent visit)
  after <-
    data %>%
    dplyr::filter(!!rlang::sym(studyid) %in% afterid[[studyid]])

  # Return
  x <- list()
  x[["initial"]] <- before
  x[["final"]] <- after
  return(x)
}


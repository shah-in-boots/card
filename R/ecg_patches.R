# Read in VivaLNK Patch Data {{{ ====

#' @title Read in VivaLNK Patch Data
#'
#' @description Reads in VivaLNK log files and helps to organize the summary
#' data. Does not obtain the ECG signal, just the summary data from the log file
#' for diagnostic purposes. This is a slow function, and is best for
#' troubleshooting.
#'
#' @param name Name of the patient that the log file was created on. The
#'   variable does NOT have an extension.
#'
#' @param loc The raw, unprocessed data folder containing the log file (which is
#'   in a ".txt" format)
#'
#' @return Returns a simple data frame with the start and endtime of the ECG
#'   data. Has several additional markers available internally, but currently
#'   not called.
#'
#' @export
proc_patch_vivalnk <- function(name, loc) {
  # Read in file name
  tmp <- readr::read_delim(file.path(loc, paste0(name, ".txt")), delim = "\n", col_names = FALSE)

  # Structure is a very tall, tall tibble. Extract only relevant rows
  time <- tmp$X1[grep("Sample", tmp$X1)]
  rr <- tmp$X1[grep("RRI", tmp$X1)]

  # Combine into dataframe
  # Split into columns to help extract time stamps
  df <-
    # combine into a single data frame
    dplyr::inner_join(tibble::enframe(time, value = "time"), tibble::enframe(rr, value = "RR"), by = "name") %>%
    # Split strings into components
    tidyr::separate(time,
      sep = ",",
      into = c("index", "datetime", "lead", "flash", "hr", "resp", "activity", "mag"),
      remove = TRUE
    ) %>%
    # Extract time
    tidyr::separate(index, into = c("sample", "index"), sep = "=", remove = TRUE, convert = TRUE) %>%
    # Convert date time column later
    tidyr::separate(datetime, into = c("trash", "datetime"), sep = "=", remove = TRUE, convert = TRUE) %>%
    # Pull HR into BPM
    tidyr::separate(hr, into = c("hr", "bpm"), sep = "=", remove = TRUE, convert = TRUE) %>%
    # Respiratory rate
    tidyr::separate(resp, into = c("rr", "resp"), sep = "=", remove = TRUE, convert = TRUE)

  # Convert date time format, but need to preserve miliseconds
  options(digits.secs = 3)
  df$datetime %<>% lubridate::ymd_hms()

  # Extract the RR intervals as well
  df$RR <- stringr::str_extract(df$RR, "\\d+") %>% as.integer(.)

  # Select relevant columns
  df <- df[c("index", "datetime", "bpm", "resp", "RR")]

  # Final form of vivalnk raw text information
  df <- df[order(df$index), ]

  # Return start time, end time, and length in list
  startTime <- utils::head(df$datetime, 1)
  endTime <- utils::tail(df$datetime, 1)
  lengthECG <-
    lubridate::interval(startTime, endTime) %>%
    lubridate::time_length(., "hours")
  x <- list(startTime, endTime, lengthECG)
  return(x)
}

# }}}

# }}}

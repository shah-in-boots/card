# Sunrise and Sunset {{{ ====

#' @title Sunrise and Sunset Times
#'
#' @description Gets sunrise and sunset times based on date and location using
#'   the algorithm by the
#'   \href{http://www.edwilliams.org/sunrise_sunset_algorithm.htm}{United States
#'   Naval Observatory}. Uses the sensible default of official zenith for
#'   calculations. Requires geographic position in latitude and longitude to
#'   calculate sunrise or sunset. Uses the function [lutz::tz_lookup_coords].
#'
#' @param date Vector of dates to calculate sun times for
#'
#' @param lat Latitude vector in degrees (e.g. Atlanta is 33.749), with negative
#'   values representing south. Each date must have a corresponding latitude.
#'
#' @param lon Longitude vector in degrees (e.g. Atlanta is -84.388), with
#'   negative values representing west
#'
#' @param zenith Zenith is the sun's zenith. There are several types, with
#'   different values. "official" = 90.8333 degrees, "civil" = 96 degrees,
#'   "nautical" = 102 degrees, "astronomical" = 108 degrees. They refer to the
#'   angle at which light allows for visibility, which can be affected by
#'   atmosphere and refraction.
#'
#' @param sunset Logical value for if sunset is wanted instead of sunrise.
#'   Default is FALSE (thus returning sunrises).
#'
#' @return Returns vector of sunrise/sunset times based on the date and location
#'   given. The time zone offset is included for the time zone represented by
#'   the latitude/longitude. The vector always returns UTC, but it has actually
#'   been corrected to the appropriate time-zone. Can use `as.character()` to
#'   strip the time zone away.
#'
#' @examples
#' data("twins")
#' twins$lat <- 33.749
#' twins$lon <- -84.388
#'
#' # Using latitude/longitude from Atlanta, GA, USA
#' twins$sunrise <- circ_sun(twins$date, twins$lat, twins$lon)
#'
#' @export
circ_sun <- function(date,
                     lat,
                     lon,
                     zenith = "official",
                     sunset = FALSE) {

  # Check to see if arguments are correct
  if (nargs() < 3) {
    stop("Missing arguments", call. = FALSE)
  }

  # Check to see if date column is correct
  dateGiven <- as.Date(date)
  if (!lubridate::is.Date(dateGiven)) {
    stop("Requires Date or POSIX as date vector", call. = FALSE)
  }

  # Set date as posix, and extract needed variables
  dateGiven <- as.POSIXlt(dateGiven, tz = "UTC")
  year <- lubridate::year(dateGiven)
  month <- lubridate::month(dateGiven)
  day <- lubridate::day(dateGiven)

  # Set zenith
  zenith <- switch(
    zenith,
    "official" = 90.8333,
    "civil" = 96,
    "nautical" = 102,
    astronomical = 108
  )

  # Calculate Julian day
  n1 <- floor(275 * month / 9)
  n2 <- floor((month + 9) / 12)
  n3 <- 1 + floor((year - 4 * floor(year / 4) + 2) / 3)
  n <- n1 - (n2 * n3) + day - 31

  # For sunrise or sunset
  # Latitude and longitude to hour value and approximate time
  longhour <- lon / 15

  if (sunset == FALSE) {
    t <- n + ((6 - longhour) / 24)
  } else {
    t <- n + ((18 - longhour) / 24)
  }

  # Calculate sun's mean anomaly
  m <- (0.9856 * t) - 3.289

  # Calculate sun's true longitude
  # l needs to be within [0,360)
  l <-
    m +
  	(1.916 * sin((pi / 180) * m)) +
  	(0.020 * sin(2 * (pi / 180) * m)) +
  	282.634
  l[l <= 0] <- l[l <= 0] + 360
  l[l > 360] <- l[l > 360] - 360

  # Calculate sun's right ascension (angles must be in degrees)
  # ra need sto be within [0, 360)
  ra <- (180 / pi) * atan(0.91764 * tan((pi / 180) * l))
  ra[ra <= 0] <- ra[ra <= 0] + 360
  ra[ra > 360] <- ra[ra > 360] - 360


  # Right ascension needs to be in same quadrant as l
  lquad <- floor(l / 90) * 90
  raquad <- floor(ra / 90) * 90
  ra <- ra + (lquad - raquad)

  # Right ascension needs to be in hours
  ra <- ra / 15

  # Calculate sun's declination
  sinDec <- 0.39782 * sin((pi / 180) * l)
  cosDec <- cos(asin(sinDec))

  # Calculate sun's local hour angle
  cosSun <-
    (cos((pi / 180) * zenith) - (sinDec * sin((pi / 180) * lat))) /
      (cosDec * cos((pi / 180) * lat))

  if (max(cosSun) > 1) {
    warning("The sun never rises on this location")
  }
  if (min(cosSun) < -1) {
    warning("The sun never sets on this location")
  }

  # calculate and convert into hours
  if (sunset == FALSE) {
    sun <- (360 - ((180 / pi) * acos(cosSun))) / 15
  } else {
    sun <- ((180 / pi) * acos(cosSun)) / 15
  }

  # calculate local time of sunrise/sunset
  tm <- sun + ra - (0.06571 * t) - 6.622

  # Adjust back to UTC, and adjust into range of [0, 24)
  ut <- tm - longhour
  ut[ut <= 0] <- ut[ut <= 0] + 24
  ut[ut > 24] <- ut[ut > 24] - 24

  # Place sunrise/sunset back onto date
  hr <- ut
  min <- 60 * (ut - floor(ut))
  sec <- 60 * (min - floor(min))
  sun <-
    dateGiven +
    lubridate::hours(floor(hr)) +
    lubridate::minutes(floor(min)) +
    lubridate::seconds(floor(sec))

  # Get origin time zone
  otz <- lutz::tz_lookup_coords(lat = lat, lon = lon, method = "accurate")

  # Difference in time zones (however it is in opposite in direction)
  offset <- sun - lubridate::force_tzs(time = sun, tzones = otz)

  # New sunrise time
  sun <- sun + offset
  return(sun)
}

# }}}

# }}}

# Center Time Around a Zeitgeiber {{{ ====

#' @title Center Time Around a Zeitgeiber
#'
#' @description Based on a centering time point, shifts a vector to a "before"
#'   and "after" system to help align multiple individuals to a universal time,
#'   like the sunrise or any other appropriate
#'   [zeitgeiber](https://en.wikipedia.org/wiki/Zeitgeber). Originally intended
#'   to expand upon the [card::circ_sun] function.
#'
#' @param times Vector of time series. The earliest time point is presumed to be
#'   the time series onset. Built with the assumption that the duration would be
#'   approximately 24 hours (or less) to remove issues with circadian rhythms
#'   and repeat zeitgeibers (e.g. sunrise). Most importantly, the time series
#'   should be roughly equally spaced, such as 1 hour apart.
#'
#' @param zeitgeiber A single timestamp that should exist within the proposed
#'   `times`. It can be a POSIX* variable or it can just be a character of a
#'   time stamp in an HMS format. Its used to create a centering point.
#'
#' @return Vector of centered times around zeitgeiber. Function guesses units of
#'   time based on time series that is input (e.g. duration / number of events).
#'   It returns a vector of relative time in guessed units as `double`, which
#'   allows centering around the zeitgeiber (Z=0).
#'
#' @examples
#' data("twins")
#' df <- subset(twins, patid == 7) # Single patient
#' times <- df$dyxtime
#' zeitgeiber <- as.POSIXct("2002-03-22 06:40:18", tz = "UTC")
#' df$zeit <- circ_center(times, zeitgeiber)
#'
#' @importFrom lubridate %within% interval int_length as.duration
#'
#' @export
circ_center <- function(times, zeitgeiber) {

  # Theoretical situations:
  # - zeitgeiber happens before the time series occurs
  # - zeitgeiber happens after the time series occurs
  # - times is > 24 hours such that zeitgeiber could exist twice

  # Establish variables and ensure zeitgieber can be centered around time series
  times <- sort(times)
  n <- length(times)
  t <- interval(dplyr::first(times), dplyr::last(times))
  z <- as.POSIXct(zeitgeiber)

  # Identify time blocks to return as in seconds, minutes, or hours
  u <- dplyr::case_when(
    as.duration(t)/n >= lubridate::hours(8) ~ "D",
    as.duration(t)/n >= lubridate::minutes(20) ~ "H",
    as.duration(t)/n >= lubridate::seconds(20) ~ "M",
    as.duration(t)/n < lubridate::seconds(20) ~ "S"
  )

  ### Three conditions: Z is in the middle, Z is before, or Z is after
  startZ <- interval(dplyr::first(times), z)
  Zend <- interval(z, dplyr::last(times))
  zpos <- NULL
  zpos <- dplyr::case_when(
    z %within% t ~ "middle",
    as.duration(startZ) < 0 ~ "before",
    as.duration(Zend) < 0 ~ "after"
  )

  # Create vector to return
  switch(
    zpos,
    # Z occurs in middle of time series
    middle = {
      zint <- interval(z, times)
      zsec <- int_length(zint) # All time intervals in seconds
      zvec <- dplyr::case_when(
        u == "D" ~ zsec/(1*60*60*24),
        u == "H" ~ zsec/(1*60*60),
        u == "M" ~ zsec/(1*60),
        u == "S" ~ zsec/(1)
      )

      return(zvec)
    },
    # Z happens before time series starts
    before = {
      zint <- interval(z, times)
      zsec <- int_length(zint)
      zvec <- dplyr::case_when(
        u == "D" ~ zsec/(1*60*60*24),
        u == "H" ~ zsec/(1*60*60),
        u == "M" ~ zsec/(1*60),
        u == "S" ~ zsec/(1)
      )

      return(zvec)
    },
    # Z happens after time series ends
    after = {
      zint <- interval(z, times)
      zsec <- int_length(zint)
      zvec <- dplyr::case_when(
        u == "D" ~ zsec/(1*60*60*24),
        u == "H" ~ zsec/(1*60*60),
        u == "M" ~ zsec/(1*60),
        u == "S" ~ zsec/(1)
      )

      return(zvec)
    },
    # Catch error, unlikely to be possible
    stop("Cannot place Zeitgeiber close to time series.")
  )

}
# }}}

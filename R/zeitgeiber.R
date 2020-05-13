# Sunrise and Sunset {{{ ====

#' @title Sunrise and Sunset Times
#'
#' @description Gets sunrise and sunset times based on date and location using
#'   the algorithm by the
#'   \href{http://www.edwilliams.org/sunrise_sunset_algorithm.htm}{United States
#'   Naval Observatory}. Uses the sensible default of official zenith for
#'   calculations. Requires geographic position in latitude and longitude to
#'   calculate sunrise or sunset.
#'
#' @param date Vector of dates to calculate sun times for
#'
#' @param lat Latitude in degrees (e.g. Atlanta is 33.749), with negative values
#'   representing south
#'
#' @param lon Longitude in degrees (e.g. Atlanta is -84.388), with negative
#'   values representing west
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
#'   given. The times are based on UTC and will need to be adjusted for timezone
#'   offset.
#'
#' @export
circ_sun <- function(date,
                     lat,
                     lon,
                     zenith = "official",
                     sunset = FALSE) {
  # Check to see if arguments are correct
  if (nargs < 3) {
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

  # Return it
  return(sun)
}

# }}}

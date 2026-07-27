#' Extract Echocardiogram Measurements
#'
#' @description A set of functions to extract common echocardiogram measurements
#' from free text reports. These functions use regular expressions to identify and
#' extract both qualitative descriptions and quantitative measurements.
#'
#' The following measurements can be extracted:
#' - Left atrial (LA) size (qualitative description)
#' - Left atrial diameter (quantitative measurement in cm)
#' - Left ventricular ejection fraction (LVEF, percentage)
#' - Left ventricular internal diameter in diastole (LVIDd, in cm)
#' - A compact set of clinically important findings from a full report
#'
#' @param text Character vector of echo report text
#'
#' @param range Which end of a graded range such as `"mildly to moderately
#'   dilated"` to report. Defaults to `"upper"`, since a report describing such a
#'   range is asserting the higher grade is present, and the conservative reading
#'   is the one that does not under-call disease.
#'
#' @param min_val,max_val Plausible range for the measurement, outside of which a
#'   match is discarded. The defaults are `1` to `10` cm for
#'   `extract_la_diameter()` and `extract_lvidd()`, and `5` to `90` percent for
#'   `extract_lvef()`.
#'
#' @details These functions use regular expressions to parse unstructured text from
#' echo reports. They handle common variations in terminology and units. Measurements
#' outside plausible ranges are returned as NA.
#'
#' A measurement is only read from the same clause as the term that names it, so
#' a value belonging to a neighboring structure is not attributed to the wrong
#' one. Where several candidate values appear, the first one inside `min_val` to
#' `max_val` is taken, which skips stray digits such as the "2d" in "LVEF by 2D
#' Simpson is 55%". Linear dimensions written in millimeters are converted to
#' centimeters; a value given without units is read as centimeters, so a
#' millimeter value with the units omitted falls outside the plausible range and
#' is returned as `NA` rather than being guessed at.
#'
#' All functions are vectorized over `text` and return one element (or row) per
#' report. Qualitative grades are normalized to `"none"`, `"trace"`, `"trivial"`,
#' `"mild"`, `"moderate"`, and `"severe"` so they can be used directly as an
#' ordered factor.
#'
#' @return
#' - `extract_la_size()`: Character vector of LA size (`"normal"`, `"mild"`, etc.)
#' - `extract_la_diameter()`: Numeric LA diameter in cm
#' - `extract_lvef()`: Numeric LVEF percentage
#' - `extract_lvidd()`: Numeric LVIDd in cm
#' - `extract_echo_findings()`: Tibble of key structure/function findings, one row per report
#'
#' @examples
#' report <- "The left atrium is mildly to moderately dilated. LVEF is 55%."
#' extract_la_size(report) # Returns "moderate"
#' extract_la_size(report, range = "lower") # Returns "mild"
#' extract_lvef(report) # Returns 55
#'
#' @name echocardiography
NULL

# Internal helper to normalize report text
# @keywords internal
clean_echo_text <- function(text) {
  text |>
    tolower() |>
    stringr::str_replace_all("\n", " ") |>
    stringr::str_replace_all("\\s+", " ") |>
    stringr::str_trim()
}

# Internal helper for LA size, returning both the grade and whether the atrium
# was reported as unable to be visualized
# @keywords internal
match_la_size <- function(text, range = c("upper", "lower")) {
  range <- match.arg(range)
  text <- clean_echo_text(text)

  # Grades may be given as a range, e.g. "mildly to moderately dilated"
  grades <- paste0(
    "normal|not well seen|likely normal|mild(?:ly)?|moderate(?:ly)?|",
    "very severely|severe(?:ly)?|elongated"
  )
  pattern <- paste0(
    "(?:left atrium|left atrial size)\\s*:?\\s*(?:is|size)?\\s*",
    "(", grades, ")(?:\\s*to\\s*(", grades, "))?"
  )

  match <- stringr::str_match_all(
    text,
    stringr::regex(pattern, ignore_case = TRUE)
  )

  # Statements about measurability or shape are not sizes
  indeterminate <- c("not well seen", "elongated")

  size <- purrr::map_chr(match, function(x) {
    if (nrow(x) == 0) {
      return(NA_character_)
    }
    found <- if (range == "upper") dplyr::coalesce(x[, 3], x[, 2]) else x[, 2]
    found <- found[!found %in% indeterminate]
    # Prefer a definite grade elsewhere in the report over an indeterminate one
    if (length(found) > 0) found[1] else NA_character_
  })

  size <- dplyr::case_when(
    is.na(size) ~ NA_character_,
    size == "likely normal" ~ "normal",
    size %in% c("mild", "mildly") ~ "mild",
    size %in% c("moderate", "moderately") ~ "moderate",
    size %in% c("severe", "severely", "very severely") ~ "severe",
    TRUE ~ size
  )

  # NA when the atrium is never described at all, rather than FALSE
  not_visualized <- purrr::map_lgl(match, function(x) {
    if (nrow(x) == 0) NA else any(x[, 2] == "not well seen")
  })

  list(size = size, not_visualized = not_visualized)
}

# Internal helper for LVEF, returning the value and any inequality qualifier
# @keywords internal
match_lvef <- function(text, min_val = 5, max_val = 90) {
  text <- clean_echo_text(text)

  # The filler allows for phrasing such as "by visual estimate is", while
  # excluding the qualifier and clause breaks so those are not swallowed
  term <- "\\b(?:lvef|ef|ejection fraction)\\b\\s*:?[^\\.;:%<>]{0,30}"
  qualifier <- "([<>]=?)?\\s*(?<![0-9])"
  patterns <- c(
    # EF range, use lower bound to avoid overcalling high end
    paste0(term, qualifier, "(\\d{1,2})\\s*(?:-|to)\\s*\\d{1,2}\\s*(?:%|percent)?"),
    # Single value, maybe decimal, optional % sign
    paste0(term, qualifier, "(\\d{1,2}\\.?\\d?)\\s*(?:%|percent)?")
  )

  value <- rep(NA_real_, length(text))
  qual <- rep(NA_character_, length(text))

  for (pat in patterns) {
    todo <- which(is.na(value))
    if (length(todo) == 0) break

    match <- stringr::str_match_all(
      text[todo],
      stringr::regex(pat, ignore_case = TRUE)
    )

    # First plausible value wins, skipping stray digits such as "2d simpson"
    hit <- purrr::map_int(match, function(x) {
      val <- suppressWarnings(as.numeric(x[, 3]))
      which(val >= min_val & val <= max_val)[1]
    })

    value[todo] <- purrr::map2_dbl(match, hit, function(x, i) {
      if (is.na(i)) NA_real_ else as.numeric(x[i, 3])
    })
    qual[todo] <- purrr::map2_chr(match, hit, function(x, i) {
      if (is.na(i)) NA_character_ else x[i, 2]
    })
  }

  list(value = value, qualifier = qual)
}

# Internal helper to search report chunks for a single LA measurement
# @keywords internal
match_la_chunks <- function(text, min_val, max_val) {
  chunks <- unlist(strsplit(text, "[\\.!:\\n]+"))

  # Define LA keywords
  la_keywords <- c(
    "la diameter",
    "la dimension",
    "la size",
    "la a/p",
    "left atrial diameter",
    "left atrial dimension",
    "left atrial size",
    "left atrium"
  )

  # Pattern for numeric values in cm
  numeric_pattern <- "(\\d+(?:\\.\\d+)?)\\s*cm"

  # Check each chunk
  for (chunk in chunks) {
    chunk_clean <- tolower(trimws(chunk))
    if (chunk_clean == "") {
      next
    }

    # Check if chunk contains LA reference
    has_la_keyword <- any(sapply(
      la_keywords,
      function(kw) grepl(kw, chunk_clean, fixed = TRUE)
    ))

    if (!has_la_keyword) {
      next
    }

    # Look for measurement
    m <- stringr::str_match(chunk_clean, numeric_pattern)
    if (!is.na(m[1, 2])) {
      val <- as.numeric(m[1, 2])
      if (!is.na(val) && val >= min_val && val <= max_val) {
        return(val) # Explicit return for early exit
      }
    }
  }

  # No valid matches found
  NA_real_
}

#' @rdname echocardiography
#' @export
extract_la_size <- function(text, range = c("upper", "lower")) {
  match_la_size(text, range = range)$size
}

#' @rdname echocardiography
#' @export
extract_lvef <- function(text, min_val = 5, max_val = 90) {
  match_lvef(text, min_val = min_val, max_val = max_val)$value
}

#' @rdname echocardiography
#' @export
extract_lvidd <- function(text, min_val = 1, max_val = 10) {
  text <- clean_echo_text(text)

  term <- paste0(
    "\\b(?:lv diameter in diastole|lvidd|lv edd|lv end diastolic dimension|",
    "lvid\\(d\\)|lv diastolic dimension|lv internal dimension diastole)\\s*:?"
  )
  # The filler allows for phrasing such as "lv edd by 2d is", while stopping at
  # a clause break so a measurement belonging to another structure is not
  # attributed to the ventricle. The left atrium is excluded by name as well,
  # since its plausible range is the same as the ventricle's and a stray match
  # would silently make the two measurements the same number
  filler <- "(?:(?!la a/p|la dimension|la diameter|la size|left atri)[^\\.;:]){0,60}"
  # The lookbehind keeps the filler from consuming the leading digits of the
  # value, which would turn "lvidd 52 mm" into 2 mm
  pattern <- paste0(term, filler, "(?<![0-9])(\\d+(?:\\.\\d+)?)\\s*(mm|cm)?")

  match <- stringr::str_match_all(
    text,
    stringr::regex(pattern, ignore_case = TRUE)
  )

  # First plausible value wins, skipping stray digits such as "2d"
  purrr::map_dbl(match, function(x) {
    if (nrow(x) == 0) {
      return(NA_real_)
    }
    val <- suppressWarnings(as.numeric(x[, 2]))
    # Check for units and convert if needed
    is_mm <- !is.na(x[, 3]) & x[, 3] == "mm"
    val <- ifelse(is_mm, val / 10, val)
    val <- val[!is.na(val) & val >= min_val & val <= max_val]
    if (length(val) > 0) val[1] else NA_real_
  })
}

#' @rdname echocardiography
#' @export
extract_la_diameter <- function(text, min_val = 1, max_val = 10) {
  text <- clean_echo_text(text)

  # Define high-priority patterns for structured sections
  priority_patterns <- list(
    la_ap = "la\\s*a/p:\\s*(\\d+\\.?\\d*)\\s*cm",
    la_measure = "l\\.?\\s*atrium\\s*\\(s\\)\\s*\\([^\\)]+\\):\\s*(\\d+\\.?\\d*)\\s*cm",
    la_dim = "left\\s+atrial\\s+a/p\\s+dimension\\s+(?:is|of)\\s*(\\d+\\.?\\d*)\\s*cm"
  )

  out <- rep(NA_real_, length(text))

  # Try priority patterns first
  for (pat in priority_patterns) {
    todo <- which(is.na(out))
    if (length(todo) == 0) break

    val <- suppressWarnings(as.numeric(stringr::str_match(text[todo], pat)[, 2]))
    val[!is.na(val) & (val < min_val | val > max_val)] <- NA_real_
    out[todo] <- val
  }

  # If no priority matches, try more general approach
  # Split into chunks and look for LA measurements
  todo <- which(is.na(out) & !is.na(text))
  out[todo] <- vapply(
    text[todo],
    match_la_chunks,
    numeric(1),
    min_val = min_val,
    max_val = max_val,
    USE.NAMES = FALSE
  )

  out
}

#' @rdname echocardiography
#' @export
extract_echo_findings <- function(text) {
  text <- clean_echo_text(text)

  # Grade for a structure, taking the upper end of any graded range. Reports
  # often put the grade ahead of the term ("mild mitral regurgitation"), so that
  # window is checked first and kept tight so a neighboring structure's grade is
  # not picked up instead.
  extract_grade <- function(term, grades) {
    before <- paste0(
      "\\b(", grades, ")\\b(?:\\s*to\\s*(", grades, ")\\b)?",
      "[^\\.;:]{0,15}?(?:", term, ")"
    )
    after <- paste0(
      "(?:", term, ")\\s*:?[^\\.;:]{0,40}?",
      "\\b(", grades, ")\\b(?:\\s*to\\s*(", grades, ")\\b)?"
    )

    m <- stringr::str_match(text, stringr::regex(before, ignore_case = TRUE))
    miss <- is.na(m[, 1])
    if (any(miss)) {
      m[miss, ] <- stringr::str_match(
        text[miss],
        stringr::regex(after, ignore_case = TRUE)
      )
    }

    dplyr::coalesce(m[, 3], m[, 2])
  }

  severity_grades <- "none|no|trace|trivial|mild(?:ly)?|moderate(?:ly)?|severe(?:ly)?"

  extract_severity <- function(term) {
    sev <- extract_grade(term, severity_grades)
    dplyr::case_when(
      is.na(sev) ~ NA_character_,
      sev == "no" ~ "none",
      sev %in% c("mildly", "mild") ~ "mild",
      sev %in% c("moderately", "moderate") ~ "moderate",
      sev %in% c("severely", "severe") ~ "severe",
      TRUE ~ sev
    )
  }

  # Named `ef` as `tibble()` would otherwise mask it with the `lvef` column
  ef <- match_lvef(text)
  lvef_category <- dplyr::case_when(
    is.na(ef$value) ~ NA_character_,
    ef$value < 30 ~ "severe systolic dysfunction",
    ef$value < 40 ~ "moderate systolic dysfunction",
    ef$value < 50 ~ "mild systolic dysfunction",
    TRUE ~ "preserved systolic function"
  )

  la <- match_la_size(text)

  # Diastolic dysfunction often reported as grade I/II/III
  lv_diastolic_dysfunction <- extract_grade(
    "diastolic (?:dysfunction|function)",
    "grade\\s*[ivx]+|normal|indeterminate|impaired relaxation|pseudonormal|restrictive"
  )

  # Pulmonary pressure / RVSP
  rvsp_match <- stringr::str_match(
    text,
    "(?:rvsp|right ventricular systolic pressure|pulmonary artery systolic pressure|pasp)\\D{0,20}(\\d{1,3}(?:\\.\\d+)?)\\s*mmhg"
  )
  rvsp_mmhg <- suppressWarnings(as.numeric(rvsp_match[, 2]))
  pulmonary_hypertension <- dplyr::case_when(
    is.na(rvsp_mmhg) ~ NA_character_,
    rvsp_mmhg < 35 ~ "none",
    rvsp_mmhg < 50 ~ "mild",
    rvsp_mmhg < 60 ~ "moderate",
    TRUE ~ "severe"
  )

  # WMA is frequently a major actionable finding
  wall_motion_abnormality <- dplyr::case_when(
    stringr::str_detect(text, "no regional wall motion abnormalit") ~ FALSE,
    stringr::str_detect(text, "regional wall motion abnormalit|rwma|hypokinesis|akinesis|dyskinesis") ~ TRUE,
    TRUE ~ NA
  )

  tibble::tibble(
    lvef = ef$value,
    lvef_qualifier = ef$qualifier,
    lvef_category = lvef_category,
    lvidd_cm = extract_lvidd(text),
    la_size = la$size,
    la_not_visualized = la$not_visualized,
    la_diameter_cm = extract_la_diameter(text),
    lv_diastolic_dysfunction = lv_diastolic_dysfunction,
    wall_motion_abnormality = wall_motion_abnormality,
    rv_dysfunction = extract_severity("right ventricular (?:systolic )?function|rv (?:systolic )?function"),
    rvsp_mmhg = rvsp_mmhg,
    pulmonary_hypertension = pulmonary_hypertension,
    mitral_regurgitation = extract_severity("mitral (?:valve )?regurgitation|\\bmr\\b"),
    aortic_stenosis = extract_severity("aortic (?:valve )?stenosis"),
    aortic_regurgitation = extract_severity("aortic (?:valve )?regurgitation"),
    tricuspid_regurgitation = extract_severity("tricuspid (?:valve )?regurgitation|\\btr\\b"),
    pericardial_effusion = extract_severity("pericardial effusion")
  )
}

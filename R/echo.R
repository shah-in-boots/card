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
#' @param text Character string containing the echo report text
#' @param min_val Minimum plausible value for measurements (default varies by function)
#' @param max_val Maximum plausible value for measurements (default varies by function)
#'
#' @details These functions use regular expressions to parse unstructured text from
#' echo reports. They handle common variations in terminology and units. Measurements
#' outside plausible ranges are returned as NA.
#'
#' @return
#' - `extract_la_size()`: Character string describing LA size ("normal", "mild", etc.)
#' - `extract_la_diameter()`: Numeric LA diameter in cm
#' - `extract_lvef()`: Numeric LVEF percentage
#' - `extract_lvidd()`: Numeric LVIDd in cm
#' - `extract_echo_findings()`: Named list of key structure/function findings
#'
#' @examples
#' report <- "The left atrium is mildly dilated. LVEF is 55%."
#' extract_la_size(report)      # Returns "mild"
#' extract_lvef(report)         # Returns 55
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

#' @rdname echocardiography
#' @export
extract_la_size <- function(text) {
  # Early return for missing data
  if (is.na(text) || is.null(text)) {
    NA_character_
  } else {
    text <- clean_echo_text(text)

    # Pattern for LA size descriptions
    pattern <- paste0(
      "(?:left atrium|left atrial size)\\s*(?:is|size)?\\s*",
      "(normal|not well seen|likely normal|mild(?:ly)?|moderate(?:ly)?|",
      "severe(?:ly)?|very severely|elongated)"
    )

    # Extract match
    match <- stringr::str_match_all(
      text,
      stringr::regex(pattern, ignore_case = TRUE)
    )[[1]]

    if (nrow(match) > 0) match[1, 2] else NA_character_
  }
}

#' @rdname echocardiography
#' @export
extract_lvef <- function(text) {
  # Early return for missing data
  if (is.na(text) || is.null(text)) {
    NA_real_
  } else {
    text <- clean_echo_text(text)

    # Define common EF patterns
    patterns <- c(
      # EF/LVEF with optional colon, digits, maybe decimal, optional % sign
      "(?:ef\\s*|lvef\\s*|ejection fraction\\s*)(:?\\s*)(\\d{1,2}\\.?\\d?)(?:\\s*%|\\s*percent)?",
      # Simpson's EF pattern
      "(?:simpson'?s?\\s*ef\\s*)(\\d{1,2}\\.?\\d?)(?:\\s*%|\\s*percent)?",
      # EF range pattern, use lower bound to avoid overcalling high end
      "(?:ef\\s*|lvef\\s*|ejection fraction\\s*)[:]?\\s*(\\d{1,2})\\s*(?:-|to)\\s*(\\d{1,2})(?:\\s*%|\\s*percent)?"
    )

    # Try each pattern until we find a match
    for (pat in patterns) {
      match <- stringr::str_match(text, pat)

      # If we found a match, check columns from right to left for numeric value
      if (!all(is.na(match))) {
        for (i in rev(seq_len(ncol(match)))) {
          possible_val <- match[1, i]

          # Try to convert to numeric if it looks like a percentage
          if (
            !is.na(possible_val) && grepl("^\\d{1,2}(\\.\\d)?$", possible_val)
          ) {
            val <- suppressWarnings(as.numeric(possible_val))
            if (!is.na(val) && val >= 5 && val <= 90) return(val)
          }
        }
      }
    }

    # No valid matches found
    NA_real_
  }
}

#' @rdname echocardiography
#' @export
extract_lvidd <- function(text) {
  # Early return for missing data
  if (is.na(text) || is.null(text)) {
    NA_real_
  } else {
    text <- clean_echo_text(text)

    # Define common LVIDd patterns
    patterns <- c(
      # Various ways to write LVIDd with units
      "(?:lv diameter in diastole|lvidd|lv edd|lv end diastolic dimension|lvid\\(d\\))\\D*(\\d+(?:\\.\\d+)?)(?:\\s*(mm|cm))?",
      "(?:lv diastolic dimension|lv internal dimension diastole)\\D*(\\d+(?:\\.\\d+)?)(?:\\s*(mm|cm))?"
    )

    # Try each pattern
    for (pat in patterns) {
      match <- stringr::str_match(text, pat)

      if (!is.na(match[1, 2])) {
        val <- suppressWarnings(as.numeric(match[1, 2]))
        # Check for units and convert if needed
        unit <- if (ncol(match) > 2) match[1, 3] else NA_character_
        if (!is.na(unit) && unit == "mm") {
          val <- val / 10 # Convert mm to cm
        }
        if (!is.na(val)) return(val) # Explicit return for early exit
      }
    }

    # No valid matches found
    NA_real_
  }
}

#' @rdname echocardiography
#' @export
extract_la_diameter <- function(text, min_val = 1, max_val = 10) {
  # Early return for missing data
  if (is.na(text) || is.null(text)) {
    NA_real_
  } else {
    text <- clean_echo_text(text)

    # Define high-priority patterns for structured sections
    priority_patterns <- list(
      la_ap = "la\\s*a/p:\\s*(\\d+\\.?\\d*)\\s*cm",
      la_measure = "l\\.?\\s*atrium\\s*\\(s\\)\\s*\\([^\\)]+\\):\\s*(\\d+\\.?\\d*)\\s*cm",
      la_dim = "left\\s+atrial\\s+a/p\\s+dimension\\s+(?:is|of)\\s*(\\d+\\.?\\d*)\\s*cm"
    )

    # Try priority patterns first
    for (pattern in priority_patterns) {
      match <- stringr::str_match(text, pattern)
      if (!is.na(match[1, 2])) {
        val <- as.numeric(match[1, 2])
        if (!is.na(val) && val >= min_val && val <= max_val) {
          return(val) # Explicit return for early exit
        }
      }
    }

    # If no priority matches, try more general approach
    # Split into chunks and look for LA measurements
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

    # No valid matches found, defaults to NA
    NA_real_
  }
}

#' @rdname echocardiography
#' @export
extract_echo_findings <- function(text) {
  if (is.na(text) || is.null(text)) {
    return(list(
      lvef = NA_real_,
      lvef_category = NA_character_,
      lvidd_cm = NA_real_,
      la_size = NA_character_,
      la_diameter_cm = NA_real_,
      lv_diastolic_dysfunction = NA_character_,
      wall_motion_abnormality = NA,
      rv_dysfunction = NA_character_,
      rvsp_mmhg = NA_real_,
      pulmonary_hypertension = NA_character_,
      mitral_regurgitation = NA_character_,
      aortic_stenosis = NA_character_,
      aortic_regurgitation = NA_character_,
      tricuspid_regurgitation = NA_character_,
      pericardial_effusion = NA_character_
    ))
  }

  text <- clean_echo_text(text)

  extract_severity <- function(term) {
    pattern <- paste0(
      "(?:", term, ")[^\\.;:]{0,80}?",
      "(none|trace|trivial|mild|mildly|moderate|moderately|severe|severely)"
    )
    m <- stringr::str_match(text, stringr::regex(pattern, ignore_case = TRUE))
    if (all(is.na(m))) return(NA_character_)
    sev <- m[1, 2]
    sev <- dplyr::case_when(
      is.na(sev) ~ NA_character_,
      sev %in% c("mildly", "mild") ~ "mild",
      sev %in% c("moderately", "moderate") ~ "moderate",
      sev %in% c("severely", "severe") ~ "severe",
      TRUE ~ sev
    )
    sev
  }

  lvef <- extract_lvef(text)
  lvef_category <- dplyr::case_when(
    is.na(lvef) ~ NA_character_,
    lvef < 30 ~ "severe systolic dysfunction",
    lvef < 40 ~ "moderate systolic dysfunction",
    lvef < 50 ~ "mild systolic dysfunction",
    TRUE ~ "preserved systolic function"
  )

  # Diastolic dysfunction often reported as grade I/II/III
  dd_match <- stringr::str_match(
    text,
    "(?:diastolic dysfunction|diastolic function)[^\\.;:]{0,60}(grade\\s*[ivx]+|normal|indeterminate|impaired relaxation|pseudonormal|restrictive)"
  )
  lv_diastolic_dysfunction <- if (!all(is.na(dd_match))) dd_match[1, 2] else NA_character_

  # Pulmonary pressure / RVSP
  rvsp_match <- stringr::str_match(
    text,
    "(?:rvsp|right ventricular systolic pressure|pulmonary artery systolic pressure|pasp)\\D{0,20}(\\d{1,3}(?:\\.\\d+)?)\\s*mmhg"
  )
  rvsp_mmhg <- if (!all(is.na(rvsp_match))) as.numeric(rvsp_match[1, 2]) else NA_real_
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

  list(
    lvef = lvef,
    lvef_category = lvef_category,
    lvidd_cm = extract_lvidd(text),
    la_size = extract_la_size(text),
    la_diameter_cm = extract_la_diameter(text),
    lv_diastolic_dysfunction = lv_diastolic_dysfunction,
    wall_motion_abnormality = wall_motion_abnormality,
    rv_dysfunction = extract_severity("right ventricular function|rv function"),
    rvsp_mmhg = rvsp_mmhg,
    pulmonary_hypertension = pulmonary_hypertension,
    mitral_regurgitation = extract_severity("mitral regurgitation|\\bmr\\b"),
    aortic_stenosis = extract_severity("aortic stenosis|\\bas\\b"),
    aortic_regurgitation = extract_severity("aortic regurgitation|\\bar\\b"),
    tricuspid_regurgitation = extract_severity("tricuspid regurgitation|\\btr\\b"),
    pericardial_effusion = extract_severity("pericardial effusion")
  )
}

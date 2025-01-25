#  LA Size (Qualitative vs Quantitative)
#########################################

# 1) Qualitative: e.g. "normal", "mildly dilated", "severely enlarged"
extract_la_size <- function(text) {
	# Return e.g. "normal", "mild", "moderate", "severe", or NA
	if (is.na(text) || is.null(text)) return(NA_character_)

	text <- tolower(text)
	text <- gsub("\n", " ", text)
	text <- gsub("\\s+", " ", text)
	text <- trimws(text)

	# Example pattern that tries to find references to LA size + descriptor
	# This is intentionally flexible; adapt to your actual text patterns
	pattern <- paste0(
		"(?:left atrium|left atrial size)\\s*(?:is|size)?\\s*",
		"(normal|not well seen|likely normal|mild(?:ly)?|moderate(?:ly)?|severe(?:ly)?|very severely|elongated)"
	)

	match <- stringr::str_match_all(text, stringr::regex(pattern, ignore_case = TRUE))[[1]]

	if (nrow(match) > 0) {
		# Return first matched descriptor, in lowercase
		return(match[1,2])
	} else {
		return(NA_character_)
	}
}



########################################
# 1) Extract LVEF (Quantitative)
########################################
extract_lvef <- function(text) {
	# Return numeric EF, e.g. 55, or NA if nothing is found

	# Clean text
	if (is.na(text) || is.null(text)) {
		return(NA_real_)
	}

	text <- tolower(text)
	text <- gsub("\n", " ", text)
	text <- gsub("\\s+", " ", text)
	text <- trimws(text)

	# Multiple patterns for EF
	#  - “EF XX%” or “LVEF: 55%”
	#  - “Ejection fraction 60%”
	#  - “Simpson’s EF 56%” or “Simpsons EF: 56%”
	#  - etc.
	# If you see other patterns in real data, add them here.
	patterns <- c(
		# EF or LVEF or ejection fraction + a possible colon or space, then digits, maybe decimal, then optional % sign
		"(?:ef\\s*|lvef\\s*|ejection fraction\\s*)(:?\\s*)(\\d{1,2}\\.?\\d?)(?:\\s*%| percent)?",
		# Simpson's EF pattern
		"(?:simpson'?s?\\s*ef\\s*)(\\d{1,2}\\.?\\d?)(?:\\s*%| percent)?"
	)

	# Try each pattern in turn; return the first numeric match found
	for (pat in patterns) {
		match <- stringr::str_match(text, pat)
		# match is a matrix; the captured group is usually in match[1,3] if we have an extra group for the possible colon
		# but let's do a quick check to see which capture group is numeric:

		# A quick strategy is to find any capturing group with digits. We'll just attempt match[1,2] or match[1,3].
		# For clarity, let's see how many capturing groups we have:
		if (!all(is.na(match))) {
			# Typically the final capturing group is the numeric. Let's do a quick approach:
			# We'll search from the last column to the first.
			for (i in rev(seq_len(ncol(match)))) {
				possible_val <- match[1,i]
				# skip if NA
				if (!is.na(possible_val) && grepl("^\\d{1,2}(\\.\\d)?$", possible_val)) {
					val <- suppressWarnings(as.numeric(possible_val))
					if (!is.na(val)) return(val)
				}
			}
		}
	}

	# If nothing matched, return NA
	return(NA_real_)
}

########################################
# 2) Extract LV Diastolic Diameter (LVIDd)
########################################
extract_lvidd <- function(text) {
	# Return numeric value in cm (if mm found, convert to cm), or NA

	# Clean text
	if (is.na(text) || is.null(text)) return(NA_real_)
	text <- tolower(text)
	text <- gsub("\n", " ", text)
	text <- gsub("\\s+", " ", text)
	text <- trimws(text)

	# Common ways LVIDd might appear:
	#   - "LV diameter in diastole 4.5 cm"
	#   - "LVIDd = 4.2 cm" or "LVID(d) is 42 mm"
	#   - "LV end diastolic dimension: 45 mm"
	patterns <- c(
		"(?:lv diameter in diastole|lvidd|lv edd|lv end diastolic dimension|lvid\\(d\\))\\D*(\\d+(?:\\.\\d+)?)(?:\\s*(mm|cm))?",
		"(?:lv diastolic dimension|lv internal dimension diastole)\\D*(\\d+(?:\\.\\d+)?)(?:\\s*(mm|cm))?"
	)

	for (pat in patterns) {
		match <- stringr::str_match(text, pat)
		# If no match at all, stringr::str_match returns NAs, so check match[1,2].
		if (!is.na(match[1,2])) {
			val <- suppressWarnings(as.numeric(match[1,2]))
			# If there's a second capturing group for the unit:
			unit <- if (ncol(match) > 2) match[1,3] else NA_character_
			if (!is.na(unit) && unit == "mm") {
				val <- val / 10  # convert mm to cm
			}
			if (!is.na(val)) return(val)
		}
	}

	return(NA_real_)
}

########################################
# 3) Extract LA Diameter (Quantitative)
########################################

extract_la_diameter <- function(text, min_val = 1, max_val = 10) {
	# Return numeric LA diameter in cm if found in a chunk of text.
	# Discard values below min_val or above max_val.
	# If no matches, return NA.

	# 1) Quick NA check
	if (is.na(text) || is.null(text)) {
		return(NA_real_)
	}

	# 2) Split the text into chunks, preserving structure.
	#    A naive approach is to split on punctuation or newlines.
	#    For instance, splitting on periods, exclamation points, colons, newlines:
	chunks <- unlist(strsplit(text, "[\\.!:\\n]+"))

	# 3) Define the keywords that suggest LA dimension info
	la_keywords <- c(
		"la diameter", "la dimension", "la size", "la a/p",
		"left atrial diameter", "left atrial dimension", "left atrial size",
		"left atrium"
	)

	# 4) We'll look for a pattern capturing a number plus “cm”
	#    e.g. "3.74 cm" or "4 cm".
	#    Since we're assuming everything is in cm, let's not worry about mm.
	numeric_pattern <- "(\\d+(?:\\.\\d+)?)\\s*cm"

	# 5) Iterate over each chunk
	for (chunk in chunks) {

		# Clean up leading/trailing whitespace
		chunk_clean <- tolower(trimws(chunk))

		# Skip if empty
		if (chunk_clean == "") next

		# Check if chunk mentions LA in any recognized way
		has_la_keyword <- any(sapply(la_keywords, function(kw) grepl(kw, chunk_clean, fixed = TRUE)))
		if (!has_la_keyword) next  # skip this chunk if no LA reference

		# Attempt to find a number + “cm” within that chunk
		m <- stringr::str_match(chunk_clean, numeric_pattern)
		# str_match returns a matrix; m[1,2] is the first capturing group if matched
		if (!is.na(m[1,2])) {
			val <- as.numeric(m[1,2])
			# If the value is plausible, return it
			if (!is.na(val) && val >= min_val && val <= max_val) {
				return(val)
			}
		}
	}

	# 6) If we never found a plausible match, return NA
	return(NA_real_)
}


extract_la_diameter <- function(report) {
	# Initialize result
	result <- NA_real_

	# Convert report to character if it isn't already
	report <- as.character(report)

	# Split report into lines and remove extra whitespace
	lines <- stringr::str_trim(unlist(stringr::str_split(report, "\n")))

	# Pattern matching priorities:

	# 1. Look for LA A/P measurement in structured section
	la_ap_pattern <- "LA\\s*A/P:\\s*(\\d+\\.?\\d*)\\s*cm"
	la_ap_match <- stringr::str_match(paste(lines, collapse = " "), la_ap_pattern)
	if (!is.na(la_ap_match[1,2])) {
		return(as.numeric(la_ap_match[1,2]))
	}

	# 2. Look for standardized LA measurement in measurements section
	la_measure_pattern <- "L\\.?\\s*Atrium\\s*\\(S\\)\\s*\\([^\\)]+\\):\\s*(\\d+\\.?\\d*)\\s*cm"
	la_measure_match <- stringr::str_match(paste(lines, collapse = " "), la_measure_pattern)
	if (!is.na(la_measure_match[1,2])) {
		return(as.numeric(la_measure_match[1,2]))
	}

	# 3. Look for LA A/P dimension mentioned in narrative
	la_dim_pattern <- "left\\s+atrial\\s+A/P\\s+dimension\\s+(?:is|of)\\s*(\\d+\\.?\\d*)\\s*cm"
	la_dim_match <- stringr::str_match(paste(lines, collapse = " "), la_dim_pattern)
	if (!is.na(la_dim_match[1,2])) {
		return(as.numeric(la_dim_match[1,2]))
	}

	# Return NA if no measurement found
	return(result)
}


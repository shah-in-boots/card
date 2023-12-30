#' Center for Medicare and Medicaid Services (CMS) Procedure Codes
#'
#' @description
#' This is a generative function used to call CMS procedure codes. It is used to
#' create a dataset that can be generally used to map procedure codes to their
#' descriptions, allowing for understanding of interventions performed. The
#' currently supported codes are explained in the details.
#'
#' The following procedure codes are currently supported:
#'
#' * ICD9 procedure codes, most recently updated on `2014-10-01`
#'
#' * ICD10 procedure codes, most recently updated on `2023-01-11`
#'
#' * HCPCS prcoedure codes, most recently updated on `2023-11-29`
#'
#' * CPT procedure codes, most recently updated on `2023-11-29`
#'
#' @details
#' CMS will usually release updated version of these codes on an annual basis.
#' Each dataset that is supported below can be identified by the year it was
#' published (not the _go-live_ date, but the _publically-available_ date). The
#' previous versions that are included in the package are as below.
#'
#' * ICD9: 2014
#'
#' * ICD10: 2023
#'
#' * HCPCS: 2023
#'
#' * CPT: 2023
#'
#' @param format The format of the procedure codes. Currently supported formats
#'   are: `c('ICD9', ICD10', HCPCS', 'CPT')` (case-sensitive character vector).
#'
#' @param version The version of the procedure codes. Currently supported:
#'   `c(2014, 2023)` (numeric vector).
#'
#' @name procedure_codes
#' @export
procedure_codes <- function(format, version) {
  # Check if the format is supported
  if (!format %in% c("icd9", "icd10", "hcpcs", "cpt")) {
    stop("Format not supported. Please use one of the following: icd9, icd10, hcpcs, cpt")
  }

  # Check if the version is supported
  if (!version %in% c("2014", "2023")) {
    stop("Version not supported. Please use one of the following: 2014, 2023")
  }

	if (format == 'icd9') {
		dat <- icd9
	}

	if (format == 'icd10') {
		dat <- icd10
	}

	if (format == 'hcpcs') {
		dat <- hcpcs
	}

	if (format == 'cpt') {
		dat <- cpt
	}

	# Return dataset
	return(dat)
}

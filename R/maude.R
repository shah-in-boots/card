# MAUDE Data ----

#' Load FDA MAUDE Coding Resources by Annex
#'
#' @description Load Medical Device Report (MDR) adverse event coding tables
#'   published by the FDA. The `annex` argument selects which FDA annex to load.
#'   The interface is designed to support additional annexes over time as more
#'   code tables are added to the package data.
#'
#' @details The FDA publishes MDR adverse event codes as annexed code tables
#'   (e.g., Annex E for health effects such as clinical signs, symptoms, or
#'   conditions). This function returns the annex-specific table bundled with
#'   the package. Currently, only Annex E is available, but the structure allows
#'   other annexes to be added with the same calling pattern.
#'
#' @param annex A single character identifying the FDA annex to load (e.g.,
#'   `"E"`). Case-sensitive. Only `"E"` is currently supported.
#'
#' @return A `tbl_df` of codes and related metadata for the requested annex.
#'   For Annex E, this includes hierarchical terms and mappings to IMDRF and
#'   MedDRA identifiers.
#'
#' @references
#' FDA MDR Adverse Event Codes: Coding Resources for Medical Device Reports
#' https://www.fda.gov/medical-devices/mdr-adverse-event-codes/coding-resources-medical-device-reports
#'
#' @examples
#' # Load Annex E health effects codes
#' annex_e <- load_maude_codes("E")
#'
#' @export
load_maude_codes <- function(annex = "E") {
  # Validate annex input
  valid_annexes <- c("E")
  if (!(annex %in% valid_annexes)) {
    stop("Invalid annex specified. Valid options are: ",
         paste(valid_annexes, collapse = ", "))
  }

  # Load appropriate dataset
  annex <- paste("annex_", tolower(annex))
  dat <- .maude_codes[[annex]]

  # Return
  dat
}


# MAUDE Narrative Evaluation with LLMs ----

# Need to create a function that can handle the LLM interaction for the
# narrative text. Need the function to limit the amount of data that is
# coming into the LLM to limit token use.

# Arguments:
# - API key for the LLM service
# - What type of events it needs to focus on (this stems from standard
#   problems listed by MAUDE, with an individual definition for each,
#   tiered terms/hierarchy for specificity of diagnosis with some overlap)
# - Narrative text to be evaluated from MAUDE database

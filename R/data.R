# Recurrent Event Sample Data ----

#' Recurrent event sample data
#'
#' Data is from a outcomes study on cardiovascular outcomes.
#' It contains the first visit date, the last known date, and
#' times of various events that have happened. They document death
#' at right censoring as well. These events are non-ordered.
#'
#' @docType data
#' @format An tibble data frame
#' @keywords datasets
"stress"

# Clinical Variables and Dyx Hourly Data ----

#' Hourly time series data with clinical covariates
#'
#' Data is from an algorithm that generates a summary HRV measure using the Poincare phase-space plot, generated from kurtoses of the x and y axis. Clinical data is also available for visualization and comparison. There are repeat rows for each hour that Dyx was taken.
#'
#' @docType data
#' @format An tibble data frame
#' @keywords datasets
"twins"

# Clinical Variables and Dyx Hourly Data ----

#' Hourly time series data with clinical covariates
#'
#' Clinical data is also available for visualization and comparison. Other HRV measures are used here for comparison and testing out functions.
#'
#' @docType data
#' @format A `tbl_df`
#' @keywords datasets
"triplets"

# Global Electrical Heterogeneity Data ----

#' GEH parameters in a large clinical cohort
#'
#' Used in the model-building examples for repeat testing.
#'
#' @docType data
#' @format A tibble
#' @keywords datasets
"geh"

# Output from MATLAB HRV Toolbox ----

#' Output from MATLAB HRV Toolbox
#'
#' Data is a single patient data output from HRV Toolbox. It contains granular data of calculated HRV in 5-second sliding windows.
#'
#' @docType data
#' @format An tibble data frame
#' @keywords datasets
"hrv"

# Zipcodes with Associated Latitude and Longitude ----

#' Zipcodes with Associated Latitude and Longitude
#'
#' This is a dataset from the archived/orphaned `zipcode` package.
#'
#' @docType data
#' @format A data frame with character vector zipcodes and latitude/longitude
#' @keywords datasets
"zipcode"

# Complication Data ----

#' Complication Definitions for Cardiac Procedure Adverse Event Adjudication
#'
#' A flat list of complication categories for adjudicating cardiac
#' electrophysiology procedure adverse event narratives. Each entry represents
#' a single complication type with a clinical definition and classification
#' schema. The built-in definitions are written for catheter-based cardiac
#' electrophysiology procedures and are most directly informed by AF ablation
#' literature. Users can subset or replace categories to fit other cardiac
#' procedures and can also supply their own complication lists in the same
#' format.
#'
#' Derived from:
#' - 2024 EHRA/HRS/APHRS/LAHRS Expert Consensus Statement on Catheter and
#'   Surgical Ablation of AF (Tzeis et al., Europace 2024;26:euae043)
#' - Procedure-Related Complications of Catheter Ablation for AF
#'   (Tzeis et al., JACC 2023;82:1524-1536)
#' - 2023 ACC/AHA/ACCP/HRS Guideline for Diagnosis and Management of AF
#'   (Joglar et al., JACC 2024;83:109-279)
#' - MANIFEST-17K: Multinational Survey on Safety of Postapproval Clinical Use
#'   of Pulsed Field Ablation (Ekanem et al., Circulation 2024)
#' - Considerations Regarding Safety with PFA for AF
#'   (Heart Rhythm O2, 2024;5:e01169)
#'
#' @format A named list of complication categories. Each element is a list with
#'   three components:
#'
#'   - **title**: Character. The full clinical name of the complication.
#'   - **definition**: Character. A clinical definition written for use by
#'     an LLM or human adjudicator to identify the complication in adverse
#'     event narrative text.
#'   - **classification**: A named character vector. Names are short
#'     classification codes; values are definitions of each classification.
#'     Codes may reflect subtype, acuity, intervention, or outcome. Within a
#'     selected category, classifications are not necessarily mutually
#'     exclusive. Categories may also be co-assigned when more than one
#'     mechanism or outcome is plausible from the narrative. Every category
#'     includes an `"insufficient_info"` level for narratives that lack
#'     sufficient detail to classify more specifically.
#'
#' @examples
#' # Access a single complication
#' complication_definitions$pericardial$title
#' complication_definitions$pericardial$definition
#' names(complication_definitions$pericardial$classification)
#'
#' # List all complication category names
#' names(complication_definitions)
#'
#' # Get all titles
#' vapply(complication_definitions, \(x) x$title, character(1))
#'
#' # Select a subset relevant to AF ablation
#' af_ablation_complications <- complication_definitions[c(
#'   "pericardial", "stroke", "vascular", "pv_stenosis", "esophageal",
#'   "phrenic", "arrhythmia", "coronary", "hemolysis", "respiratory",
#'   "infection", "death", "device_malfunction", "no_harm", "other"
#' )]
#'
#' # Define a custom complication list in the same format
#' my_complications <- list(
#'   lead_dislodgement = list(
#'     title = "Lead Dislodgement",
#'     definition = "Displacement of a pacemaker or ICD lead from its
#'       implant site, resulting in loss of capture, sensing failure, or
#'       change in pacing threshold. The narrative may describe lead
#'       repositioning, revision surgery, or new pacing parameters.",
#'     classification = c(
#'       reprogrammed = "Lead dislodgement managed by device reprogramming
#'         without surgical intervention.",
#'       repositioned = "Lead surgically repositioned or replaced.",
#'       insufficient_info = "Lead dislodgement suspected but insufficient
#'         detail to determine management."
#'     )
#'   )
#' )
#'
#' @source
#' Tzeis S, Gerstenfeld EP, Kalman J, et al. 2024 European Heart Rhythm
#' Association/Heart Rhythm Society/Asia Pacific Heart Rhythm Society/Latin
#' American Heart Rhythm Society expert consensus statement on catheter and
#' surgical ablation of atrial fibrillation. Europace. 2024;26(4):euae043.
#' @docType data
#' @keywords datasets
"complication_definitions"

#' MAUDE Complication Index for Cardiovascular Procedure Adverse Events
#'
#' A named list of normalized MAUDE problem terms organized into the
#' complication categories defined in `complication_definitions`, plus a
#' residual `not_indexed` bucket. The list is maintained explicitly in
#' `data-raw/complications.R` as hand-written term vectors. Overlap between
#' categories is expected.
#'
#' @examples
#' maude_complication_index$pericardial
#' maude_complication_index$stroke
#' maude_complication_index$device_malfunction
#' maude_complication_index$not_indexed
#'
#' @docType data
#' @keywords datasets
"maude_complication_index"

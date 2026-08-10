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
#' a single complication type with a clinical definition and subcategory
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
#'   - **classification**: A named list or named character vector. Names are
#'     short subcategory codes; values are scalar character definitions of each
#'     subcategory. Codes may reflect subtype, acuity, intervention, or outcome.
#'     Within a selected category, subcategories are not necessarily mutually
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
#'     classification = list(
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

#' MAUDE Manufacturer Index for Cardiovascular Devices
#'
#' A regex lookup mapping the manufacturing entities that appear in MAUDE's
#' `manufacturer_name` field onto a canonical entity name. Maintained in
#' `data-raw/maude-entities/manufacturer-patterns.csv` and applied by
#' [normalize_maude_manufacturer()], which takes the first pattern that matches.
#'
#' The entity is the company that made the device, not the company that owns it
#' today. Plants and contract manufacturers resolve here -- `"MPRI"` is
#' Medtronic, `"VENUSA DE MEXICO S.A. DE C.V."` builds under contract for
#' Abbott -- because neither is an ownership relationship and neither has a date.
#' Corporate ownership is [maude_ownership], and is applied separately by
#' [resolve_maude_owner()], because it is the part that changes with time.
#'
#' Coverage is measured against the openFDA `count` endpoint each time the
#' table is rebuilt and travels with the data rather than with this page, which
#' would go stale on the next edit:
#'
#' ```r
#' attr(maude_manufacturer_index, "coverage")
#' ```
#'
#' The full report, including patterns that matched nothing and the largest
#' strings that fell through, is written to
#' `data-raw/maude-entities/coverage.md`. An entity the index does not cover
#' returns `NA` rather than a guess.
#'
#' @format A `tbl_df` with five columns:
#'
#'   - **pattern**: Character. A case-insensitive regular expression matched
#'     against the raw MAUDE manufacturer string.
#'   - **entity**: Character. The canonical name of the entity that made the
#'     device.
#'   - **priority**: Integer. Lower patterns are tested first, defaulting to
#'     `100`. Only a row that exists to pre-empt another carries a different
#'     value: `CRYOCATH` is `10` because reports arrive as
#'     `"MEDTRONIC CRYOCATH LP"` and the plain `MEDTRONIC` pattern would
#'     otherwise claim them.
#'   - **note**: Character. Why the row maps the way it does, where that is not
#'     obvious. `NA` where it is.
#'   - **source**: Character. A citation for the mapping, `NA` where none was
#'     recorded.
#'
#'   The table carries a `coverage` attribute, described above.
#'
#' @examples
#' # What the index knows about the Medtronic entities
#' maude_manufacturer_index[
#'   grepl("MEDTRONIC|CRYOCATH", maude_manufacturer_index$pattern),
#' ]
#'
#' # Measured coverage, as of the last rebuild
#' attr(maude_manufacturer_index, "coverage")
#'
#' @seealso [normalize_maude_manufacturer()], [maude_ownership]
#' @docType data
#' @keywords datasets
"maude_manufacturer_index"

#' Corporate Ownership of Medical Device Manufacturers
#'
#' A dated parent-pointer table recording which company owned which, and when.
#' Maintained in `data-raw/maude-entities/ownership.csv` and walked by
#' [resolve_maude_owner()], which follows an entity up the chain until no row
#' matches the date it was given.
#'
#' # Why it is dated, and separate
#'
#' MAUDE reports span 1992 to the present, and the company that made a device is
#' frequently not the company that owns that business now. Holding ownership as
#' its own table, rather than as a parent baked into each pattern, means an
#' acquisition is **one row** and an acquisition of a parent carries every
#' entity beneath it. `"Telectronics Pacing Systems"` reaches Abbott through
#' St. Jude Medical without anyone writing Abbott next to Telectronics.
#'
#' It also makes a divestiture expressible, which a single-parent column cannot
#' do. An entity with no row matching the date owns itself, so Physio-Control
#' resolves to Medtronic in 2005, to itself in 2014, and to Stryker in 2020.
#'
#' # What earns a row
#'
#' Three rules keep this from becoming a corporate database:
#'
#'   1. An entity earns a row only if a pattern index names it, or if it sits on
#'      a chain between one and its parent.
#'   2. A row requires a closing date that can be stated. A relationship that is
#'      real but undated -- Stockert building generators for Biosense Webster --
#'      is recorded in the pattern index instead.
#'   3. The chain stops at the operating company the field would name. ZOLL has
#'      been an Asahi Kasei subsidiary since 2012, and nobody calls it an Asahi
#'      Kasei defibrillator.
#'
#' Where a precise closing date could not be established the row uses the start
#' of the month or year and says so in `note`, and `source` is `NA`. That is a
#' stated approximation rather than a confident-looking wrong date, and
#' `coverage.md` lists every row lacking a source so the gap stays visible.
#'
#' @format A `tbl_df` with six columns:
#'
#'   - **entity**: Character. The company that was owned.
#'   - **parent**: Character. The company that owned it.
#'   - **from**: Date. When the ownership began, `NA` for unbounded, which is
#'     used where the relationship predates the period MAUDE covers.
#'   - **to**: Date. When it ended, `NA` for ongoing. A closed interval is a
#'     divestiture.
#'   - **note**: Character. The precision of the date, and the nature of the
#'     transaction where it is not a plain acquisition.
#'   - **source**: Character. A citation, `NA` where none was recorded.
#'
#'   Intervals do not overlap within an entity, which is what lets
#'   [resolve_maude_owner()] take the first matching row.
#'
#' @examples
#' # The chain from a 1990s pacemaker company to its owner today
#' maude_ownership[maude_ownership$entity == "Telectronics Pacing Systems", ]
#' resolve_maude_owner(
#'   "Telectronics Pacing Systems",
#'   as_of = as.Date(c("2000-01-01", "2020-01-01"))
#' )
#'
#' # A divestiture, which a single-parent column cannot express
#' maude_ownership[maude_ownership$entity == "Physio-Control", ]
#'
#' @seealso [resolve_maude_owner()], [maude_manufacturer_index]
#' @docType data
#' @keywords datasets
"maude_ownership"

#' MAUDE Cardiac Ablation Brand Index
#'
#' A regex lookup mapping cardiac ablation brand names as they appear in MAUDE's
#' `device_brand_name` field onto a platform, an energy modality, and the entity
#' that makes it. Maintained as two files --
#' `data-raw/maude-entities/ablation-patterns.csv`, one brand alias per row, and
#' `ablation-platforms.csv`, one platform per row -- joined at build time and
#' applied by [normalize_maude_ablation()].
#'
#' It exists because product code does not determine modality: `OAE` covers
#' cryoablation and radiofrequency alike, and a rule that resolves it from the
#' product code alone will place cryoballoon, pulsed-field, and laser devices in
#' a radiofrequency arm.
#'
#' Two things it is careful about. Substring matching on `"arctic"` is not a
#' cryoablation signal, since `"ARCTIC SUN"` is a targeted temperature
#' management console; the pattern is `"ARCTIC FRONT"`. And mapping, access, and
#' irrigation devices share the ablation product codes, so `PENTARAY`,
#' `OCTARAY`, `FARADRIVE`, `RHYTHMIA`, `ENSITE` and others are in the table with
#' a `modality` of `NA` rather than being swept into whichever arm their product
#' code implies.
#'
#' The `entity` column names the maker, never the corporate parent, so the table
#' does not have to be edited when a company is acquired. Pass it to
#' [resolve_maude_owner()] with a date for the parent.
#'
#' Coverage is measured on each rebuild and travels with the data:
#' `attr(maude_ablation_index, "coverage")`, with the full report in
#' `data-raw/maude-entities/coverage.md`.
#'
#' @format A `tbl_df` with seven columns:
#'
#'   - **pattern**: Character. A case-insensitive regular expression matched
#'     against the raw MAUDE brand string.
#'   - **platform**: Character. The device platform, such as `"Arctic Front"`.
#'   - **modality**: Character. One of `"cryoablation"`, `"radiofrequency"`,
#'     `"pulsed field"`, `"pulsed field or radiofrequency"` for the Affera
#'     Sphere-9, which delivers both from one catheter, or `"laser"`. `NA` for a
#'     device that delivers no energy.
#'   - **entity**: Character. The entity that makes the platform, using the same
#'     vocabulary as [maude_manufacturer_index].
#'   - **priority**: Integer. Lower patterns are tested first, defaulting to
#'     `100`.
#'   - **note**: Character. What the device is, where the platform name does not
#'     say. `NA` where it does.
#'   - **source**: Character. A citation, `NA` where none was recorded.
#'
#'   The table carries a `coverage` attribute, described above.
#'
#' @examples
#' # Every pulsed field platform the index knows
#' maude_ablation_index[
#'   !is.na(maude_ablation_index$modality) &
#'     maude_ablation_index$modality == "pulsed field",
#' ]
#'
#' # The devices that share the ablation product codes but do not ablate
#' maude_ablation_index[is.na(maude_ablation_index$modality), ]
#'
#' @seealso [normalize_maude_ablation()], [maude_ownership]
#' @docType data
#' @keywords datasets
"maude_ablation_index"

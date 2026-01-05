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


# OpenFDA MAUDE Query ----

#' Query the FDA MAUDE Database
#'
#' @description Query the Manufacturer and User Facility Device Experience
#'   (MAUDE) database using the openFDA API. This database contains medical
#'   device adverse event reports submitted to the FDA.
#'
#' @details The function queries the openFDA device adverse event endpoint,
#'   which contains reports from mandatory reporters (manufacturers, importers,
#'   and device user facilities) and voluntary reporters (healthcare
#'   professionals, patients, and consumers). Data covers publicly releasable
#'   records from approximately 1992 to present and is updated weekly.
#'
#'   **Rate Limits:** The openFDA API allows approximately 240 requests per
#'   minute (4 per second) without an API key, and 240 requests per minute with
#'   a key. Large queries are automatically paginated in batches of up to 1000
#'   records.
#'
#'   **Search Syntax:** The `search` parameter uses Elasticsearch query syntax.
#'   Common patterns include:
#'   - Simple term: `"pacemaker"`
#'   - Field-specific: `"device.generic_name:pacemaker"`
#'   - Multiple terms: `"device.generic_name:pacemaker+AND+event_type:malfunction"`
#'   - Date range: `"date_received:[20200101+TO+20201231]"`
#'   - Exact phrase: `"device.brand_name:\"Medtronic\""`
#'
#' @param search Character string specifying the search query. Can be a simple
#'   term (e.g., `"pacemaker"`) or a field-specific query (e.g.,
#'   `"device.generic_name:pacemaker"`). See Details for query syntax.
#'
#' @param limit Integer specifying the maximum number of records to return.
#'   Default is 100. The openFDA API has a maximum of 1000 records per request;
#'   larger requests are automatically paginated.
#'
#' @param date_start Optional character string specifying the start date for
#'   filtering by `date_received` in `"YYYYMMDD"` format (e.g., `"20200101"`).
#'
#' @param date_end Optional character string specifying the end date for
#'   filtering by `date_received` in `"YYYYMMDD"` format (e.g., `"20201231"`).
#'
#' @param api_key Optional character string containing your openFDA API key.
#'   Not required, but recommended for heavy usage to avoid rate limiting.
#'   Register at: https://open.fda.gov/apis/authentication/
#'
#'
#' @return A `tbl_df` containing device adverse event reports with columns:
#'   \describe{
#'     \item{report_number}{MDR report number (unique identifier)}
#'     \item{event_type}{Type of event (e.g., "Malfunction", "Injury", "Death")}
#'     \item{date_received}{Date the report was received by FDA}
#'     \item{device_generic_name}{Generic name of the device}
#'     \item{device_brand_name}{Brand name of the device}
#'     \item{manufacturer_name}{Name of the device manufacturer}
#'     \item{event_description}{Narrative description of the adverse event}
#'     \item{patient_problem}{Reported problems affecting the patient}
#'     \item{device_problem}{Reported problems with the device}
#'   }
#'
#'   Returns an empty tibble with the correct column structure if no results
#'   are found.
#'
#' @references
#' openFDA Device Adverse Event API:
#' https://open.fda.gov/apis/device/event/
#'
#' MAUDE Database Overview:
#' https://open.fda.gov/data/maude/
#'
#' @examples
#' \dontrun{
#' # Search for pacemaker-related adverse events
#' pacemaker_events <- query_maude("pacemaker", limit = 10)
#'
#' # Search by device generic name
#' results <- query_maude("device.generic_name:defibrillator", limit = 50)
#'
#' # Search within a date range
#' results <- query_maude(
#'   search = "insulin pump",
#'   date_start = "20230101",
#'   date_end = "20231231",
#'   limit = 100
#' )
#'
#' # Search by manufacturer
#' results <- query_maude(
#'   search = "device.manufacturer_d_name:medtronic",
#'   limit = 25
#' )
#' }
#'
#' @export
query_maude <- function(
    search,
    limit = 100,
    date_start = NULL,
    date_end = NULL,
    api_key = NULL
) {
  # Input validation
  if (!is.character(search) || length(search) != 1 || nchar(search) == 0) {
    stop("'search' must be a non-empty character string")
  }

  if (!is.numeric(limit) || length(limit) != 1 || limit < 1) {
    stop("'limit' must be a positive integer")
  }
  limit <- as.integer(limit)

  if (!is.null(date_start) && !grepl("^\\d{8}$", date_start)) {
    stop("'date_start' must be in YYYYMMDD format (e.g., '20200101')")
  }

  if (!is.null(date_end) && !grepl("^\\d{8}$", date_end)) {
    stop("'date_end' must be in YYYYMMDD format (e.g., '20201231')")
  }

  if (!is.null(api_key) && (!is.character(api_key) || length(api_key) != 1)) {
    stop("'api_key' must be NULL or a single character string")
  }

  # Build search query
  query <- search
  if (!is.null(date_start) || !is.null(date_end)) {
    date_query <- .build_date_query(date_start, date_end)
    query <- paste0(query, "+AND+", date_query)
  }

  # Paginate if limit > 1000 (openFDA max per request)
  max_per_request <- 1000
  if (limit <= max_per_request) {
    result <- .openfda_request(query, limit, 0, api_key)
  } else {
    # Paginate through results
    all_results <- list()
    n_batches <- ceiling(limit / max_per_request)
    show_progress <- limit > 1000

    for (i in seq_len(n_batches)) {
      skip <- (i - 1) * max_per_request
      batch_limit <- min(max_per_request, limit - skip)

      if (show_progress) {
        message(
          "  Retrieving batch ", i, "/", n_batches,
          " (records ", skip + 1, "-", skip + batch_limit, ")..."
        )
      }

      batch_result <- .openfda_request(query, batch_limit, skip, api_key)
      if (nrow(batch_result) == 0) break
      all_results[[i]] <- batch_result

      # Stop if we got fewer results than requested (end of data)
      if (nrow(batch_result) < batch_limit) break

      # Rate limiting between requests
      Sys.sleep(0.25)
    }

    result <- dplyr::bind_rows(all_results)
  }

  if (nrow(result) == 0) {
    message("No adverse event reports found for query: ", search)
  } else {
    message("Retrieved ", nrow(result), " adverse event report(s)")
  }

  result
}


# Internal function to build date range query
#' @noRd
#' @keywords internal
.build_date_query <- function(date_start, date_end) {
  if (is.null(date_start)) date_start <- "19920101"
  if (is.null(date_end)) date_end <- format(Sys.Date(), "%Y%m%d")
  paste0("date_received:[", date_start, "+TO+", date_end, "]")
}


# Internal function to make openFDA API request
#' @noRd
#' @keywords internal
.openfda_request <- function(search, limit, skip, api_key) {
  base_url <- "https://api.fda.gov/device/event.json"

  query_params <- list(
    search = search,
    limit = limit,
    skip = skip
  )

  if (!is.null(api_key)) {
    query_params$api_key <- api_key
  }

  response <- httr::GET(base_url, query = query_params)

  if (httr::http_error(response)) {
    status <- httr::status_code(response)
    if (status == 404) {
      return(.empty_maude_table())
    }
    stop("openFDA API request failed with status ", status)
  }

  content <- httr::content(response, as = "parsed")

  if (is.null(content$results) || length(content$results) == 0) {
    return(.empty_maude_table())
  }

  .parse_maude_results(content$results)
}


# Internal function to parse MAUDE API results
#' @noRd
#' @keywords internal
.parse_maude_results <- function(results) {
  parsed <- lapply(results, function(record) {
    # Extract device information (may have multiple devices)
    device <- if (!is.null(record$device) && length(record$device) > 0) {
      record$device[[1]]
    } else {
      list()
    }

    # Extract patient problems (concatenate if multiple)
    patient_problem <- tryCatch({
      if (!is.null(record$patient) && length(record$patient) > 0) {
        problems <- lapply(record$patient, function(p) {
          if (!is.null(p$patient_problems)) {
            paste(unlist(p$patient_problems), collapse = "; ")
          } else {
            NA_character_
          }
        })
        paste(na.omit(unlist(problems)), collapse = "; ")
      } else {
        NA_character_
      }
    }, error = function(e) NA_character_)
    if (length(patient_problem) == 0 || patient_problem == "") {
      patient_problem <- NA_character_
    }

    # Extract device problems (concatenate if multiple)
    device_problem <- tryCatch({
      if (!is.null(record$device) && length(record$device) > 0) {
        problems <- lapply(record$device, function(d) {
          if (!is.null(d$device_problem_codes)) {
            paste(unlist(d$device_problem_codes), collapse = "; ")
          } else {
            NA_character_
          }
        })
        paste(na.omit(unlist(problems)), collapse = "; ")
      } else {
        NA_character_
      }
    }, error = function(e) NA_character_)
    if (length(device_problem) == 0 || device_problem == "") {
      device_problem <- NA_character_
    }

    # Extract event narrative texts
    event_description <- tryCatch({
      texts <- c()
      if (!is.null(record$mdr_text) && length(record$mdr_text) > 0) {
        texts <- sapply(record$mdr_text, function(t) {
          if (!is.null(t$text)) t$text else NA_character_
        })
      }
      if (length(texts) > 0 && any(!is.na(texts))) {
        paste(na.omit(texts), collapse = " | ")
      } else {
        NA_character_
      }
    }, error = function(e) NA_character_)

    tibble::tibble(
      report_number = .safe_extract(record, "report_number"),
      event_type = .safe_extract(record, "event_type"),
      date_received = .safe_extract(record, "date_received"),
      device_generic_name = .safe_extract(device, "generic_name"),
      device_brand_name = .safe_extract(device, "brand_name"),
      manufacturer_name = .safe_extract(device, "manufacturer_d_name"),
      event_description = event_description,
      patient_problem = patient_problem,
      device_problem = device_problem
    )
  })

  dplyr::bind_rows(parsed)
}


# Internal function to safely extract values from nested lists
#' @noRd
#' @keywords internal
.safe_extract <- function(x, field) {
  tryCatch({
    val <- x[[field]]
    if (is.null(val) || length(val) == 0) {
      NA_character_
    } else if (is.list(val)) {
      paste(unlist(val), collapse = "; ")
    } else {
      as.character(val)
    }
  }, error = function(e) NA_character_)
}


# Internal function to create empty MAUDE result table
#' @noRd
#' @keywords internal
.empty_maude_table <- function() {
  tibble::tibble(
    report_number = character(0),
    event_type = character(0),
    date_received = character(0),
    device_generic_name = character(0),
    device_brand_name = character(0),
    manufacturer_name = character(0),
    event_description = character(0),
    patient_problem = character(0),
    device_problem = character(0)
  )
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

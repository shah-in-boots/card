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
#' @description
#' `query_maude()` queries the Manufacturer and User Facility Device Experience
#' (MAUDE) database using the openFDA API. This is the recommended interface
#' for most users, providing automatic pagination, date range handling, and
#' input validation.
#'
#' `maude_fda_api_call()` is the lower-level function that makes direct API calls.
#' Use this for advanced scenarios requiring manual pagination control or
#' pre-constructed query strings.
#'
#' @details
#' **Database Coverage:** The openFDA device adverse event endpoint contains
#' reports from mandatory reporters (manufacturers, importers, and device user
#' facilities) and voluntary reporters (healthcare professionals, patients, and
#' consumers). Data covers publicly releasable records from approximately 1992
#' to present and is updated weekly.
#'
#' **Rate Limits:** The openFDA API allows approximately 240 requests per
#' minute (4 per second) without an API key, and 240 requests per minute with
#' a key. Large queries are automatically paginated in batches of up to 1000
#' records.
#'
#' **Search Syntax:** The `search` parameter uses Elasticsearch query syntax.
#' Common patterns include:
#' - Simple term: `"pacemaker"`
#' - Field-specific: `"device.generic_name:pacemaker"`
#' - Multiple terms: `"device.generic_name:pacemaker+AND+event_type:malfunction"`
#' - Date range: `"date_received:[20200101+TO+20201231]"`
#' - Exact phrase: `"device.brand_name:\"Medtronic\""`
#'
#' **API Response Handling:** The openFDA API returns HTTP 404 for queries with
#' no results (rather than an empty array). Both functions handle this by
#' returning an empty tibble instead of throwing an error.
#'
#' **When to use `maude_fda_api_call()`:** Most users should use `query_maude()`.
#' The lower-level `maude_fda_api_call()` is useful when you need:
#' - Direct control over `skip` for custom pagination strategies
#' - Pre-constructed query strings with complex Elasticsearch syntax
#' - Integration into custom retry/error-handling logic
#'
#' @param search Character string specifying the search query. For
#'   `query_maude()`, this can be a simple term (e.g., `"pacemaker"`) or a
#'   field-specific query (e.g., `"device.generic_name:pacemaker"`). For
#'   `maude_fda_api_call()`, this should be the fully constructed query string
#'   including any date filters.
#'
#' @param limit Integer specifying the maximum number of records to return.
#'   For `query_maude()`, defaults to 100 and requests exceeding 1000 are
#'   automatically paginated. For `maude_fda_api_call()`, maximum is 1000 per the
#'   openFDA API limits.
#'
#' @param date_start Optional character string specifying the start date for
#'   filtering by `date_received` in `"YYYYMMDD"` format (e.g., `"20200101"`).
#'   Only used by `query_maude()`.
#'
#' @param date_end Optional character string specifying the end date for
#'   filtering by `date_received` in `"YYYYMMDD"` format (e.g., `"20201231"`).
#'   Only used by `query_maude()`.
#'
#' @param skip Integer specifying the number of records to skip for pagination.
#'   Only used by `maude_fda_api_call()`. Combined with `limit`, allows fetching
#'   records in pages (e.g., skip=0 gets records 1-1000, skip=1000 gets
#'   1001-2000).
#'
#' @param api_key Optional character string containing your openFDA API key.
#'   Not required, but recommended for heavy usage to avoid rate limiting.
#'   Register at: <https://open.fda.gov/apis/authentication/>
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
#'   Returns an empty tibble if no results are found.
#'
#' @references
#' openFDA Device Adverse Event API:
#' <https://open.fda.gov/apis/device/event/>
#'
#' MAUDE Database Overview:
#' <https://open.fda.gov/data/maude/>
#'
#' openFDA API Query Parameters:
#' <https://open.fda.gov/apis/query-parameters/>
#'
#' @examples
#' \dontrun{
#' # --- query_maude(): Recommended for most users ---
#'
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
#' # --- maude_fda_api_call(): Advanced usage with manual pagination ---
#'
#' # Direct API call for first page
#' first_batch <- maude_fda_api_call(
#'   search = "device.generic_name:pacemaker",
#'   limit = 100,
#'   skip = 0,
#'   api_key = NULL
#' )
#'
#' # Get next page
#' second_batch <- maude_fda_api_call(
#'   search = "device.generic_name:pacemaker",
#'   limit = 100,
#'   skip = 100,
#'   api_key = NULL
#' )
#'
#' # Complex pre-built query with date filter baked in
#' results <- maude_fda_api_call(
#'   search = "device.generic_name:pump+AND+date_received:[20220101+TO+20221231]",
#'   limit = 500,
#'   skip = 0,
#'   api_key = Sys.getenv("OPENFDA_API_KEY")
#' )
#' }
#'
#' @name query_maude
NULL

#' @rdname query_maude
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

  # Build search query with optional date range (inlined)
  query <- search
  if (!is.null(date_start) || !is.null(date_end)) {
    ds <- date_start %||% "19920101"
    de <- date_end %||% format(Sys.Date(), "%Y%m%d")
    query <- paste0(query, "+AND+date_received:[", ds, "+TO+", de, "]")
  }

  # Paginate if limit > 1000 (openFDA max per request)
  max_per_request <- 1000
  if (limit <= max_per_request) {
    result <- maude_fda_api_call(query, limit, skip = 0, api_key)
  } else {
    all_results <- list()
    n_batches <- ceiling(limit / max_per_request)

    for (i in seq_len(n_batches)) {
      skip <- (i - 1) * max_per_request
      batch_limit <- min(max_per_request, limit - skip)

      message(
        "Retrieving batch ", i, "/", n_batches,
        " (records ", skip + 1, "-", skip + batch_limit, ")..."
      )

      batch <- maude_fda_api_call(query, batch_limit, skip, api_key)
      if (nrow(batch) == 0) break
      all_results[[i]] <- batch
      if (nrow(batch) < batch_limit) break

      Sys.sleep(0.25) # Rate limiting
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

#' @rdname query_maude
#' @export
maude_fda_api_call <- function(search, limit, skip, api_key) {

  # Build query parameters as a named list.
	# httr::GET accepts a `query` argument that takes a named list and
	# automatically URL-encodes and appends each element as query string
	# parameters (e.g., ?search=pacemaker&limit=100&skip=0).
	# This approach is preferred over manual string concatenation because:
	#   1. httr handles URL encoding of special characters automatically
	#   2. NULL values are automatically omitted from the query string
	#   3. The resulting URL is properly formatted without manual "&" joining
  params <- list(search = search, limit = limit, skip = skip)

  # Conditionally add api_key only if provided.
	# When api_key is NULL, this line is skipped and the parameter is not
	# included in the request, resulting in an unauthenticated call.
  if (!is.null(api_key)) params$api_key <- api_key

  # Make the HTTP GET request to the openFDA device adverse event endpoint.
	# The `query` parameter passes our list, which httr converts to URL params.
	# Example resulting URL:
	#   https://api.fda.gov/device/event.json?search=pacemaker&limit=100&skip=0
  resp <- httr::GET("https://api.fda.gov/device/event.json", query = params)

  # Handle HTTP errors from the API response.
	# The openFDA API returns 404 when no results match the query (rather than
	# returning an empty results array), so we treat 404 as "no results" and
	# return an empty tibble instead of stopping with an error.
	# Other error codes (400 bad request, 429 rate limited, 500 server error)
	# indicate actual problems that should be surfaced to the user.
  if (httr::http_error(resp)) {
    if (httr::status_code(resp) == 404) return(tibble::tibble())
    stop("openFDA API request failed with status ", httr::status_code(resp))
  }

  # Parse JSON response and extract the results array.
	# httr::content with as="parsed" uses jsonlite to convert JSON to R lists.
	# The openFDA response structure is: { "meta": {...}, "results": [...] }
	# We only need the results array; if missing/NULL, default to empty list.
  results <- httr::content(resp, as = "parsed")$results %||% list()
  if (length(results) == 0) return(tibble::tibble())

  # Transform each API result record into a standardized tibble row.
	# purrr::map_dfr iterates over results and row-binds the individual tibbles.
	# This parsing is necessary because the raw API response contains deeply
	# nested structures (device info in arrays, multiple narrative texts, etc.)
	# that need to be flattened into a rectangular data frame format.
  purrr::map_dfr(results, function(rec) {

    # Extract the first device entry from the record.
		# Each MDR report can contain multiple devices, but we extract the primary
		# device (index 1) for the main device fields. The full device list is
		# accessed separately for device_problem_codes.
    device <- purrr::pluck(rec, "device", 1, .default = list())

    # Helper function to extract and collapse nested array fields.
		# Many MAUDE fields (patient_problems, device_problem_codes) are stored as
		# arrays of objects, where each object may contain an array of values.
		# This helper navigates that structure and collapses all values into a
		# single "; " separated string suitable for a data frame column.
		# Example input structure for patient_problems:
		#   [{"patient_problems": ["Arrhythmia", "Chest Pain"]}, ...]
		# Example output: "Arrhythmia; Chest Pain"
    collapse_field <- function(items, field) {
      vals <- purrr::map_chr(items, ~ {
        x <- purrr::pluck(.x, field, .default = NULL)
        if (is.null(x)) NA_character_ else paste(unlist(x), collapse = "; ")
      })
      out <- paste(stats::na.omit(vals), collapse = "; ")
      if (out == "") NA_character_ else out
    }

    # Extract and combine all narrative text entries.
		# MDR reports contain multiple text blocks in mdr_text (e.g., event
		# description from manufacturer, additional info, etc.). We combine all
		# text entries with " | " as a delimiter to preserve all narrative content
		# while keeping it in a single column.
    texts <- purrr::map_chr(
      purrr::pluck(rec, "mdr_text", .default = list()),
      ~ purrr::pluck(.x, "text", .default = NA_character_)
    )
    event_desc <- paste(stats::na.omit(texts), collapse = " | ")

    # Build the standardized output tibble with selected fields.
		# Field selection focuses on the most commonly needed data for adverse
		# event analysis. Additional fields from the raw API response can be
		# accessed by modifying this function or using the API directly.
    tibble::tibble(
      report_number = purrr::pluck(rec, "report_number", .default = NA_character_),
      event_type = purrr::pluck(rec, "event_type", .default = NA_character_),
      date_received = purrr::pluck(rec, "date_received", .default = NA_character_),
      device_generic_name = purrr::pluck(device, "generic_name", .default = NA_character_),
      device_brand_name = purrr::pluck(device, "brand_name", .default = NA_character_),
      manufacturer_name = purrr::pluck(device, "manufacturer_d_name", .default = NA_character_),
      event_description = if (event_desc == "") NA_character_ else event_desc,
      patient_problem = collapse_field(purrr::pluck(rec, "patient", .default = list()), "patient_problems"),
      device_problem = collapse_field(purrr::pluck(rec, "device", .default = list()), "device_problem_codes")
    )
  })
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

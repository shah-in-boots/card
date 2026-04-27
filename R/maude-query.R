# MAUDE Data ----

#' Load FDA MAUDE Coding Resources by Annex
#'
#' @description Load Medical Device Report (MDR) adverse event coding tables
#'   published by the FDA. The `annex` argument selects which FDA annex to load.
#'   The interface is designed to support additional annexes over time as more
#'   code tables are added to the package data.
#'
#' @details The FDA publishes MDR adverse event codes as annexed code tables.
#'   This function returns the annex-specific table bundled with the package.
#'   Supported annexes:
#'   - **A**: Device problem codes
#'   - **E**: Clinical signs, symptoms, or conditions
#'   - **F**: Health impact codes
#'
#' @param annex A single character identifying the FDA annex to load:
#'   `"A"`, `"E"`, or `"F"`. Case-sensitive.
#'
#' @return A `tbl_df` of codes and related metadata for the requested annex.
#'   All returned annexes share the same core columns:
#'   `annex`, `imdrf_code`, `fda_code`, `ncit_code`, `term`,
#'   `level_1`, `level_2`, `level_3`, and `definition`.
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
load_maude_codes <- function(annex) {
  # Validate annex input
  valid_annexes <- c("A", "E", "F")
  if (!(annex %in% valid_annexes)) {
    stop("Invalid annex specified. Valid options are: ",
         paste(valid_annexes, collapse = ", "))
  }

  # Map annex letters to internal data keys
  annex_key <- switch(annex,
    A = "device_problems",
    E = "clinical_signs",
    F = "health_impact"
  )
  codes <- get0("maude_annex_codes", inherits = TRUE)
  if (is.null(codes)) {
    stop("Internal MAUDE annex code data is not available.")
  }

  dat <- codes[[annex_key]]

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
#' **Result Order:** `query_maude()` requests results sorted in reverse
#' chronological order by `date_received` (`date_received:desc`). This provides
#' deterministic pagination for large requests.
#'
#' **Search Syntax:** The `search` parameter uses *Elasticsearch* query syntax.
#' Common patterns include:
#' - Simple term: `"pacemaker"`
#' - Field-specific: `"device.generic_name:pacemaker"`
#' - Multiple terms: `"device.generic_name:pacemaker+AND+event_type:malfunction"`
#' - Date range: `"date_received:[20200101+TO+20201231]"`
#' - Exact phrase: `"device.brand_name:\"Medtronic\""`
#'
#' **Building Queries in `query_maude()`:** `query_maude()` is designed to help
#' you build a valid search string without writing the full query yourself.
#' Use the common field arguments (e.g., `device_generic_name`, `event_type`)
#' to add structured filters, and pass any additional fields through `...`.
#' The `...` names should match openFDA searchable fields, documented at:
#' <https://open.fda.gov/apis/device/event/searchable-fields/>.
#'
#' **API Response Handling:** The openFDA API returns HTTP 404 for queries with
#' no results (rather than an empty array). Both functions handle this by
#' returning an empty tibble instead of throwing an error.
#'
#' **When to use `maude_fda_api_call()`:** Most users should use `query_maude()`.
#' The lower-level `maude_fda_api_call()` is useful when you need:
#' - Direct control over `skip` for custom pagination strategies
#' - Pre-constructed query strings with complex *Elasticsearch* syntax
#' - Integration into custom retry/error-handling logic
#' - Full control over `search_query`, `sort`, and other openFDA parameters
#'
#' @param search Character string specifying the search query or `NULL`. For
#'   `query_maude()`, this can be a simple term (e.g., `"pacemaker"`) or a
#'   field-specific query (e.g., `"device.generic_name:pacemaker"`). Use the
#'   other `query_maude()` arguments to add additional filters.
#'
#' @param search_query Fully constructed query string to send to the openFDA
#'   API (advanced use). This should include any date or field filters that you
#'   want applied exactly as written.
#'
#' @param device_generic_name Optional character vector of device generic names
#'   to filter on (`device.generic_name`).
#'
#' @param device_brand_name Optional character vector of device brand names to
#'   filter on (`device.brand_name`).
#'
#' @param manufacturer_name Optional character vector of manufacturer names to
#'   filter on (`device.manufacturer_d_name`).
#'
#' @param event_type Optional character vector of event types to filter on
#'   (`event_type`).
#'
#' @param report_number Optional character vector of MDR report numbers to
#'   filter on (`report_number`).
#'
#' @param device_problem Optional character vector of device problem codes to
#'   filter on (`device.device_problem_codes`).
#'
#' @param patient_problem Optional character vector of patient problems to
#'   filter on (`patient.patient_problems`).
#'
#' @param ... Additional named search fields and values to include in the
#'   query. Names should match the openFDA searchable fields list.
#'
#' @param limit Integer specifying the maximum number of records to return.
#'   For `query_maude()`, defaults to 100 and requests exceeding 1000 are
#'   automatically paginated. Due to openFDA `skip` limits, `query_maude()`
#'   currently supports up to 26,000 records per call. For
#'   `maude_fda_api_call()`, maximum per request is 1000 per openFDA limits.
#'
#' @param date_start Optional start date for filtering by `date_received`.
#'   Accepts `Date`, POSIXt, `"YYYYMMDD"`, or `"YYYY-MM-DD"` formats. Only used
#'   by `query_maude()`.
#'
#' @param date_end Optional end date for filtering by `date_received`. Accepts
#'   `Date`, POSIXt, `"YYYYMMDD"`, or `"YYYY-MM-DD"` formats. Only used by
#'   `query_maude()`.
#'
#' @param skip Integer specifying the number of records to skip for pagination.
#'   Only used by `maude_fda_api_call()`. Combined with `limit`, allows fetching
#'   records in pages (e.g., skip=0 gets records 1-1000, skip=1000 gets
#'   1001-2000).
#'
#' @param sort Optional sort specification for the openFDA API (e.g.,
#'   `"date_received:desc"`). Only used by `maude_fda_api_call()`.
#'
#' @param api_key Optional character string containing your openFDA API key.
#'   Not required, but recommended for heavy usage to avoid rate limiting.
#'   Register at: <https://open.fda.gov/apis/authentication/>
#'
#' @param verbose Logical. If `TRUE`, prints progress messages for pagination,
#'   retries, and total records retrieved. Defaults to `interactive()`.
#'
#' @param max_retries Integer giving the number of retry attempts for transient
#'   API failures in `maude_fda_api_call()` (HTTP 429/5xx). Defaults to `3`.
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
#'   Returns an empty *tibble* if no results are found.
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
#' # Search for pacemaker-related adverse events
#' pacemaker_events <- query_maude("pacemaker", limit = 10)
#'
#' # Search by device generic name
#' results <- query_maude(device_generic_name = "defibrillator", limit = 50)
#'
#' # Add an extra searchable field via ...
#' results <- query_maude(
#'   device_generic_name = "infusion pump",
#'   device.product_code = "LVP",
#'   limit = 50
#' )
#' }
#'
#' @name query_maude
NULL

#' @rdname query_maude
#' @export
query_maude <- function(
    search = NULL,
    device_generic_name = NULL,
    device_brand_name = NULL,
    manufacturer_name = NULL,
    event_type = NULL,
    report_number = NULL,
    device_problem = NULL,
    patient_problem = NULL,
    ...,
    limit = 100,
    date_start = NULL,
    date_end = NULL,
    api_key = NULL,
    verbose = interactive()
) {
  parse_date_arg <- function(x, arg_name) {
    if (is.null(x)) {
      return(NULL)
    }

    if (length(x) != 1) {
      stop("'", arg_name, "' must be a single date value")
    }

    if (inherits(x, "Date")) {
      return(format(x, "%Y%m%d"))
    }

    if (inherits(x, "POSIXt")) {
      return(format(as.Date(x), "%Y%m%d"))
    }

    if (is.character(x)) {
      if (grepl("^\\d{8}$", x)) {
        return(x)
      }
      parsed <- as.Date(x)
      if (is.na(parsed)) {
        stop(
          "'",
          arg_name,
          "' must be a Date, POSIXt, or YYYYMMDD/YYYY-MM-DD string"
        )
      }
      return(format(parsed, "%Y%m%d"))
    }

    stop("'", arg_name, "' must be a Date, POSIXt, or YYYYMMDD/YYYY-MM-DD string")
  }

  build_field_terms <- function(field, values) {
    if (is.null(values)) {
      return(character(0))
    }

    values <- values[!is.na(values)]
    if (!length(values)) {
      return(character(0))
    }
    if (!is.character(values)) {
      stop("'", field, "' must be a character vector")
    }

    escaped <- gsub(
      "([+\\-=&|><!(){}\\[\\]^\"~*?:\\\\/])",
      "\\\\\\1",
      values,
      perl = TRUE
    )
    escaped <- ifelse(grepl("\\s", escaped), paste0("\"", escaped, "\""), escaped)

    if (length(escaped) == 1) {
      return(paste0(field, ":", escaped))
    }

    paste0(field, ":(", paste(escaped, collapse = "+OR+"), ")")
  }

  # Basic input validation.
  if (!is.null(search) &&
      (!is.character(search) || length(search) != 1 || nchar(search) == 0)) {
    stop("'search' must be NULL or a non-empty character string")
  }

  if (!is.numeric(limit) || length(limit) != 1 || is.na(limit) ||
      !is.finite(limit) || limit < 1 || as.integer(limit) != limit) {
    stop("'limit' must be a positive integer")
  }
  limit <- as.integer(limit)
  if (limit > 26000L) {
    stop(
      "'limit' cannot exceed 26000 with openFDA skip/limit pagination. ",
      "Narrow your query, split by date ranges, or use search_after workflows."
    )
  }

  date_start <- parse_date_arg(date_start, "date_start")
  date_end <- parse_date_arg(date_end, "date_end")

  if (!is.null(api_key) && (!is.character(api_key) || length(api_key) != 1)) {
    stop("'api_key' must be NULL or a single character string")
  }
  if (!is.logical(verbose) || length(verbose) != 1 || is.na(verbose)) {
    stop("'verbose' must be TRUE or FALSE")
  }

  if (!is.null(date_start) && !is.null(date_end) && date_start > date_end) {
    stop(
      "'date_start' (", date_start, ") must be on or before ",
      "'date_end' (", date_end, ")"
    )
  }

  # Collect extra terms supplied via ...
  extra_terms <- list(...)
  if (length(extra_terms) > 0) {
    if (is.null(names(extra_terms)) || any(names(extra_terms) == "")) {
      stop("All '...' inputs must be named with openFDA field names")
    }
  }

  # Build the search terms from common fields and extras.
  terms <- character(0)
  if (!is.null(search)) terms <- c(terms, search)

  field_map <- list(
    "device.generic_name" = device_generic_name,
    "device.brand_name" = device_brand_name,
    "device.manufacturer_d_name" = manufacturer_name,
    "event_type" = event_type,
    "report_number" = report_number,
    "device.device_problem_codes" = device_problem,
    "patient.patient_problems" = patient_problem
  )

  for (field in names(field_map)) {
    terms <- c(terms, build_field_terms(field, field_map[[field]]))
  }

  for (field in names(extra_terms)) {
    terms <- c(terms, build_field_terms(field, extra_terms[[field]]))
  }

  # Assemble the final query string, including an optional date range.
  clauses <- terms[!is.na(terms)]
  if (!is.null(date_start) || !is.null(date_end)) {
    ds <- if (is.null(date_start)) "19920101" else date_start
    de <- if (is.null(date_end)) format(Sys.Date(), "%Y%m%d") else date_end
    clauses <- c(clauses, paste0("date_received:[", ds, "+TO+", de, "]"))
  }
  if (length(clauses) == 0) {
    stop(
      "Provide 'search', at least one field filter, or a date range ",
      "to build a query"
    )
  }
  query <- paste(clauses, collapse = "+AND+")

  # Paginate if limit > 1000 (openFDA max per request).
  max_per_request <- 1000
  sort <- "date_received:desc"
  if (limit <= max_per_request) {
    result <- maude_fda_api_call(
      search_query = query,
      limit = limit,
      skip = 0,
      api_key = api_key,
      sort = sort,
      verbose = verbose
    )
  } else {
    all_results <- list()
    n_batches <- ceiling(limit / max_per_request)

    for (i in seq_len(n_batches)) {
      skip <- (i - 1) * max_per_request
      batch_limit <- min(max_per_request, limit - skip)

      if (verbose) {
        message(
          "Retrieving batch ", i, "/", n_batches,
          " (records ", skip + 1, "-", skip + batch_limit, ")..."
        )
      }

      batch <- maude_fda_api_call(
        search_query = query,
        limit = batch_limit,
        skip = skip,
        api_key = api_key,
        sort = sort,
        verbose = verbose
      )
      if (nrow(batch) == 0) break
      all_results[[i]] <- batch
      if (nrow(batch) < batch_limit) break

      Sys.sleep(0.26) # Keep below openFDA's ~4 requests/second guidance
    }

    result <- dplyr::bind_rows(all_results)
  }

  # Provide a short message about the outcome.
  if (verbose) {
    if (nrow(result) == 0) {
      message("No adverse event reports found for query: ", query)
    } else {
      message("Retrieved ", nrow(result), " adverse event report(s)")
    }
  }

  result
}

#' @rdname query_maude
#' @export
maude_fda_api_call <- function(
  search_query,
  limit,
  skip,
  api_key,
  sort = NULL,
  max_retries = 3,
  verbose = FALSE
) {

  # This is or query parameters for "direct" calling the API, not user friendly
  # Format =  "?search=pacemaker&limit=100&skip=0" (gets appended to URL)
  # The `httr::GET()` function accepts the query argument in form of named list
  #
  # Why?
  #   1. httr handles URL encoding of special characters automatically
  #   2. NULL values are automatically omitted from the query string
  #   3. The resulting URL is properly formatted without manual "&" joining
  params <- list(search = search_query, limit = limit, skip = skip)

  # Conditionally add api_key only if provided.
  # When api_key is NULL, this line is skipped and the parameter is not
  # included in the request, resulting in an unauthenticated call.
  if (!is.null(api_key)) {
    params$api_key <- api_key
  }
  if (!is.null(sort)) {
    params$sort <- sort
  }

  # Make the HTTP GET request to the openFDA device adverse event endpoint.
  # The `query` parameter passes the list
  # Example URL response:
  #   https://api.fda.gov/device/event.json?search=pacemaker&limit=100&skip=0
  attempt <- 1L
  max_attempts <- max_retries + 1L

  repeat {
    resp <- httr::GET("https://api.fda.gov/device/event.json", query = params)
    req <- list(
      status = httr::status_code(resp),
      error = httr::http_error(resp),
      parsed = tryCatch(
        httr::content(resp, as = "parsed", encoding = "UTF-8"),
        error = function(e) NULL
      )
    )

    # Handle HTTP errors from the API response.
    # The openFDA API returns 404 when no results match the query.
    # We return an empty tibble in that case.
    if (!req$error) {
      break
    }

    status <- req$status
    if (status == 404) {
      return(tibble::tibble())
    }

    retryable <- status %in% c(429, 500, 502, 503, 504) && attempt < max_attempts
    if (retryable) {
      wait_seconds <- min(2 ^ (attempt - 1), 8)
      if (verbose) {
        message(
          "openFDA request failed with status ", status, ". Retrying in ",
          wait_seconds, " second(s)..."
        )
      }
      Sys.sleep(wait_seconds)
      attempt <- attempt + 1L
      next
    }

    err <- ""
    if (is.list(req$parsed) && is.list(req$parsed$error)) {
      err <- req$parsed$error$message
      if (is.null(err)) {
        err <- ""
      } else {
        err <- as.character(err)[1]
        if (is.na(err) || !nchar(err)) {
          err <- ""
        }
      }
    }
    if (nchar(err) > 0) {
      stop("openFDA API request failed with status ", status, ": ", err, call. = FALSE)
    }
    stop("openFDA API request failed with status ", status, call. = FALSE)
  }

  # Parse JSON response and extract the results array.
  # httr::content with as="parsed" uses jsonlite to convert JSON to R lists.
  # The openFDA response structure is: { "meta": {...}, "results": [...] }
  # We only need the results array; if missing/NULL, default to empty list.
  parsed <- req$parsed
  if (!is.list(parsed)) {
    return(tibble::tibble())
  }
  results <- parsed$results
  if (is.null(results)) {
    results <- list()
  }
  if (length(results) == 0) {
    return(tibble::tibble())
  }

  # Transform each API result record into a standardized tibble row.
  # purrr::map_dfr iterates over results and row-binds the individual tibbles.
  # Parsed results are very nested
  purrr::map_dfr(results, flatten_maude_record)
}

# OpenFDA MAUDE API helpers ----

#' Collect a named field from nested openFDA MAUDE structures
#'
#' @description Internal helper used by `flatten_maude_record()` to normalize
#'   openFDA MAUDE fields that may arrive as nested lists, list-columns, or
#'   data frames. It walks the structure recursively and returns all non-empty
#'   character values stored under `field`.
#'
#' @param x Nested MAUDE field content.
#' @param field Scalar character string naming the field to collect.
#'
#' @return A character vector of non-missing values found for `field`.
#'
#' @keywords internal
#' @noRd
collect_maude_field_values <- function(x, field) {
  normalize_character <- function(value) {
    if (is.null(value) || length(value) == 0) {
      return(character(0))
    }

    vals <- as.character(unlist(value, use.names = FALSE))
    vals <- vals[!is.na(vals)]
    if (!length(vals)) {
      return(character(0))
    }

    vals <- trimws(vals)
    vals[nzchar(vals)]
  }

  if (is.null(x) || length(x) == 0) {
    return(character(0))
  }

  if (is.data.frame(x)) {
    if (!(field %in% names(x))) {
      return(character(0))
    }
    return(normalize_character(x[[field]]))
  }

  if (!is.list(x)) {
    return(character(0))
  }

  out <- character(0)

  if (!is.null(names(x)) && field %in% names(x)) {
    out <- c(out, normalize_character(x[[field]]))
  }

  nested <- unlist(
    lapply(x, function(entry) {
      if (is.list(entry) || is.data.frame(entry)) {
        collect_maude_field_values(entry, field)
      } else {
        character(0)
      }
    }),
    use.names = FALSE
  )

  c(out, nested)
}

#' Flatten one openFDA MAUDE record into the package's output schema
#'
#' @description Internal helper used by `maude_fda_api_call()` to convert a
#'   single parsed MAUDE API record into the standard tibble row returned by
#'   `query_maude()`. This centralizes the logic for preserving event narrative
#'   text and normalizing variable nested shapes from the API response.
#'
#' @param rec A single parsed MAUDE record from the openFDA API response.
#'
#' @return A one-row tibble in the `query_maude()` output format.
#'
#' @keywords internal
#' @noRd
flatten_maude_record <- function(rec) {
  first_value <- function(x) {
    vals <- as.character(unlist(x, use.names = FALSE))
    vals <- vals[!is.na(vals)]
    if (!length(vals)) {
      return(NA_character_)
    }

    vals <- trimws(vals)
    vals <- vals[nzchar(vals)]
    if (!length(vals)) {
      return(NA_character_)
    }

    vals[[1]]
  }

  collapse_field <- function(x, field, delimiter = "; ") {
    vals <- collect_maude_field_values(x, field)
    if (!length(vals)) {
      return(NA_character_)
    }

    paste(vals, collapse = delimiter)
  }

  devices <- purrr::pluck(rec, "device", .default = list())

  tibble::tibble(
  	mdr_report_key = purrr::pluck(rec, "mdr_report_key", .default = NA_character_),
    report_number = first_value(
      purrr::pluck(rec, "report_number", .default = NULL)
    ),
    event_type = first_value(
      purrr::pluck(rec, "event_type", .default = NULL)
    ),
    date_received = first_value(
      purrr::pluck(rec, "date_received", .default = NULL)
    ),
    device_generic_name = first_value(
      collect_maude_field_values(devices, "generic_name")
    ),
    device_brand_name = first_value(
      collect_maude_field_values(devices, "brand_name")
    ),
    manufacturer_name = first_value(
      collect_maude_field_values(devices, "manufacturer_d_name")
    ),
    event_description = collapse_field(
      purrr::pluck(rec, "mdr_text", .default = list()),
      "text",
      delimiter = " | "
    ),
    patient_problem = collapse_field(
      purrr::pluck(rec, "patient", .default = list()),
      "patient_problems"
    ),
    device_problem = collapse_field(
      devices,
      "device_problem_codes"
    )
  )
}

#' Fill missing MAUDE event descriptions using FDA narrative text files
#'
#' @description Helper function that supplements the output of
#'   `query_maude()` by filling in missing `event_description` values.
#'   For records where the openFDA API does not return narrative text,
#'   this function retrieves the corresponding text from the FDA MAUDE
#'   downloadable narrative text files and joins it back using
#'   `mdr_report_key`.
#'
#'   This function is designed as a best-effort backfill utility. It does
#'   not modify or overwrite existing `event_description` values returned
#'   by `query_maude()`, and only fills values that are missing or empty.
#'
#'   The function relies on FDA-provided bulk text files, which are
#'   periodically updated and may not include complete coverage for the
#'   current year. As a result, some records may remain unfilled even after
#'   this function is applied.
#'
#'   This function may download MAUDE text files from the FDA website and
#'   cache them locally in `cache_dir`. File availability, structure, and
#'   naming conventions are determined by the FDA and may change over time.
#'
#' @param events A tibble returned by `query_maude()`, including
#'   `mdr_report_key`, `event_description`, and `date_received`.
#' @param key_col Name of the column containing the MDR report key.
#' @param desc_col Name of the column containing event descriptions.
#' @param date_col Name of the column containing event dates.
#' @param cache_dir Directory used to store downloaded MAUDE text files.
#' @param quiet Logical; if `FALSE`, prints progress messages.
#'
#' @return A tibble with missing `event_description` values filled where possible.
#'
#' @keywords internal
fill_maude_event_descriptions <- function(
		events,
		key_col = "mdr_report_key",
		desc_col = "event_description",
		date_col = "date_received",
		cache_dir = file.path(tempdir(), "maude_text_cache"),
		quiet = FALSE
) {
	needed <- c("dplyr", "readr", "purrr", "tibble")
	missing_pkgs <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
	if (length(missing_pkgs) > 0) {
		stop("Please install required packages: ", paste(missing_pkgs, collapse = ", "))
	}

	if (!is.data.frame(events)) {
		stop("'events' must be a data.frame or tibble")
	}

	for (nm in c(key_col, desc_col, date_col)) {
		if (!nm %in% names(events)) {
			stop("Column not found in events: ", nm)
		}
	}

	normalize_date <- function(x) {
		if (inherits(x, "Date")) return(x)

		x <- as.character(x)
		x <- trimws(x)
		x[x == ""] <- NA_character_

		out <- as.Date(x, format = "%Y%m%d")

		bad <- is.na(out) & !is.na(x)
		if (any(bad)) {
			out[bad] <- as.Date(x[bad])
		}

		out
	}

	clean_utf8 <- function(x) {
		x <- as.character(x)
		x <- iconv(x, from = "", to = "UTF-8", sub = "")
		x[is.na(x)] <- ""
		x
	}

	events[[date_col]] <- normalize_date(events[[date_col]])

	need_fill <- is.na(events[[desc_col]]) | trimws(as.character(events[[desc_col]])) == ""
	if (!any(need_fill)) {
		if (!quiet) message("No missing event descriptions found.")
		return(events)
	}

	keys_needed <- unique(as.character(events[[key_col]][need_fill]))
	keys_needed <- keys_needed[!is.na(keys_needed) & nzchar(keys_needed)]

	if (length(keys_needed) == 0) {
		stop("No usable MDR report key values found for rows with missing descriptions.")
	}

	yrs <- unique(format(stats::na.omit(events[[date_col]][need_fill]), "%Y"))
	yrs <- yrs[!is.na(yrs) & nzchar(yrs)]

	if (length(yrs) == 0) {
		stop("Could not determine year(s) from ", date_col)
	}

	if (!dir.exists(cache_dir)) {
		dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
	}

	get_text_sources <- function(year) {
		year <- as.character(year)
		current_year <- format(Sys.Date(), "%Y")

		if (year <= "1995") {
			return("foitextthru1995.zip")
		}

		if (year < current_year) {
			return(sprintf("foitext%s.zip", year))
		}

		if (year == current_year) {
			return(c("foitextadd.zip", "foitextchange.zip"))
		}

		stop("Year ", year, " is in the future or not yet available from FDA")
	}

	maude_text_url <- function(file_name) {
		sprintf("https://www.accessdata.fda.gov/MAUDE/ftparea/%s", file_name)
	}

	get_text_file <- function(file_name) {
		zip_path <- file.path(cache_dir, file_name)
		out_dir <- file.path(cache_dir, tools::file_path_sans_ext(file_name))

		if (!file.exists(zip_path)) {
			if (!quiet) message("Downloading MAUDE text file: ", file_name, " ...")

			download_ok <- tryCatch(
				{
					utils::download.file(
						maude_text_url(file_name),
						destfile = zip_path,
						mode = "wb",
						quiet = quiet
					)
					TRUE
				},
				error = function(e) {
					if (!quiet) {
						message("Failed to download MAUDE text file: ", file_name)
						message("Reason: ", conditionMessage(e))
						message("This file will be skipped.")
					}
					FALSE
				}
			)

			if (!download_ok || !file.exists(zip_path)) {
				return(NULL)
			}
		}

		if (!dir.exists(out_dir)) {
			unzip_ok <- tryCatch(
				{
					dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
					utils::unzip(zip_path, exdir = out_dir)
					TRUE
				},
				error = function(e) {
					if (!quiet) {
						message("Failed to unzip MAUDE text file: ", file_name)
						message("Reason: ", conditionMessage(e))
						message("This file will be skipped.")
					}
					FALSE
				}
			)

			if (!unzip_ok) {
				return(NULL)
			}
		}

		txts <- list.files(
			out_dir,
			pattern = "\\.txt$",
			full.names = TRUE,
			ignore.case = TRUE
		)

		if (length(txts) == 0) {
			if (!quiet) {
				message("No .txt file found after unzipping MAUDE file: ", file_name)
				message("This file will be skipped.")
			}
			return(NULL)
		}

		txts[1]
	}

	read_text_file <- function(path) {
		dat <- readr::read_delim(
			file = path,
			delim = "|",
			col_names = FALSE,
			show_col_types = FALSE,
			progress = FALSE,
			quote = ""
		)

		if (ncol(dat) < 6) {
			stop("Unexpected MAUDE text file structure in: ", path)
		}

		names(dat)[1:6] <- c(
			"mdr_report_key",
			"mdr_text_key",
			"text_type_code",
			"patient_sequence_number",
			"date_report",
			"text"
		)

		dat |>
			dplyr::mutate(
				mdr_report_key = as.character(.data$mdr_report_key),
				text_type_code = as.character(.data$text_type_code),
				text = clean_utf8(.data$text)
			) |>
			dplyr::select(.data$mdr_report_key, .data$text_type_code, .data$text)
	}

	files_needed <- unique(unlist(lapply(yrs, get_text_sources)))

	skipped_files <- character(0)

	text_rows <- purrr::map_dfr(files_needed, function(f) {
		txt <- get_text_file(f)

		if (is.null(txt)) {
			skipped_files <<- c(skipped_files, f)
			return(tibble::tibble())
		}

		dat <- tryCatch(
			read_text_file(txt),
			error = function(e) {
				if (!quiet) {
					message("Failed to read MAUDE text file: ", f)
					message("Reason: ", conditionMessage(e))
					message("This file will be skipped.")
				}
				skipped_files <<- c(skipped_files, f)
				tibble::tibble()
			}
		)

		if (nrow(dat) == 0) {
			return(tibble::tibble())
		}

		dat |>
			dplyr::filter(.data$mdr_report_key %in% keys_needed)
	})

	if (length(skipped_files) > 0 && !quiet) {
		message(
			"Skipped ",
			length(unique(skipped_files)),
			" MAUDE text file(s): ",
			paste(unique(skipped_files), collapse = ", ")
		)
	}

	if (nrow(text_rows) == 0) {
		if (!quiet) message("No matching text rows found in available MAUDE text files.")
		return(events)
	}

	text_rows$text <- clean_utf8(text_rows$text)

	text_join <- text_rows |>
		dplyr::filter(trimws(.data$text) != "") |>
		dplyr::group_by(.data$mdr_report_key) |>
		dplyr::summarise(
			mdr_text_file_description = paste(unique(.data$text), collapse = " | "),
			.groups = "drop"
		)

	out <- events |>
		dplyr::left_join(
			text_join,
			by = stats::setNames("mdr_report_key", key_col)
		) |>
		dplyr::mutate(
			!!desc_col := dplyr::if_else(
				is.na(.data[[desc_col]]) | trimws(as.character(.data[[desc_col]])) == "",
				.data$mdr_text_file_description,
				.data[[desc_col]]
			)
		) |>
		dplyr::select(-mdr_text_file_description)

	if (!quiet) {
		before_n <- sum(is.na(events[[desc_col]]) | trimws(as.character(events[[desc_col]])) == "")
		after_n  <- sum(is.na(out[[desc_col]]) | trimws(as.character(out[[desc_col]])) == "")
		message("Filled ", before_n - after_n, " missing event description(s).")
	}

	out
}

#' Fill missing MAUDE event descriptions using MAUDE web pages
#'
#' @description Helper function that supplements the output of
#'   `query_maude()` by filling in missing `event_description` values
#'   using data scraped from individual MAUDE report web pages.
#'   For records where neither the openFDA API nor FDA downloadable
#'   text files provide narrative text, this function retrieves
#'   narrative sections from the MAUDE web interface using
#'   `mdr_report_key` and combines them into a single text field.
#'
#'   This function is designed as a best-effort fallback utility.
#'   It does not modify or overwrite existing `event_description`
#'   values returned by `query_maude()` or other backfill functions,
#'   and only fills values that are missing or empty.
#'
#'   The function attempts to extract multiple narrative sections
#'   (e.g., "Event or Problem Description", "Manufacturer Narrative",
#'   "Additional Manufacturer Narrative") and concatenates them
#'   into a single string. The availability and structure of these
#'   sections are determined by the FDA MAUDE web interface and may
#'   vary across reports.
#'
#'   This function relies on web scraping of FDA MAUDE pages and is
#'   inherently more fragile than API- or file-based approaches.
#'   Changes to the MAUDE website structure or section labels may
#'   affect its ability to extract narrative text. Additionally,
#'   some reports (e.g., older records) may not be available via
#'   the web interface and will remain unfilled.
#'
#'   The function performs one web request per missing report and
#'   includes a delay (`pause_seconds`) between requests to reduce
#'   load on FDA servers.
#'
#' @param events A tibble returned by `query_maude()`, including
#'   `mdr_report_key` and `event_description`.
#' @param key_col Name of the column containing the MDR report key.
#' @param desc_col Name of the column containing event descriptions.
#' @param pause_seconds Numeric delay (in seconds) between web requests.
#' @param quiet Logical; if `FALSE`, prints progress messages.
#'
#' @return A tibble with missing `event_description` values filled where possible.
#'
#' @keywords internal
fill_maude_descriptions_from_web <- function(
		events,
		key_col = "mdr_report_key",
		desc_col = "event_description",
		pause_seconds = 0.25,
		quiet = FALSE
) {
	needed <- c("rvest", "xml2", "dplyr", "purrr", "stringr", "tibble")
	missing_pkgs <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]

	if (length(missing_pkgs) > 0) {
		stop("Please install required packages: ", paste(missing_pkgs, collapse = ", "))
	}

	if (!key_col %in% names(events)) stop("Column not found: ", key_col)
	if (!desc_col %in% names(events)) stop("Column not found: ", desc_col)

	clean_text <- function(x) {
		x <- as.character(x)
		x <- iconv(x, from = "", to = "UTF-8", sub = "")
		x <- stringr::str_squish(x)
		x[x == ""] <- NA_character_
		x
	}

	extract_all_sections_in_order <- function(page_text, section_labels, stop_labels) {
		label_pattern <- paste(section_labels, collapse = "|")
		locs <- stringr::str_locate_all(page_text, label_pattern)[[1]]

		if (nrow(locs) == 0) {
			return(character(0))
		}

		pieces <- character(0)

		for (i in seq_len(nrow(locs))) {
			start <- locs[i, "end"] + 1

			next_section_start <- if (i < nrow(locs)) locs[i + 1, "start"] - 1 else nchar(page_text)

			section_text_raw <- substr(page_text, start, next_section_start)

			# For the final section, prevent accidentally capturing the rest of the webpage.
			stop_pattern <- paste(stop_labels, collapse = "|")
			stop_loc <- stringr::str_locate(section_text_raw, stop_pattern)

			if (!all(is.na(stop_loc))) {
				section_text_raw <- substr(section_text_raw, 1, stop_loc[1, "start"] - 1)
			}

			section_text <- clean_text(section_text_raw)

			if (!is.na(section_text) && nzchar(section_text)) {
				pieces <- c(pieces, section_text)
			}
		}

		pieces
	}

	extract_maude_web_narrative <- function(mdr_report_key) {
		url <- paste0(
			"https://www.accessdata.fda.gov/scripts/cdrh/cfdocs/cfMAUDE/detail.cfm?mdrfoi__id=",
			mdr_report_key
		)

		page <- tryCatch(
			xml2::read_html(url),
			error = function(e) NULL
		)

		if (is.null(page)) return(NA_character_)

		page_text <- tryCatch(
			page |>
				rvest::html_element("body") |>
				rvest::html_text2(),
			error = function(e) NA_character_
		)

		page_text <- clean_text(page_text)
		if (is.na(page_text)) return(NA_character_)

		section_labels <- c(
			"Event or Problem Description",
			"Manufacturer Narrative",
			"Additional Manufacturer Narrative",
			"Manufacturer Evaluation",
			"Manufacturer Evaluation Summary",
			"Device Evaluation"
		)

		stop_labels <- c(
			"Event Problem and Evaluation Codes",
			"Brand Name",
			"Common Device Name",
			"Product Code",
			"Manufacturer",
			"Reporter",
			"Patient",
			"Device",
			"MDR Report Key",
			"Search Alerts/Recalls"
		)

		pieces <- extract_all_sections_in_order(
			page_text = page_text,
			section_labels = section_labels,
			stop_labels = stop_labels
		)

		if (length(pieces) == 0) return(NA_character_)

		paste(pieces, collapse = " | ")
	}

	need_fill <- is.na(events[[desc_col]]) | trimws(as.character(events[[desc_col]])) == ""

	keys <- unique(as.character(events[[key_col]][need_fill]))
	keys <- keys[!is.na(keys) & nzchar(keys)]

	if (length(keys) == 0) {
		if (!quiet) message("No missing descriptions to fill from MAUDE web pages.")
		return(events)
	}

	if (!quiet) {
		message("Attempting web fallback for ", length(keys), " MDR report key(s)...")
	}

	failed_keys <- character(0)

	lookup <- purrr::map_dfr(keys, function(k) {
		if (!quiet) message("Checking MDR report key: ", k)
		Sys.sleep(pause_seconds)

		narrative <- tryCatch(
			extract_maude_web_narrative(k),
			error = function(e) {
				failed_keys <<- c(failed_keys, k)
				if (!quiet) message("Skipping MDR report key ", k, ": ", conditionMessage(e))
				NA_character_
			}
		)

		if (is.na(narrative)) {
			failed_keys <<- c(failed_keys, k)
		}

		tibble::tibble(
			mdr_report_key = k,
			web_event_description = narrative
		)
	})

	out <- events |>
		dplyr::left_join(
			lookup,
			by = stats::setNames("mdr_report_key", key_col)
		) |>
		dplyr::mutate(
			!!desc_col := dplyr::if_else(
				is.na(.data[[desc_col]]) | trimws(as.character(.data[[desc_col]])) == "",
				.data$web_event_description,
				.data[[desc_col]]
			)
		) |>
		dplyr::select(-web_event_description)

	if (!quiet) {
		before_n <- sum(is.na(events[[desc_col]]) | trimws(as.character(events[[desc_col]])) == "")
		after_n  <- sum(is.na(out[[desc_col]]) | trimws(as.character(out[[desc_col]])) == "")

		message("Filled ", before_n - after_n, " missing description(s) from MAUDE web pages.")
		message(
			"Web lookup failures: ",
			length(unique(failed_keys)),
			" / ",
			length(keys),
			" report(s)."
		)
	}

	out
}
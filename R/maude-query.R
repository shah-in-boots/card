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
#' # Hierarchy
#'
#' Each row carries one coded term in `term`, and `level_1` through `level_3`
#' give that term's place in the annex hierarchy, read from the IMDRF code
#' rather than inferred. A three-character code such as `A05` is a *family*, and
#' on those rows `term` and `level_1` are the same value while `level_2` and
#' `level_3` are `NA`. Deeper codes carry their ancestors, so `A0501` has
#' `level_1` of `"Mechanical Problem"` and `level_2` of its own term.
#'
#' This matters because FDA codes a substantial share of reports **at the family
#' level**: `"Material Integrity Problem"` and `"Use of Device Problem"` are
#' values that appear in the data, not merely headings above it. Joining
#' returned MAUDE terms to this table on `term` therefore picks up leaf and
#' family codings alike, and `level_1` rolls either up to its family.
#'
#' # Polyhierarchy in Annex E
#'
#' Annex E places some terms under more than one family -- `"Brain Injury"`
#' belongs to both `"Nervous System"` and `"Injury"` -- so those codes appear on
#' one row per parent. A join on `term` or `imdrf_code` will multiply such rows,
#' which is correct but will double-count if the result is then tallied. Reduce
#' to `imdrf_code` first, or pick a parent, before counting.
#'
#' @param annex A single character identifying the FDA annex to load:
#'   `"A"`, `"E"`, or `"F"`. Case-sensitive.
#'
#' @return A `tbl_df` of codes and related metadata for the requested annex.
#'   All returned annexes share the same core columns:
#'   `annex`, `imdrf_code`, `fda_code`, `ncit_code`, `term`,
#'   `level_1`, `level_2`, `level_3`, and `definition`. `level_1` is never
#'   missing; `level_2` and `level_3` are missing for terms that do not go that
#'   deep.
#'
#' @references
#' FDA MDR Adverse Event Codes: Coding Resources for Medical Device Reports
#' https://www.fda.gov/medical-devices/mdr-adverse-event-codes/coding-resources-medical-device-reports
#'
#' @examples
#' # Load Annex E health effects codes
#' annex_e <- load_maude_codes("E")
#'
#' # Roll device problem terms up to their family, leaf or family coded alike
#' terms <- c("Material Deformation", "Material Integrity Problem")
#' annex_a <- load_maude_codes("A")
#' annex_a$level_1[match(terms, annex_a$term)]
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
#' `maude_query()` queries the Manufacturer and User Facility Device Experience
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
#' **Result Order:** `maude_query()` requests results sorted in reverse
#' chronological order by `date_received` (`date_received:desc`). This provides
#' deterministic pagination for large requests.
#'
#' **Search Syntax:** The `search` parameter uses *Elasticsearch* query syntax.
#' Common patterns include:
#' - Simple term: `"pacemaker"`
#' - Field-specific: `"device.generic_name:pacemaker"`
#' - Multiple terms: `"device.generic_name:pacemaker AND event_type:malfunction"`
#' - Date range: `"date_received:[20200101 TO 20201231]"`
#' - Exact phrase: `"device.brand_name:\"Medtronic\""`
#'
#' **Building Queries in `maude_query()`:** `maude_query()` is designed to help
#' you build a valid search string without writing the full query yourself.
#' Use the common field arguments (e.g., `device_generic_name`, `event_type`)
#' to add structured filters, and pass any additional fields through `...`.
#' The `...` names should match openFDA searchable fields, documented at:
#' <https://open.fda.gov/apis/device/event/searchable-fields/>.
#'
#' **API Response Handling:** The openFDA API returns HTTP 404 for queries with
#' no results (rather than an empty array). Both functions handle this by
#' returning an empty tibble instead of throwing an error. A 404 is therefore a
#' legitimately empty stratum and is not retried.
#'
#' **Counting:** `maude_fda_api_call(count = )` uses the openFDA `count`
#' endpoint, which aggregates on the server and returns one `term`/`count` pair
#' per distinct value rather than the records themselves. A frequency table
#' that would otherwise take hundreds of paginated record requests takes one:
#'
#' ```r
#' maude_fda_api_call(
#'   search_query = "device.device_report_product_code:QZI",
#'   count = "product_problems.exact",
#'   limit = 999
#' )
#' ```
#'
#' Two limits are worth knowing. The endpoint returns at most 1000 terms and
#' offers no pagination cursor, so a field with a longer tail is silently
#' truncated -- check whether `nrow()` has hit the cap before treating the table
#' as complete. Without an `api_key` the usable ceiling is 999, and a `limit` of
#' 1000 is refused with HTTP 403 and the message "No api_key was supplied",
#' which is not what went wrong. And counts are of *mentions*, not reports: a
#' report coded with three problems contributes to three terms, so the column
#' does not sum to the number of matching reports.
#'
#' **Device Problem Terms:** the coded device problem terms are returned in the
#' record's top-level `product_problems` field. They are never returned inside
#' `device[]`, even though `device.device_problem_codes` is a valid field to
#' search on, which is why `device_problem` is read from one field and filtered
#' on another.
#'
#' **When to use `maude_fda_api_call()`:** Most users should use `maude_query()`.
#' The lower-level `maude_fda_api_call()` is useful when you need:
#' - Direct control over `skip` for custom pagination strategies
#' - Pre-constructed query strings with complex *Elasticsearch* syntax
#' - Integration into custom retry/error-handling logic
#' - Full control over `search_query`, `sort`, and other openFDA parameters
#'
#' @param search Character string specifying the search query or `NULL`. For
#'   `maude_query()`, this can be a simple term (e.g., `"pacemaker"`) or a
#'   field-specific query (e.g., `"device.generic_name:pacemaker"`). Use the
#'   other `maude_query()` arguments to add additional filters.
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
#'   filter on (`device.device_problem_codes`). Note the asymmetry: openFDA
#'   accepts this as a *search* field but never returns it, so the returned
#'   `device_problem` column is read from the record's top-level
#'   `product_problems` instead. Filtering and reading therefore name different
#'   fields for the same terms.
#'
#' @param patient_problem Optional character vector of patient problems to
#'   filter on (`patient.patient_problems`).
#'
#' @param ... Additional named search fields and values to include in the
#'   query. Names should match the openFDA searchable fields list.
#'
#' @param limit Integer specifying the maximum number of records to return.
#'   For `maude_query()` this is a cap on the whole call, not on each request:
#'   it defaults to 100, and anything above 1000 is paginated internally into
#'   requests of 999. Due to openFDA `skip` limits, `maude_query()` currently
#'   supports up to 26,000 records per call and refuses a larger `limit`. When
#'   the query matches more reports than `limit` returns, the result is
#'   truncated and a warning says how many matched. For `maude_fda_api_call()`,
#'   `limit` is the per-request maximum of 1000 per openFDA limits; when `count`
#'   is given it caps the number of terms instead, at 1000.
#'
#' @param date_start Optional start date for filtering by `date_received`.
#'   Prefer a `Date` object, such as `as.Date("2026-01-01")`. POSIXt,
#'   `"YYYYMMDD"`, and `"YYYY-MM-DD"` values are also accepted and converted to
#'   `Date`. Only used by `maude_query()`.
#'
#' @param date_end Optional end date for filtering by `date_received`. Prefer a
#'   `Date` object, such as `as.Date("2026-01-31")`. POSIXt, `"YYYYMMDD"`, and
#'   `"YYYY-MM-DD"` values are also accepted and converted to `Date`. Only used
#'   by `maude_query()`.
#'
#' @param skip Integer specifying the number of records to skip for pagination.
#'   Only used by `maude_fda_api_call()`. Combined with `limit`, allows fetching
#'   records in pages (e.g., skip=0 gets records 1-1000, skip=1000 gets
#'   1001-2000).
#'
#' @param sort Optional sort specification for the openFDA API (e.g.,
#'   `"date_received:desc"`). Only used by `maude_fda_api_call()`.
#'
#' @param count Optional openFDA field to tabulate on instead of returning
#'   records, such as `"product_problems.exact"` or
#'   `"device.manufacturer_d_name.exact"`. Only used by
#'   `maude_fda_api_call()`. The `.exact` suffix counts whole field values;
#'   without it openFDA counts individual tokens, so "Cardiac Tamponade" is
#'   split into "cardiac" and "tamponade". See **Counting** in the details.
#'
#' @param api_key Optional character string containing your openFDA API key.
#'   Not required, but recommended for heavy usage to avoid rate limiting.
#'   Register at: <https://open.fda.gov/apis/authentication/>
#'
#' @param descriptions_from_web Logical. If `TRUE`, after the API call
#'   `maude_query()` backfills missing `event_description` values in two
#'   passes: first from FDA's bulk MAUDE narrative archives
#'   (`foitext{YYYY}.zip`, `foitextadd.zip`, `foitextchange.zip`,
#'   `foitextthru1995.zip`), then by scraping the FDA MAUDE detail page for
#'   any rows still missing. Disabled by default because the web pass adds
#'   one HTTP request per remaining missing report. The bulk archives are
#'   downloaded once and cached locally. Requires the `rvest` and `xml2`
#'   packages for the web pass.
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
#'     \item{patient_problem}{Reported problems affecting the patient, from
#'       `patient[].patient_problems`, de-duplicated and `"; "`-delimited}
#'     \item{device_problem}{Reported problems with the device, from the
#'       top-level `product_problems`, de-duplicated and `"; "`-delimited}
#'   }
#'
#'   Returns an empty *tibble* if no results are found.
#'
#'   The result carries a `"total"` attribute giving how many reports the query
#'   matched, which is larger than `nrow()` whenever `limit` truncated it.
#'
#'   With `count`, `maude_fda_api_call()` returns a two-column *tibble* of
#'   `term` and `count` instead, at most 1000 rows.
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
#' pacemaker_events <- maude_query("pacemaker", limit = 10)
#'
#' # Search by device generic name
#' results <- maude_query(device_generic_name = "defibrillator", limit = 50)
#'
#' # Filter by received-date range using Date objects
#' results <- maude_query(
#'   search = "pacemaker",
#'   date_start = as.Date("2026-01-01"),
#'   date_end = as.Date("2026-01-31"),
#'   limit = 50
#' )
#'
#' # Add an extra searchable field via ...
#' results <- maude_query(
#'   device_generic_name = "infusion pump",
#'   device.product_code = "LVP",
#'   limit = 50
#' )
#'
#' # Frequency table of device problem terms in one request, rather than
#' # paginating through every matching record
#' problems <- maude_fda_api_call(
#'   search_query = "device.device_report_product_code:QZI",
#'   count = "product_problems.exact",
#'   limit = 999
#' )
#' }
#'
#' @name maude_query
NULL

#' @rdname maude_query
#' @export
maude_query <- function(
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
    descriptions_from_web = FALSE,
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
      if (is.na(x)) {
        stop("'", arg_name, "' must not be NA")
      }
      return(x)
    }

    if (inherits(x, "POSIXt")) {
      parsed <- as.Date(x)
      if (is.na(parsed)) {
        stop("'", arg_name, "' must not be NA")
      }
      return(parsed)
    }

    if (is.character(x)) {
      if (grepl("^\\d{8}$", x)) {
        parsed <- as.Date(x, format = "%Y%m%d")
      } else {
        parsed <- as.Date(x)
      }
      if (is.na(parsed)) {
        stop(
          "'",
          arg_name,
          "' must be a Date, POSIXt, or YYYYMMDD/YYYY-MM-DD string"
        )
      }
      return(parsed)
    }

    stop("'", arg_name, "' must be a Date, POSIXt, or YYYYMMDD/YYYY-MM-DD string")
  }

  format_openfda_date <- function(x) {
    format(x, "%Y%m%d")
  }

  build_field_terms <- function(field, values) {
    if (is.null(values)) {
      return(character(0))
    }

    values <- values[!is.na(values)]
    if (!length(values)) {
      return(character(0))
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

    paste0(field, ":(", paste(escaped, collapse = " OR "), ")")
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
  if (!is.logical(descriptions_from_web) ||
      length(descriptions_from_web) != 1 ||
      is.na(descriptions_from_web)) {
    stop("'descriptions_from_web' must be TRUE or FALSE")
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
    de <- if (is.null(date_end)) Sys.Date() else date_end
    ds <- if (inherits(ds, "Date")) format_openfda_date(ds) else ds
    de <- if (inherits(de, "Date")) format_openfda_date(de) else de
    clauses <- c(clauses, paste0("date_received:[", ds, " TO ", de, "]"))
  }
  if (length(clauses) == 0) {
    stop(
      "Provide 'search', at least one field filter, or a date range ",
      "to build a query"
    )
  }
  query <- paste(clauses, collapse = " AND ")

  # Paginate if limit > 1000 (openFDA max per request).
  max_per_request <- 999
  sort <- "date_received:desc"
  total <- NA_integer_
  if (limit <= max_per_request) {
    result <- maude_fda_api_call(
      search_query = query,
      limit = limit,
      skip = 0,
      api_key = api_key,
      sort = sort,
      verbose = verbose
    )
    total <- attr(result, "total")
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
      if (i == 1L) total <- attr(batch, "total")
      all_results[[i]] <- batch
      if (nrow(batch) < batch_limit) break

      Sys.sleep(0.26) # Keep below openFDA's ~4 requests/second guidance
    }

    result <- dplyr::bind_rows(all_results)
  }

  # `limit` is a cap on how many records this call returns, not a statement
  # about how many exist. A query matching 41,000 reports and a query matching
  # exactly `limit` of them otherwise come back looking identical, so say so
  # rather than handing back a silently truncated frame.
  if (is.null(total)) total <- NA_integer_
  if (length(total) == 1L && !is.na(total) && total > nrow(result)) {
    warning(
      "Query matched ", total, " report(s) but 'limit' returned ", nrow(result),
      ". Raise 'limit', or split the query with 'date_start'/'date_end' ",
      "to retrieve the rest.",
      immediate. = TRUE,
      call. = FALSE
    )
  }

  # Provide a short message about the outcome.
  if (verbose) {
    if (nrow(result) == 0) {
      message("No adverse event reports found for query: ", query)
    } else {
      message("Retrieved ", nrow(result), " adverse event report(s)")
    }
  }

  if (descriptions_from_web && nrow(result) > 0) {
    result <- get_maude_file_descriptions(result, quiet = !verbose)
    result <- get_maude_web_descriptions(result, quiet = !verbose)
  }

  if ("date_received" %in% names(result)) {
    date_received <- as.character(result$date_received)
    date_received <- trimws(date_received)
    date_received[!nzchar(date_received)] <- NA_character_

    ymd_compact <- !is.na(date_received) & grepl("^\\d{8}$", date_received)
    if (any(ymd_compact)) {
      date_received[ymd_compact] <- as.character(
        as.Date(date_received[ymd_compact], format = "%Y%m%d")
      )
    }

    result$date_received <- suppressWarnings(as.Date(date_received))
  }

  attr(result, "total") <- total
  result
}

#' @rdname maude_query
#' @export
maude_fda_api_call <- function(
  search_query,
  limit = 100,
  skip = 0,
  api_key = NULL,
  sort = NULL,
  count = NULL,
  max_retries = 3,
  verbose = FALSE
) {

  if (!is.null(count) &&
      (!is.character(count) || length(count) != 1 || !nzchar(count))) {
    stop("'count' must be NULL or a single openFDA field name", call. = FALSE)
  }

  # This is or query parameters for "direct" calling the API, not user friendly
  # Format =  "?search=pacemaker&limit=100&skip=0" (gets appended to URL)
  # The `httr::GET()` function accepts the query argument in form of named list
  #
  # Why?
  #   1. httr handles URL encoding of special characters automatically
  #   2. NULL values are automatically omitted from the query string
  #   3. The resulting URL is properly formatted without manual "&" joining
  params <- list(search = search_query, limit = limit, skip = skip)

  # The count endpoint aggregates server-side and returns { term, count } pairs
  # instead of records, which turns a frequency table over tens of thousands of
  # reports into a single request. `skip` is meaningless there, and openFDA caps
  # the response at 1000 terms with no pagination cursor.
  if (!is.null(count)) {
    params$count <- count
    params$skip <- NULL
  }

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
    # openFDA answers an anonymous caller with "No api_key was supplied"
    # whenever it declines the request, whether because the per-minute rate was
    # exceeded or because `limit` was above 999. Neither is what the message
    # says, and both are fixable without registering for a key.
    if (status == 403 && is.null(api_key)) {
      err <- paste0(
        err,
        " (openFDA reports this for any refused anonymous request: `limit` ",
        "above 999, or the per-minute rate exceeded. Lower 'limit', wait, or ",
        "supply 'api_key'.)"
      )
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

  if (!is.null(count)) {
    return(tibble::tibble(
      term = vapply(
        results,
        function(x) as.character(purrr::pluck(x, "term", .default = NA_character_)),
        character(1)
      ),
      count = vapply(
        results,
        function(x) as.integer(purrr::pluck(x, "count", .default = NA_integer_)),
        integer(1)
      )
    ))
  }

  # Transform each API result record into a standardized tibble row.
  # purrr::map_dfr iterates over results and row-binds the individual tibbles.
  # Parsed results are very nested
  out <- purrr::map_dfr(results, flatten_maude_record)

  # `meta$results$total` is how many reports the query actually matched, as
  # against the `limit` that were asked for. Carrying it lets the caller find
  # out that a result was truncated, which is otherwise invisible: a query that
  # matched 41,000 reports and a query that matched exactly `limit` of them
  # return the same thing.
  attr(out, "total") <- as.integer(
    purrr::pluck(parsed, "meta", "results", "total", .default = NA_integer_)
  )
  out
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

#' Flatten one openFDA MAUDE record into the correct output schema
#'
#' @description Internal helper used by `maude_fda_api_call()` to convert a
#'   single parsed MAUDE API record into the standard tibble row returned by
#'   `maude_query()`. This centralizes the logic for preserving event narrative
#'   text and normalizing variable nested shapes from the API response.
#'
#' @param rec A single parsed MAUDE record from the openFDA API response.
#'
#' @return A one-row tibble in the `maude_query()` output format.
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

  # openFDA emits the coded-term arrays twice for most records, so
  # `product_problems` arrives as c("Material Deformation", "Material
  # Deformation") on a single-device report. The repetition is an artefact of
  # how the endpoint joins its tables, not a count of anything, and pasted
  # through it reads as two separate problems. Narrative blocks are left alone:
  # each carries its own `mdr_text_key` and a supplement may legitimately repeat
  # the text of the report it amends.
  collapse_field <- function(x, field, delimiter = "; ", unique_values = TRUE) {
    vals <- collect_maude_field_values(x, field)
    if (!length(vals)) {
      return(NA_character_)
    }

    if (unique_values) {
      vals <- unique(vals)
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
      delimiter = " | ",
      unique_values = FALSE
    ),
    patient_problem = collapse_field(
      purrr::pluck(rec, "patient", .default = list()),
      "patient_problems"
    ),
    # The device problem terms live at the top level of the record, in
    # `product_problems`. They are not returned inside `device[]` at all, even
    # though `device.device_problem_codes` is a valid field to *search* on; that
    # asymmetry is why this was previously read from the device list and came
    # back NA for every record ever returned.
    device_problem = collapse_field(
      rec["product_problems"],
      "product_problems"
    )
  )
}

# FDA MAUDE web fallback ----

#' Fill missing MAUDE descriptions from FDA detail pages
#'
#' @description Internal helper used by `maude_query()` when
#'   `descriptions_from_web = TRUE`. It looks up missing
#'   `event_description` values by `mdr_report_key` on the FDA MAUDE detail
#'   pages. Existing API-provided descriptions are never overwritten.
#'
#' @param events A data frame returned by `maude_query()`. Must contain
#'   `mdr_report_key` and `event_description`.
#' @param pause_seconds Delay between FDA web page requests. This keeps the
#'   fallback gentle because it makes one request per missing report.
#' @param quiet Logical. If `FALSE`, prints progress messages.
#'
#' @return The input data frame with missing `event_description` values filled
#'   where FDA web pages provide narrative text.
#'
#' @author Reese Fuller
#' @keywords internal
#' @noRd
get_maude_web_descriptions <- function(
  events,
  pause_seconds = 0.25,
  quiet = FALSE
) {
  # The fallback only knows how to update the standard maude_query() output.
  if (!is.data.frame(events)) {
    stop("'events' must be a data.frame or tibble")
  }
  for (col in c("mdr_report_key", "event_description")) {
    if (!col %in% names(events)) {
      stop("Column not found in events: ", col)
    }
  }

  # The pause is user-adjustable, but it should never be negative.
  if (!is.numeric(pause_seconds) || length(pause_seconds) != 1 ||
      is.na(pause_seconds) || pause_seconds < 0) {
    stop("'pause_seconds' must be a single non-negative number")
  }

  # Treat NA, empty strings, and whitespace-only strings as missing.
  is_blank <- function(x) {
    is.na(x) | !nzchar(trimws(as.character(x)))
  }

  # Only request pages for rows where openFDA did not return narrative text.
  needs_fill <- is_blank(events$event_description)
  keys <- unique(as.character(events$mdr_report_key[needs_fill]))
  keys <- keys[!is_blank(keys)]

  # If there is nothing to fill, return early without loading web packages.
  if (length(keys) == 0) {
    if (!quiet) message("No missing descriptions to fill from MAUDE web pages.")
    return(events)
  }

  # These packages are only needed for the optional web fallback.
  if (!requireNamespace("xml2", quietly = TRUE) ||
      !requireNamespace("rvest", quietly = TRUE)) {
    stop(
      "Packages 'xml2' and 'rvest' are required when ",
      "'descriptions_from_web = TRUE'.",
      call. = FALSE
    )
  }

  # These labels are the only page-layout assumptions in the web fallback.
  # If FDA changes the MAUDE detail page, update these labels first.
  narrative_labels <- c(
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
    "MDR Report Key",
    "Search Alerts/Recalls"
  )

  # Escape labels before putting them into one regular expression.
  escape_regex <- function(x) {
    gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x, perl = TRUE)
  }

  # Use word boundaries to avoid matching these labels inside narrative text.
  label_pattern <- function(labels) {
    paste0("\\b(?:", paste(escape_regex(labels), collapse = "|"), ")\\b")
  }

  # Normalize page text once so section matching is predictable.
  clean_text <- function(x) {
    if (is.null(x) || length(x) == 0 || is.na(x[[1]])) {
      return(NA_character_)
    }

    x <- iconv(as.character(x[[1]]), from = "", to = "UTF-8", sub = "")
    x <- gsub("[[:space:]]+", " ", x)
    x <- trimws(x)
    if (!nzchar(x)) {
      return(NA_character_)
    }
    x
  }

  section_pattern <- label_pattern(narrative_labels)
  stop_pattern <- label_pattern(stop_labels)

  # The MDR report key is the same identifier used by the MAUDE detail page.
  url_template <- paste0(
    "https://www.accessdata.fda.gov/scripts/cdrh/cfdocs/cfMAUDE/detail.cfm",
    "?mdrfoi__id=%s"
  )

  if (!quiet) {
    message("Fetching MAUDE web narratives for ", length(keys), " report(s)...")
  }

  # Build a named lookup so duplicate keys in the result are filled together.
  descriptions <- stats::setNames(
    vapply(seq_along(keys), function(i) {
      # Pause between requests to avoid hammering FDA pages.
      if (i > 1L && pause_seconds > 0) {
        Sys.sleep(pause_seconds)
      }

      # Failed pages are not fatal; they simply remain unfilled.
      page <- tryCatch(
        xml2::read_html(sprintf(
          url_template,
          utils::URLencode(keys[[i]], reserved = TRUE)
        )),
        error = function(e) NULL
      )
      if (is.null(page)) {
        return(NA_character_)
      }

      # Work with visible body text instead of brittle HTML node paths.
      page_text <- tryCatch(
        rvest::html_text2(rvest::html_element(page, "body")),
        error = function(e) NA_character_
      )
      page_text <- clean_text(page_text)
      if (is.na(page_text)) {
        return(NA_character_)
      }

      # Find each narrative heading on the page.
      section_starts <- gregexpr(
        section_pattern,
        page_text,
        ignore.case = TRUE,
        perl = TRUE
      )[[1]]
      if (section_starts[[1]] == -1L) {
        return(NA_character_)
      }

      # The narrative text starts after each heading.
      section_ends <- section_starts + attr(section_starts, "match.length") - 1L
      pieces <- vapply(seq_along(section_starts), function(j) {
        start <- section_ends[[j]] + 1L

        # A narrative section ends at the next narrative heading, or the page end.
        end <- if (j < length(section_starts)) {
          section_starts[[j + 1L]] - 1L
        } else {
          nchar(page_text)
        }

        chunk <- substr(page_text, start, end)

        # Stop labels keep the final narrative from swallowing the rest of page.
        stop_start <- regexpr(
          stop_pattern,
          chunk,
          ignore.case = TRUE,
          perl = TRUE
        )[[1]]
        if (stop_start > 0) {
          chunk <- substr(chunk, 1L, stop_start - 1L)
        }
        clean_text(chunk)
      }, character(1))

      # De-duplicate repeated sections and join them like the API text blocks.
      pieces <- unique(pieces[!is_blank(pieces)])
      if (!length(pieces)) {
        return(NA_character_)
      }
      paste(pieces, collapse = " | ")
    }, character(1)),
    keys
  )

  # Fill only rows that were blank before the web fallback.
  out <- events
  row_keys <- as.character(out$mdr_report_key)
  matched <- match(row_keys, names(descriptions))
  fill_rows <- needs_fill & !is.na(matched) & !is_blank(descriptions[matched])
  out$event_description[fill_rows] <- unname(descriptions[matched[fill_rows]])

  if (!quiet) {
    message("Filled ", sum(fill_rows), " missing event description(s).")
  }

  out
}

# FDA MAUDE bulk text-archive fallback ----

#' Fill missing MAUDE descriptions from FDA bulk narrative archives
#'
#' @description Internal helper used by `maude_query()` when
#'   `descriptions_from_web = TRUE`. It looks up missing `event_description`
#'   values by `mdr_report_key` in FDA's bulk pipe-delimited MAUDE narrative
#'   archives (`foitext{YYYY}.zip`, `foitextadd.zip`, `foitextchange.zip`,
#'   `foitextthru1995.zip`). Existing API-provided descriptions are never
#'   overwritten. Downloaded archives are cached in `cache_dir` so subsequent
#'   calls reuse them.
#'
#' @param events A data frame returned by `maude_query()`. Must contain
#'   `mdr_report_key`, `event_description`, and `date_received`.
#' @param cache_dir Directory used to store downloaded MAUDE text archives.
#' @param quiet Logical. If `FALSE`, prints progress messages.
#'
#' @return The input data frame with missing `event_description` values filled
#'   where FDA bulk narrative archives provide narrative text.
#'
#' @author Reese Fuller
#' @keywords internal
#' @noRd
get_maude_file_descriptions <- function(
  events,
  cache_dir = file.path(tempdir(), "maude_text_cache"),
  quiet = FALSE
) {
  if (!is.data.frame(events)) {
    stop("'events' must be a data.frame or tibble")
  }
  for (col in c("mdr_report_key", "event_description", "date_received")) {
    if (!col %in% names(events)) {
      stop("Column not found in events: ", col)
    }
  }

  # Treat NA, empty, and whitespace-only as missing (mirrors web fallback).
  is_blank <- function(x) {
    is.na(x) | !nzchar(trimws(as.character(x)))
  }

  needs_fill <- is_blank(events$event_description)
  keys_needed <- unique(as.character(events$mdr_report_key[needs_fill]))
  keys_needed <- keys_needed[!is_blank(keys_needed)]

  # Skip the (potentially large) download work when nothing needs filling.
  if (length(keys_needed) == 0) {
    if (!quiet) {
      message("No missing descriptions to fill from MAUDE text archives.")
    }
    return(events)
  }

  if (!requireNamespace("readr", quietly = TRUE)) {
    stop(
      "Package 'readr' is required for the MAUDE text-archive fallback.",
      call. = FALSE
    )
  }

  # FDA partitions narratives by year, so we only download archives covering
  # the years where rows are actually missing descriptions.
  date_received <- events$date_received[needs_fill]
  # openFDA returns "YYYYMMDD" strings; maude_query() may have already coerced
  # to Date. Try the compact format first, then fall back to ISO.
  if (!inherits(date_received, "Date")) {
    raw <- trimws(as.character(date_received))
    raw[!nzchar(raw)] <- NA_character_
    parsed <- suppressWarnings(as.Date(raw, format = "%Y%m%d"))
    bad <- is.na(parsed) & !is.na(raw)
    if (any(bad)) {
      parsed[bad] <- suppressWarnings(as.Date(raw[bad]))
    }
    date_received <- parsed
  }

  yrs <- unique(format(stats::na.omit(date_received), "%Y"))
  yrs <- yrs[!is.na(yrs) & nzchar(yrs)]
  if (length(yrs) == 0) {
    if (!quiet) {
      message("Could not determine year(s) from 'date_received'; skipping.")
    }
    return(events)
  }

  # FDA archive naming: pre-1996 lumped into one file, prior years get their
  # own annual zip, and the current year is split across rolling add/change
  # files since it has not been finalized yet.
  current_year <- format(Sys.Date(), "%Y")
  archive_files_for_year <- function(year) {
    if (year <= "1995") return("foitextthru1995.zip")
    if (year < current_year) return(sprintf("foitext%s.zip", year))
    if (year == current_year) return(c("foitextadd.zip", "foitextchange.zip"))
    character(0)
  }

  files_needed <- unique(unlist(lapply(yrs, archive_files_for_year)))
  if (length(files_needed) == 0) {
    return(events)
  }

  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }

  archive_url <- function(file_name) {
    sprintf("https://www.accessdata.fda.gov/MAUDE/ftparea/%s", file_name)
  }

  # Cache zip + extracted dir per archive so repeat calls in the same R
  # session (or with a persistent cache_dir) skip the download/unzip.
  fetch_archive <- function(file_name) {
    zip_path <- file.path(cache_dir, file_name)
    out_dir <- file.path(cache_dir, tools::file_path_sans_ext(file_name))

    if (!file.exists(zip_path)) {
      if (!quiet) message("Downloading MAUDE text file: ", file_name, " ...")
      download_ok <- tryCatch(
        {
          utils::download.file(
            archive_url(file_name),
            destfile = zip_path,
            mode = "wb",
            quiet = quiet
          )
          TRUE
        },
        error = function(e) {
          if (!quiet) {
            message(
              "Failed to download MAUDE text file: ",
              file_name,
              " (",
              conditionMessage(e),
              "); skipping."
            )
          }
          FALSE
        }
      )
      # A failed download is non-fatal; fall through to the web fallback.
      if (!download_ok || !file.exists(zip_path)) return(NULL)
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
            message(
              "Failed to unzip MAUDE text file: ",
              file_name,
              " (",
              conditionMessage(e),
              "); skipping."
            )
          }
          FALSE
        }
      )
      if (!unzip_ok) return(NULL)
    }

    # Each FDA archive contains a single narrative .txt; take the first
    # match defensively in case the archive layout ever changes.
    txts <- list.files(
      out_dir,
      pattern = "\\.txt$",
      full.names = TRUE,
      ignore.case = TRUE
    )
    if (length(txts) == 0) return(NULL)
    txts[[1]]
  }

  read_archive <- function(path) {
    # Pipe-delimited, no header
    # Quoting insite narrative could break (so disable)
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
    # Column order is documented by FDA
    # Only the first 6 are needed.
    names(dat)[1:6] <- c(
      "mdr_report_key",
      "mdr_text_key",
      "text_type_code",
      "patient_sequence_number",
      "date_report",
      "text"
    )
    dat$mdr_report_key <- as.character(dat$mdr_report_key)
    # Narratives sometimes contain non-UTF-8 bytes -> strip
    dat$text <- iconv(
      as.character(dat$text),
      from = "",
      to = "UTF-8",
      sub = ""
    )
    dat[, c("mdr_report_key", "text")]
  }
 
  # Filter each archive down to the keys we actually need 
  # Avoids needing holding full dataset in memory 
  text_rows <- list()
  for (f in files_needed) {
    txt <- fetch_archive(f)
    if (is.null(txt)) next
    dat <- tryCatch(
      read_archive(txt),
      error = function(e) {
        if (!quiet) {
          message(
            "Failed to read MAUDE text file: ",
            f,
            " (",
            conditionMessage(e),
            "); skipping."
          )
        }
        NULL
      }
    )
    if (is.null(dat) || nrow(dat) == 0) next
    text_rows[[length(text_rows) + 1L]] <- dat[
      dat$mdr_report_key %in% keys_needed, ,
      drop = FALSE
    ]
  }

  if (length(text_rows) == 0) {
    if (!quiet) {
      message("No matching text rows found in MAUDE text archives.")
    }
    return(events)
  }

  text_rows <- do.call(rbind, text_rows)
  text_rows <- text_rows[!is_blank(text_rows$text), , drop = FALSE]
  if (nrow(text_rows) == 0) {
    if (!quiet) {
      message("No matching text rows found in MAUDE text archives.")
    }
    return(events)
  }

  # One report can have multiple narrative rows (initial + supplements);
  # join them with " | " to match the web fallback's output format.
  descriptions <- tapply(
    text_rows$text,
    text_rows$mdr_report_key,
    function(x) paste(unique(x), collapse = " | ")
  )

  # Only fill rows that were blank to begin with
  # API first if its available
  out <- events
  row_keys <- as.character(out$mdr_report_key)
  matched <- match(row_keys, names(descriptions))
  fill_rows <- needs_fill & !is.na(matched) & !is_blank(descriptions[matched])
  out$event_description[fill_rows] <- unname(descriptions[matched[fill_rows]])

  if (!quiet) {
    message("Filled ", sum(fill_rows), " missing event description(s).")
  }

  out
}

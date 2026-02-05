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
#'   "E"). Case-sensitive. Only "E" and "F" are supported currently.
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
load_maude_codes <- function(annex) {
  # Validate annex input
  valid_annexes <- c("E", "F")
  if (!(annex %in% valid_annexes)) {
    stop("Invalid annex specified. Valid options are: ",
         paste(valid_annexes, collapse = ", "))
  }

  # Load appropriate dataset
  annex_name <- paste0("annex_", tolower(annex))
  dat <- .maude_codes[[annex_name]]

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
#' **Default Sort:** When no date range is provided, `query_maude()` requests
#' results in reverse chronological order by `date_received`.
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
#'   automatically paginated. For `maude_fda_api_call()`, maximum is 1000 per the
#'   openFDA API limits.
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
    api_key = NULL
) {
  # Basic input validation.
  if (!is.null(search) &&
      (!is.character(search) || length(search) != 1 || nchar(search) == 0)) {
    stop("'search' must be NULL or a non-empty character string")
  }

  if (!is.numeric(limit) || length(limit) != 1 || limit < 1) {
    stop("'limit' must be a positive integer")
  }
  limit <- as.integer(limit)

  if (!is.null(date_start)) {
    if (length(date_start) != 1) {
      stop("'date_start' must be a single date value")
    }
    if (inherits(date_start, "Date")) {
      date_start <- format(date_start, "%Y%m%d")
    } else if (inherits(date_start, "POSIXt")) {
      date_start <- format(as.Date(date_start), "%Y%m%d")
    } else if (is.character(date_start)) {
      if (!grepl("^\\d{8}$", date_start)) {
        parsed <- as.Date(date_start)
        if (is.na(parsed)) {
          stop("'date_start' must be a Date, POSIXt, or YYYYMMDD/YYYY-MM-DD string")
        }
        date_start <- format(parsed, "%Y%m%d")
      }
    } else {
      stop("'date_start' must be a Date, POSIXt, or YYYYMMDD/YYYY-MM-DD string")
    }
  }

  if (!is.null(date_end)) {
    if (length(date_end) != 1) {
      stop("'date_end' must be a single date value")
    }
    if (inherits(date_end, "Date")) {
      date_end <- format(date_end, "%Y%m%d")
    } else if (inherits(date_end, "POSIXt")) {
      date_end <- format(as.Date(date_end), "%Y%m%d")
    } else if (is.character(date_end)) {
      if (!grepl("^\\d{8}$", date_end)) {
        parsed <- as.Date(date_end)
        if (is.na(parsed)) {
          stop("'date_end' must be a Date, POSIXt, or YYYYMMDD/YYYY-MM-DD string")
        }
        date_end <- format(parsed, "%Y%m%d")
      }
    } else {
      stop("'date_end' must be a Date, POSIXt, or YYYYMMDD/YYYY-MM-DD string")
    }
  }

  if (!is.null(api_key) && (!is.character(api_key) || length(api_key) != 1)) {
    stop("'api_key' must be NULL or a single character string")
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
    values <- field_map[[field]]
    if (is.null(values)) next
    values <- values[!is.na(values)]
    if (length(values) == 0) next
    if (!is.character(values)) {
      stop("'", field, "' must be a character vector")
    }
    values <- ifelse(grepl("\\s", values), paste0("\"", values, "\""), values)
    if (length(values) == 1) {
      terms <- c(terms, paste0(field, ":", values))
    } else {
      terms <- c(terms, paste0(field, ":(", paste(values, collapse = "+OR+"), ")"))
    }
  }

  for (field in names(extra_terms)) {
    values <- extra_terms[[field]]
    if (is.null(values)) next
    values <- values[!is.na(values)]
    if (length(values) == 0) next
    if (!is.character(values)) {
      stop("'", field, "' must be a character vector")
    }
    values <- ifelse(grepl("\\s", values), paste0("\"", values, "\""), values)
    if (length(values) == 1) {
      terms <- c(terms, paste0(field, ":", values))
    } else {
      terms <- c(terms, paste0(field, ":(", paste(values, collapse = "+OR+"), ")"))
    }
  }

  terms <- terms[!is.na(terms)]
  if (length(terms) == 0) {
    stop("Provide 'search' or at least one field filter to build a query")
  }

  # Assemble the final query string, including an optional date range.
  query <- paste(terms, collapse = "+AND+")
  if (!is.null(date_start) || !is.null(date_end)) {
    ds <- date_start %||% "19920101"
    de <- date_end %||% format(Sys.Date(), "%Y%m%d")
    query <- paste0(query, "+AND+date_received:[", ds, "+TO+", de, "]")
  }

  # Paginate if limit > 1000 (openFDA max per request).
  max_per_request <- 1000
  sort <- if (is.null(date_start) && is.null(date_end)) {
    "date_received:desc"
  } else {
    NULL
  }
  if (limit <= max_per_request) {
    result <- maude_fda_api_call(
      search_query = query,
      limit = limit,
      skip = 0,
      api_key = api_key,
      sort = sort
    )
  } else {
    all_results <- list()
    n_batches <- ceiling(limit / max_per_request)

    for (i in seq_len(n_batches)) {
      skip <- (i - 1) * max_per_request
      batch_limit <- min(max_per_request, limit - skip)
      sort <- NULL
      if (is.null(date_start) && is.null(date_end)) {
        sort <- "date_received:desc"
      }

      message(
        "Retrieving batch ", i, "/", n_batches,
        " (records ", skip + 1, "-", skip + batch_limit, ")..."
      )

      batch <- maude_fda_api_call(
        search_query = query,
        limit = batch_limit,
        skip = skip,
        api_key = api_key,
        sort = sort
      )
      if (nrow(batch) == 0) break
      all_results[[i]] <- batch
      if (nrow(batch) < batch_limit) break

      Sys.sleep(0.25) # Rate limiting
    }

    result <- dplyr::bind_rows(all_results)
  }

  # Provide a short message about the outcome.
  if (nrow(result) == 0) {
    message("No adverse event reports found for query: ", query)
  } else {
    message("Retrieved ", nrow(result), " adverse event report(s)")
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
  sort = NULL
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
  resp <- httr::GET("https://api.fda.gov/device/event.json", query = params)

  # Handle HTTP errors from the API response.
  # The openFDA API returns 404 when no results match the query 
  # We should return an empty tibble in that case as well
  # Other error codes probably exist as well
  if (httr::http_error(resp)) {
    if (httr::status_code(resp) == 404) {
      return(tibble::tibble())
    }
    stop("openFDA API request failed with status ", httr::status_code(resp))
  }

  # Parse JSON response and extract the results array.
  # httr::content with as="parsed" uses jsonlite to convert JSON to R lists.
  # The openFDA response structure is: { "meta": {...}, "results": [...] }
  # We only need the results array; if missing/NULL, default to empty list.
  results <- httr::content(resp, as = "parsed")$results %||% list()
  if (length(results) == 0) {
    return(tibble::tibble())
  }

  # Transform each API result record into a standardized tibble row.
  # purrr::map_dfr iterates over results and row-binds the individual tibbles.
  # Parsed results are very nested
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
      vals <- purrr::map_chr(
        items,
        ~ {
          x <- purrr::pluck(.x, field, .default = NULL)
          if (is.null(x)) NA_character_ else paste(unlist(x), collapse = "; ")
        }
      )
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
      report_number = purrr::pluck(
        rec,
        "report_number",
        .default = NA_character_
      ),
      event_type = purrr::pluck(rec, "event_type", .default = NA_character_),
      date_received = purrr::pluck(
        rec,
        "date_received",
        .default = NA_character_
      ),
      device_generic_name = purrr::pluck(
        device,
        "generic_name",
        .default = NA_character_
      ),
      device_brand_name = purrr::pluck(
        device,
        "brand_name",
        .default = NA_character_
      ),
      manufacturer_name = purrr::pluck(
        device,
        "manufacturer_d_name",
        .default = NA_character_
      ),
      event_description = if (event_desc == "") NA_character_ else event_desc,
      patient_problem = collapse_field(
        purrr::pluck(rec, "patient", .default = list()),
        "patient_problems"
      ),
      device_problem = collapse_field(
        purrr::pluck(rec, "device", .default = list()),
        "device_problem_codes"
      )
    )
  })
}


# MAUDE Narrative Evaluation with LLMs ----

# Need to create a function that can handle the LLM interaction for the
# narrative text. Need the function to limit the amount of data that is
# coming into the LLM to limit token use.

# Review how to use an LLM API with R as seen in https://ellmer.tidyverse.org/articles/structured-data.html

# I want to create a function in `maude.R` that identifies if a certain event happened or not in the descriptive text or narrative text of an MAUDE MDR event report. This function would work in a vectorized manner, such that it uses individual values for its evaluation, which in turn can be mapped back into a dataframe.  We have to specify which type of dataset we're wanting to look at (e.g. clinical problems vs. device problems, etc). 

# For example, a MAUDE dataset would contain a "patient problem" column, which would map onto clinical problems seen in the Annex E dataset (e.g. `dat <- load_maude_codes(annex = "E")`. There would also be a column called "event text" in the MAUDE dataset that would have the narrative text of that event. 

# I want the function to call the user's requested LLM model and provide them with a personal API key. The patient problem would have a definition in the Annex E dataset.  That definition would help to guide the LLM. The LLM would need to be prompted to evaluate the narrative text and return a structured event from it. There could be more than 1 event possible, as there are more than 1 possible patient problems that could be used to label the event. I want the LLM to then return structured data on if that event type happened or not.

#' @param event_type A `character` input choosing between *impact* or *clinical* as the type of health event that occurred. By selecting this the function will use the problem codes and definitions of the appropriate annex.
#' @param problem_code A `character` string that is the problem code, or vector of problem codes, that represents the health effects that occurred. The codes are selected from Annex E and Annex F based on the type
#' @param event_text Text description of the event as a `character` string. 
#' 
#' @export
evaluate_maude_health_event <- function(
  event_type = c("impact", "clinical"),
  problem_code,
  event_text,
  model_provider,
  model_version,
  api_key
) {

  # This function will generally be used with the API call from the MAUDE dataset
  # This data table will have a column for a problem_code and an event description
  # This function will take the problem code and match it to hte definition
  # Then, it will ask an LLM to look at the event text to see if that event occurred
  # The LLM model should return structured text of if the event(s) occurred
  # Would use `{ellmer}` to help organized structured chat return
  # LLM should be protected from prompt injection. Will need to prompt it from within the function, and not externally, to avoid issues
  # Should also display the LLM prompt information so the user knows what is happening (and document this appropriately)

}
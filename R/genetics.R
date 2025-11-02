# Genetic Variant Database Query Functions

#' Query Genetic Variant Databases by Phenotype
#'
#' Query online genetic variant databases (ClinVar and others) to retrieve
#' gene-disease associations based on a phenotype or clinical condition.
#'
#' @param phenotype Character string specifying the phenotype or disease
#'   condition to search for (e.g., "atrial fibrillation", "hypertrophic
#'   cardiomyopathy").
#' 
#' @param database Character string specifying which database to query.
#'   Currently supports "clinvar" (default). Additional databases may be
#'   added in future versions.
#' 
#' @param api_key Optional character string containing your NCBI API key.
#'   Providing an API key increases the rate limit from 3 to 10 requests
#'   per second. Get a key at: https://www.ncbi.nlm.nih.gov/account/
#' 
#' @param max_results Integer specifying maximum number of results to return.
#'   Default is 100. Maximum allowed is 500.
#'
#' @return A tibble with the following columns:
#'   \describe{
#'     \item{gene_symbol}{Gene symbol (e.g., "TTN")}
#'     \item{variant_id}{Database-specific variant identifier}
#'     \item{variant_name}{Human-readable variant name (HGVS notation when available)}
#'     \item{chromosome}{Chromosome location}
#'     \item{position}{Genomic position}
#'     \item{clinical_significance}{Clinical interpretation (e.g., "Pathogenic", "Benign")}
#'     \item{review_status}{Level of expert review}
#'     \item{phenotypes}{Associated phenotypes/conditions}
#'     \item{molecular_consequence}{Effect on protein/transcript}
#'     \item{database}{Source database}
#'   }
#'
#' @details
#' The function queries the specified genetic variant database and returns
#' a standardized table of results. For ClinVar, it uses the NCBI E-utilities
#' API with automatic rate limiting to comply with NCBI's usage policies
#' (3 requests/second without API key, 10 requests/second with API key).
#'
#' The search is performed using disease/phenotype field matching and may
#' return variants for the exact condition as well as related conditions
#' (e.g., searching for "autism" may return "autism spectrum disorder").
#'
#' @examples
#' \dontrun{
#' # Basic query without API key
#' af_variants <- query_genetic_variants("atrial fibrillation")
#'
#' # Query with API key for faster rate limit
#' # This will error without an API key
#' af_variants <- query_genetic_variants(
#'   "atrial fibrillation",
#'   api_key = "your_api_key_here"
#' )
#'
#' # Limit results
#' af_variants <- query_genetic_variants(
#'   "atrial fibrillation",
#'   max_results = 50
#' )
#' }
#'
#' @export
query_genetic_variants <- function(phenotype,
                                   database = "clinvar",
                                   api_key = NULL,
                                   max_results = 100) {

  # Input validation
  if (!is.character(phenotype) || length(phenotype) != 1 || nchar(phenotype) == 0) {
    stop("'phenotype' must be a non-empty character string")
  }

  if (!is.character(database) || length(database) != 1) {
    stop("'database' must be a single character string")
  }

  if (!is.null(api_key) && (!is.character(api_key) || length(api_key) != 1)) {
    stop("'api_key' must be NULL or a single character string")
  }

  if (!is.numeric(max_results) || length(max_results) != 1 ||
      max_results < 1 || max_results > 500) {
    stop("'max_results' must be a single number between 1 and 500")
  }

  # Route to appropriate database function
  database <- tolower(database)

  result <- switch(
    database,
    "clinvar" = .query_clinvar(phenotype, api_key, max_results),
    stop("Unsupported database: '", database, "'. Currently only 'clinvar' is supported.")
  )

  return(result)
}


# Internal function to query ClinVar
#' @noRd
#' @keywords internal
.query_clinvar <- function(phenotype, api_key, max_results) {

  # Build search query - ClinVar searches across all fields by default
  # This provides flexible matching for phenotypes, genes, and conditions
  search_term <- phenotype

  # Step 1: Search for variant IDs
  variant_ids <- .clinvar_search(search_term, api_key, max_results)

  if (length(variant_ids) == 0) {
    message("No variants found for phenotype: ", phenotype)
    return(.empty_result_table())
  }

  message("Found ", length(variant_ids), " variants for '", phenotype, "'")

  # Step 2: Get detailed summaries for the variants
  # Process in batches to respect rate limits
  batch_size <- 200  # ESummary can handle multiple IDs
  n_batches <- ceiling(length(variant_ids) / batch_size)

  all_results <- list()

  for (i in seq_len(n_batches)) {
    start_idx <- (i - 1) * batch_size + 1
    end_idx <- min(i * batch_size, length(variant_ids))
    batch_ids <- variant_ids[start_idx:end_idx]

    # Rate limiting
    .rate_limit(api_key)

    batch_results <- .clinvar_summary(batch_ids, api_key)
    all_results[[i]] <- batch_results
  }

  # Combine all batches
  combined_results <- dplyr::bind_rows(all_results)

  return(combined_results)
}


# Internal function to perform ClinVar search (esearch)
.clinvar_search <- function(search_term, api_key, max_results) {

  base_url <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi"

  # Build query parameters
  query_params <- list(
    db = "clinvar",
    term = search_term,
    retmax = max_results,
    retmode = "json"
  )

  if (!is.null(api_key)) {
    query_params$api_key <- api_key
  }

  # Rate limiting
  .rate_limit(api_key)

  # Make request
  response <- httr::GET(base_url, query = query_params)

  # Check response
  if (httr::http_error(response)) {
    stop("ClinVar API request failed with status ", httr::status_code(response))
  }

  # Parse JSON response
  content <- httr::content(response, as = "parsed")

  # Extract variant IDs
  id_list <- content$esearchresult$idlist

  if (is.null(id_list) || length(id_list) == 0) {
    return(character(0))
  }

  return(unlist(id_list))
}


# Internal function to get variant summaries (esummary)
.clinvar_summary <- function(variant_ids, api_key) {

  base_url <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi"

  # Build query parameters
  query_params <- list(
    db = "clinvar",
    id = paste(variant_ids, collapse = ","),
    retmode = "json"
  )

  if (!is.null(api_key)) {
    query_params$api_key <- api_key
  }

  # Make request
  response <- httr::GET(base_url, query = query_params)

  # Check response
  if (httr::http_error(response)) {
    warning("ESummary request failed with status ", httr::status_code(response))
    return(.empty_result_table())
  }

  # Parse JSON response
  content <- httr::content(response, as = "parsed")

  # Extract and format results
  results <- .parse_clinvar_summary(content)

  return(results)
}


# Internal function to parse ClinVar ESummary JSON response
.parse_clinvar_summary <- function(content) {

  # Get the result list
  result_list <- content$result

  # Remove the 'uids' element
  result_list$uids <- NULL

  if (length(result_list) == 0) {
    return(.empty_result_table())
  }

  # Parse each variant
  parsed_variants <- lapply(result_list, function(variant) {

    # Extract gene symbols
    gene_symbols <- tryCatch({
      genes <- variant$genes
      if (!is.null(genes) && length(genes) > 0) {
        paste(sapply(genes, function(g) g$symbol), collapse = "; ")
      } else {
        NA_character_
      }
    }, error = function(e) NA_character_)

    # Extract clinical significance from germline_classification
    clin_sig <- tryCatch({
      # Try germline_classification first (newer format)
      if (!is.null(variant$germline_classification$description) &&
          nchar(variant$germline_classification$description) > 0) {
        variant$germline_classification$description
      } else if (!is.null(variant$clinical_significance$description)) {
        # Fallback to older format
        variant$clinical_significance$description
      } else {
        NA_character_
      }
    }, error = function(e) NA_character_)

    # Extract review status
    review_status <- tryCatch({
      # Try germline_classification first (newer format)
      if (!is.null(variant$germline_classification$review_status) &&
          nchar(variant$germline_classification$review_status) > 0) {
        variant$germline_classification$review_status
      } else if (!is.null(variant$clinical_significance$review_status)) {
        # Fallback to older format
        variant$clinical_significance$review_status
      } else {
        NA_character_
      }
    }, error = function(e) NA_character_)

    # Extract phenotypes/traits from germline_classification
    phenotypes <- tryCatch({
      trait_set <- NULL

      # Try germline_classification first (newer format)
      if (!is.null(variant$germline_classification$trait_set)) {
        trait_set <- variant$germline_classification$trait_set
      } else if (!is.null(variant$trait_set)) {
        # Fallback to older format
        trait_set <- variant$trait_set
      }

      if (!is.null(trait_set) && length(trait_set) > 0) {
        trait_names <- sapply(trait_set, function(t) {
          if (!is.null(t$trait_name)) {
            t$trait_name
          } else if (!is.null(t$name)) {
            t$name
          } else {
            NA_character_
          }
        })
        paste(na.omit(trait_names), collapse = "; ")
      } else {
        NA_character_
      }
    }, error = function(e) NA_character_)

    # Extract chromosome and position
    chrom <- NA_character_
    position <- NA_integer_

    tryCatch({
      if (!is.null(variant$variation_set) &&
          length(variant$variation_set) > 0 &&
          !is.null(variant$variation_set[[1]]$variation_loc)) {

        loc_list <- variant$variation_set[[1]]$variation_loc
        if (length(loc_list) > 0) {
          loc <- loc_list[[1]]
          if (!is.null(loc$chr)) chrom <- loc$chr
          if (!is.null(loc$start)) position <- as.integer(loc$start)
        }
      }
    }, error = function(e) NULL)

    # Extract molecular consequence
    mol_consequence <- tryCatch({
      # Try molecular_consequence_list first (newer format)
      if (!is.null(variant$molecular_consequence_list) &&
          length(variant$molecular_consequence_list) > 0) {
        paste(unlist(variant$molecular_consequence_list), collapse = "; ")
      } else if (!is.null(variant$variation_set) &&
                 length(variant$variation_set) > 0 &&
                 !is.null(variant$variation_set[[1]]$consequence_type)) {
        # Fallback to older format
        consequence <- variant$variation_set[[1]]$consequence_type
        if (!is.null(consequence$value)) {
          consequence$value
        } else {
          NA_character_
        }
      } else {
        NA_character_
      }
    }, error = function(e) NA_character_)

    # Create row
    tibble::tibble(
      gene_symbol = gene_symbols,
      variant_id = as.character(variant$uid),
      variant_name = if (!is.null(variant$title)) variant$title else NA_character_,
      chromosome = chrom,
      position = position,
      clinical_significance = clin_sig,
      review_status = review_status,
      phenotypes = phenotypes,
      molecular_consequence = mol_consequence,
      database = "ClinVar"
    )
  })

  # Combine into single tibble
  result_table <- dplyr::bind_rows(parsed_variants)

  return(result_table)
}


# Internal function for rate limiting
.rate_limit <- function(api_key) {

  # Get or create rate limiter state
  if (!exists(".clinvar_last_request", envir = .GlobalEnv)) {
    assign(".clinvar_last_request", Sys.time(), envir = .GlobalEnv)
    return(invisible(NULL))
  }

  last_request <- get(".clinvar_last_request", envir = .GlobalEnv)

  # Calculate required delay
  # 3 requests/second without key, 10 with key
  requests_per_second <- if (is.null(api_key)) 3 else 10
  min_interval <- 1 / requests_per_second

  time_since_last <- as.numeric(difftime(Sys.time(), last_request, units = "secs"))

  if (time_since_last < min_interval) {
    Sys.sleep(min_interval - time_since_last)
  }

  # Update last request time
  assign(".clinvar_last_request", Sys.time(), envir = .GlobalEnv)

  invisible(NULL)
}


# Internal function to create empty result table with correct structure
.empty_result_table <- function() {
  tibble::tibble(
    gene_symbol = character(0),
    variant_id = character(0),
    variant_name = character(0),
    chromosome = character(0),
    position = integer(0),
    clinical_significance = character(0),
    review_status = character(0),
    phenotypes = character(0),
    molecular_consequence = character(0),
    database = character(0)
  )
}

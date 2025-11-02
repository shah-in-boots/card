# Genetic Variant Database Query Functions

# Package-level environment for rate limiter state (safer than .GlobalEnv)
.card_env <- new.env(parent = emptyenv())

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
#'   Default is 1000. Maximum allowed is 10,000. Large queries will automatically
#'   paginate and may take several minutes due to API rate limits.
#'
#' @param genes Optional character vector of gene symbols to filter results.
#'   If provided, only variants in these genes will be returned. Default is
#'   NULL (return all genes).
#'
#' @param clean_gene_symbols Logical indicating whether to clean gene symbols
#'   by removing pseudogenes and non-standard gene symbols (e.g., LOC* genes)
#'   from multi-gene entries. When TRUE (default), entries like "LOC123; TTN"
#'   become "TTN". No data is lost; all rows are retained.
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
#'
#' # Filter to specific genes only
#' af_variants <- query_genetic_variants(
#'   "atrial fibrillation",
#'   genes = c("SCN5A", "KCNQ1", "KCNH2")
#' )
#'
#' # Keep pseudogenes in gene symbols (don't clean)
#' af_variants <- query_genetic_variants(
#'   "atrial fibrillation",
#'   clean_gene_symbols = FALSE
#' )
#'
#' # Large query with automatic pagination
#' # Progress updates shown for queries > 1000 variants
#' hcm_variants <- query_genetic_variants(
#'   "hypertrophic cardiomyopathy",
#'   max_results = 5000
#' )
#' }
#'
#' @export
query_genetic_variants <- function(phenotype,
                                   database = "clinvar",
                                   api_key = NULL,
                                   max_results = 1000,
                                   genes = NULL,
                                   clean_gene_symbols = TRUE) {

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
      max_results < 1 || max_results > 10000) {
    stop("'max_results' must be a single number between 1 and 10,000")
  }

  if (!is.null(genes) && !is.character(genes)) {
    stop("'genes' must be NULL or a character vector")
  }

  if (!is.logical(clean_gene_symbols) || length(clean_gene_symbols) != 1) {
    stop("'clean_gene_symbols' must be a single logical value")
  }

  # Route to appropriate database function
  database <- tolower(database)

  result <- switch(
    database,
    "clinvar" = .query_clinvar(phenotype, api_key, max_results),
    stop("Unsupported database: '", database, "'. Currently only 'clinvar' is supported.")
  )

  # Post-process results
  if (nrow(result) > 0) {
    # Clean gene symbols if requested
    if (clean_gene_symbols) {
      result <- .clean_gene_symbols(result)
    }

    # Filter to specific genes if requested
    if (!is.null(genes)) {
      result <- .filter_genes(result, genes)
    }
  }

  return(result)
}


# Internal function to query ClinVar
#' @noRd
#' @keywords internal
.query_clinvar <- function(phenotype, api_key, max_results) {

  # Warn about NCBI's recommendation for large queries
  if (max_results > 1000 && is.null(api_key)) {
    message("Note: Large queries (>1000 results) without an API key may be slow.")
    message("Consider getting a free API key at: https://www.ncbi.nlm.nih.gov/account/")
  }

  if (max_results > 5000) {
    current_hour <- as.POSIXlt(Sys.time())$hour
    current_day <- weekdays(Sys.time())
    is_weekend <- current_day %in% c("Saturday", "Sunday")
    is_off_peak <- current_hour >= 21 || current_hour < 5  # 9 PM - 5 AM ET (approximation)

    if (!is_weekend && !is_off_peak) {
      message("Note: NCBI recommends running large queries (>5000 results) on weekends")
      message("or off-peak hours (9 PM - 5 AM ET) to reduce server load.")
    }
  }

  # Build search query - ClinVar searches across all fields by default
  # This provides flexible matching for phenotypes, genes, and conditions
  search_term <- phenotype

  # Step 1: Search with History Server
  search_result <- .clinvar_search(search_term, api_key)

  if (search_result$total_count == 0) {
    message("No variants found for phenotype: ", phenotype)
    return(.empty_result_table())
  }

  # Inform user about results
  total_available <- search_result$total_count
  to_retrieve <- min(total_available, max_results)

  if (total_available > max_results) {
    message("Found ", total_available, " total variants for '", phenotype,
            "', retrieving first ", to_retrieve, " variants")
  } else {
    message("Found ", total_available, " variants for '", phenotype, "'")
  }

  # Step 2: Retrieve detailed summaries using WebEnv pagination
  # Batch size: 1000 (optimal balance between speed and reliability)
  # NCBI allows up to 10,000 per request, but 1000 reduces timeout risk
  # and works well with the rate limiter (3-10 seconds per batch depending on API key)
  batch_size <- 1000
  n_batches <- ceiling(to_retrieve / batch_size)

  all_results <- list()

  # Show progress for large queries (> 1000 variants)
  show_progress <- to_retrieve > 1000

  for (i in seq_len(n_batches)) {
    retstart <- (i - 1) * batch_size
    retmax <- min(batch_size, to_retrieve - retstart)

    if (show_progress) {
      message("  Retrieving batch ", i, "/", n_batches, " (variants ",
              retstart + 1, "-", retstart + retmax, ")...")
    }

    # Rate limiting
    .rate_limit(api_key)

    # Use WebEnv pagination mode
    batch_results <- .clinvar_summary(
      variant_ids = NULL,
      api_key = api_key,
      web_env = search_result$web_env,
      query_key = search_result$query_key,
      retstart = retstart,
      retmax = retmax
    )

    all_results[[i]] <- batch_results
  }

  # Combine all batches
  combined_results <- dplyr::bind_rows(all_results)

  if (show_progress) {
    message("Retrieved ", nrow(combined_results), " variants successfully")
  }

  return(combined_results)
}


# Internal function to perform ClinVar search (esearch)
# Returns list with: total_count, web_env, query_key, ids
.clinvar_search <- function(search_term, api_key) {

  base_url <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi"

  # Build query parameters with History Server
  query_params <- list(
    db = "clinvar",
    term = search_term,
    retmax = 0,  # Don't return IDs - we'll use History Server for pagination
    retmode = "json",
    usehistory = "y"  # Enable History Server
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
  result <- content$esearchresult

  # Extract information
  total_count <- as.integer(result$count)
  web_env <- result$webenv
  query_key <- result$querykey
  id_list <- result$idlist

  # Return comprehensive search result
  list(
    total_count = total_count,
    web_env = web_env,
    query_key = query_key,
    ids = if (!is.null(id_list) && length(id_list) > 0) unlist(id_list) else character(0)
  )
}


# Internal function to get variant summaries (esummary)
# Supports two modes:
#   1. Legacy: variant_ids provided (comma-separated IDs)
#   2. WebEnv: web_env + query_key provided (History Server pagination)
.clinvar_summary <- function(variant_ids = NULL,
                             api_key = NULL,
                             web_env = NULL,
                             query_key = NULL,
                             retstart = 0,
                             retmax = 1000) {

  base_url <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi"

  # Build query parameters - two modes
  if (!is.null(web_env) && !is.null(query_key)) {
    # WebEnv pagination mode (preferred for large datasets)
    query_params <- list(
      db = "clinvar",
      query_key = query_key,
      WebEnv = web_env,
      retstart = retstart,
      retmax = retmax,
      retmode = "json"
    )
  } else if (!is.null(variant_ids)) {
    # Legacy mode with explicit IDs
    query_params <- list(
      db = "clinvar",
      id = paste(variant_ids, collapse = ","),
      retmode = "json"
    )
  } else {
    stop("Either variant_ids or (web_env + query_key) must be provided")
  }

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

  # Get or create rate limiter state in package environment
  if (!exists(".clinvar_last_request", envir = .card_env)) {
    assign(".clinvar_last_request", Sys.time(), envir = .card_env)
    return(invisible(NULL))
  }

  last_request <- get(".clinvar_last_request", envir = .card_env)

  # Calculate required delay based on NCBI rate limits
  # 3 requests/second without key, 10 with key
  requests_per_second <- if (is.null(api_key)) 3 else 10
  min_interval <- 1 / requests_per_second

  time_since_last <- as.numeric(difftime(Sys.time(), last_request, units = "secs"))

  if (time_since_last < min_interval) {
    Sys.sleep(min_interval - time_since_last)
  }

  # Update last request time
  assign(".clinvar_last_request", Sys.time(), envir = .card_env)

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


# Internal function to clean gene symbols by removing pseudogenes
.clean_gene_symbols <- function(result_table) {

  if (nrow(result_table) == 0) {
    return(result_table)
  }

  # Pattern for pseudogenes:
  # - LOC followed by numbers (e.g., LOC123456)
  # - LINC followed by numbers (long intergenic non-coding RNA)
  # - MIR followed by numbers (microRNA genes)
  pseudo_pattern <- "^(LOC|LINC|MIR)[0-9]"

  # Clean gene symbols intelligently
  result_table$gene_symbol <- unname(sapply(result_table$gene_symbol, function(x) {
    if (is.na(x)) return(NA_character_)

    # Split into multiple genes if present (e.g., "LOC123456; TTN" or "TTN; LOC123456")
    genes <- strsplit(x, ";\\s*")[[1]]

    # Separate real genes from pseudogenes
    real_genes <- genes[!grepl(pseudo_pattern, genes, ignore.case = FALSE)]

    # Prefer real genes if available, otherwise keep first original gene (conservative)
    if (length(real_genes) > 0) {
      real_genes[1]  # Return first real gene
    } else {
      genes[1]  # Conservative: keep original if all are pseudogenes
    }
  }))

  return(result_table)
}


# Internal function to filter results to specific genes
.filter_genes <- function(result_table, gene_list) {

  if (nrow(result_table) == 0 || is.null(gene_list)) {
    return(result_table)
  }

  # Convert gene list to uppercase for case-insensitive matching
  gene_list_upper <- toupper(gene_list)

  # Filter rows where gene_symbol matches any in the gene_list
  # Handle multiple genes separated by semicolons
  filtered <- result_table[sapply(result_table$gene_symbol, function(x) {
    if (is.na(x)) return(FALSE)
    # Split by semicolon and check if any match
    genes <- strsplit(x, ";\\s*")[[1]]
    genes_upper <- toupper(genes)
    any(genes_upper %in% gene_list_upper)
  }), ]

  if (nrow(filtered) == 0) {
    message("No variants found for the specified genes: ", paste(gene_list, collapse = ", "))
  }

  return(filtered)
}


#' Summarize Genetic Variants by Gene
#'
#' Takes variant-level results from [query_genetic_variants()] and aggregates
#' them to unique genes with summary statistics about associated variants.
#'
#' @param phenotype Character string specifying the phenotype or disease
#'   condition to search for (same as [query_genetic_variants()]).
#'
#' @param database Character string specifying which database to query.
#'   Currently supports "clinvar" (default).
#'
#' @param api_key Optional character string containing your NCBI API key.
#'
#' @param max_results Integer specifying maximum number of variants to query
#'   before aggregation. Default is 2000. Maximum allowed is 10,000. Larger
#'   values provide more complete gene lists but take longer to retrieve.
#'
#' @param genes Optional character vector of gene symbols to filter results.
#'
#' @param clean_gene_symbols Logical indicating whether to clean gene symbols
#'   by removing pseudogenes and non-standard gene symbols. Default is TRUE.
#'
#' @return A tibble with one row per unique gene, containing:
#'   \describe{
#'     \item{gene_symbol}{Gene symbol}
#'     \item{n_variants}{Total number of variants for this gene}
#'     \item{n_pathogenic}{Number of pathogenic/likely pathogenic variants}
#'     \item{n_benign}{Number of benign/likely benign variants}
#'     \item{n_vus}{Number of variants of uncertain significance}
#'     \item{phenotypes}{Unique phenotypes associated with this gene (collapsed)}
#'     \item{chromosomes}{Chromosome(s) where gene is located}
#'     \item{database}{Source database}
#'   }
#'
#' @details
#' This function queries genetic variant databases and returns a gene-level
#' summary instead of variant-level details. Each gene appears only once,
#' with counts of different variant classifications and a list of associated
#' phenotypes.
#'
#' This is useful when you want to identify which genes are associated with
#' a phenotype, rather than examining individual variants.
#'
#' @examples
#' \dontrun{
#' # Get gene-level summary for a phenotype
#' genes <- query_genes_by_phenotype("atrial fibrillation")
#'
#' # Filter to specific genes
#' genes <- query_genes_by_phenotype(
#'   "cardiomyopathy",
#'   genes = c("MYH7", "MYBPC3", "TNNT2")
#' )
#' }
#'
#' @export
query_genes_by_phenotype <- function(phenotype,
                                     database = "clinvar",
                                     api_key = NULL,
                                     max_results = 2000,
                                     genes = NULL,
                                     clean_gene_symbols = TRUE) {

  # Get variant-level results
  variants <- query_genetic_variants(
    phenotype = phenotype,
    database = database,
    api_key = api_key,
    max_results = max_results,
    genes = genes,
    clean_gene_symbols = clean_gene_symbols
  )

  # Return empty result if no variants found
  if (nrow(variants) == 0) {
    return(.empty_gene_summary_table())
  }

  # Aggregate by gene
  gene_summary <- variants %>%
    dplyr::group_by(gene_symbol) %>%
    dplyr::summarise(
      n_variants = dplyr::n(),
      n_pathogenic = sum(
        grepl("pathogenic", clinical_significance, ignore.case = TRUE) &
        !grepl("benign", clinical_significance, ignore.case = TRUE),
        na.rm = TRUE
      ),
      n_benign = sum(
        grepl("benign", clinical_significance, ignore.case = TRUE),
        na.rm = TRUE
      ),
      n_vus = sum(
        grepl("uncertain", clinical_significance, ignore.case = TRUE) |
        grepl("conflicting", clinical_significance, ignore.case = TRUE),
        na.rm = TRUE
      ),
      phenotypes = paste(unique(na.omit(phenotypes)), collapse = "; "),
      chromosomes = paste(unique(na.omit(chromosome)), collapse = "; "),
      database = dplyr::first(database),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(n_pathogenic), dplyr::desc(n_variants))

  return(gene_summary)
}


# Internal function to create empty gene summary table
.empty_gene_summary_table <- function() {
  tibble::tibble(
    gene_symbol = character(0),
    n_variants = integer(0),
    n_pathogenic = integer(0),
    n_benign = integer(0),
    n_vus = integer(0),
    phenotypes = character(0),
    chromosomes = character(0),
    database = character(0)
  )
}

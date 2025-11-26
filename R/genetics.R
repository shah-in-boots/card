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
#'   Default is 100. Large queries will automatically paginate in 500-record
#'   batches and may take several minutes due to API rate limits. There is no
#'   hard maximum, but consider the total available (shown in the message) when
#'   requesting large numbers.
#'
#' @param genes Optional character vector of gene symbols to filter results.
#'   If provided, only variants in these genes will be returned. Default is
#'   NULL (return all genes).
#'
#' @param clean_gene_symbols Logical indicating whether to clean/normalize gene
#'   symbol values (remove/replace pseudogenes such as LOC*, LINC*, MIR*).
#'   Default is TRUE.
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
#' af_variants <- query_genetic_variants("atrial fibrillation")
#' }
#'
#' @export
query_genetic_variants <- function(phenotype,
                                   database = "clinvar",
                                   api_key = NULL,
                                   max_results = 100,
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
      max_results < 1) {
    stop("'max_results' must be a single number of 1 or greater")
  }

  if (!is.null(genes) && !is.character(genes)) {
    stop("'genes' must be NULL or a character vector")
  }

  if (!is.logical(clean_gene_symbols) || length(clean_gene_symbols) != 1) {
    stop("'clean_gene_symbols' must be a single logical value")
  }

  database <- tolower(database)

  result <- switch(
    database,
    "clinvar" = .query_clinvar(phenotype, api_key, max_results),
    stop("Unsupported database: '", database, "'. Currently only 'clinvar' is supported.")
  )

  if (nrow(result) > 0) {
    if (clean_gene_symbols) {
      result <- .clean_gene_symbols(result)
    }
    if (!is.null(genes)) {
      result <- .filter_genes(result, genes)
    }
  }

  return(result)
}


#' Summarize VEP-Annotated VCF Headers
#'
#' Inspect the INFO header lines of a VCF produced by Ensembl VEP to understand
#' which annotations are available (e.g., CSQ/ANN content) and how they are
#' encoded. This helper makes it easier to decide which columns to keep when
#' parsing the data table.
#'
#' @param vcf_path Path to a VCF file annotated with Ensembl VEP.
#' @param info_id INFO field identifier that stores the VEP annotations. The
#'   default (`"CSQ"`) matches the format produced by the `vep` command.
#'
#' @return A list with elements:
#'   \describe{
#'     \item{info_fields}{A tibble with parsed INFO metadata (ID, Number, Type,
#'       Description) for each INFO line in the header.}
#'     \item{csq_fields}{Character vector describing the pipe-delimited CSQ/ANN
#'       annotation order as defined by VEP (e.g., `Allele`, `Consequence`,
#'       `IMPACT`, `SYMBOL`).}
#'     \item{lof_codes}{Named list describing loss-of-function confidence codes
#'       (high/low confidence). The codes are derived from the VEP LOF plugin
#'       definitions (typically `HC` and `LC`).}
#'   }
#'
#' @details
#' The Ensembl VEP header provides a `Format: ...` statement inside the INFO
#' line for the CSQ/ANN field. This function extracts those field names so that
#' downstream parsing can map each pipe-separated element to a column. If the
#' LOF plugin is present, the resulting data typically contain `LoF`,
#' `LoF_filter`, `LoF_flags`, and `LoF_info` columns where `LoF` values of `HC`
#' (high confidence) or `LC` (low confidence) indicate the plugin's assessment.
#'
#' @examples
#' \dontrun{
#' header_info <- vep_header_summary("path/to/annotated.vcf.gz")
#' header_info$csq_fields
#' }
#'
#' @export
vep_header_summary <- function(vcf_path, info_id = "CSQ") {
  if (!file.exists(vcf_path)) {
    stop("File not found: ", vcf_path)
  }

  header_lines <- readLines(vcf_path, warn = FALSE)
  header_lines <- header_lines[grepl("^##", header_lines)]

  if (length(header_lines) == 0) {
    stop("No VCF header lines detected. Confirm the file is a valid VCF.")
  }

  info_lines <- header_lines[grepl("^##INFO=<", header_lines)]

  parse_info_line <- function(x) {
    content <- sub("^##INFO=<", "", x)
    content <- sub(">$", "", content)
    parts <- strsplit(content, ",(?=[A-Za-z]+\\=)", perl = TRUE)[[1]]
    key_vals <- strsplit(parts, "=", fixed = TRUE)
    names <- vapply(key_vals, `[[`, character(1), 1)
    values <- vapply(key_vals, function(y) paste(y[-1], collapse = "="), character(1))
    tibble::tibble(
      id = names[match("ID", names)],
      number = values[match("Number", names)],
      type = values[match("Type", names)],
      description = values[match("Description", names)]
    )
  }

  info_df <- purrr::map_dfr(info_lines, parse_info_line)

  csq_line <- info_lines[grepl(paste0("ID=", info_id, "[,>]"), info_lines)]
  csq_fields <- character(0)
  if (length(csq_line) > 0) {
    format_match <- regmatches(csq_line[1], regexpr("Format: [^"]+", csq_line[1]))
    if (length(format_match) > 0) {
      format_string <- sub("^Format: ", "", format_match)
      csq_fields <- strsplit(format_string, "\\|", fixed = FALSE)[[1]]
    }
  }

  list(
    info_fields = info_df,
    csq_fields = csq_fields,
    lof_codes = list(
      LoF = c(HC = "High confidence loss-of-function", LC = "Low confidence loss-of-function")
    )
  )
}


#' Parse VEP-Annotated VCF Files
#'
#' Convert an annotated VCF file into a tidy tibble by expanding the CSQ/ANN
#' annotation field and selecting commonly used consequence columns (e.g.,
#' `SIFT`, `PolyPhen`, `LoF`). Optional filters allow you to keep only variants
#' matching specific impact or loss-of-function confidence levels.
#'
#' @param vcf_path Path to a VCF file annotated with Ensembl VEP.
#' @param info_id INFO field identifier that stores the VEP annotations.
#' @param selected_fields Character vector of CSQ/ANN columns to retain. Columns
#'   absent from the file are silently dropped.
#' @param impact_filter Optional character vector of `IMPACT` labels to retain
#'   (e.g., `c("HIGH", "MODERATE")`).
#' @param lof_confidence Optional character vector of `LoF` confidence codes to
#'   retain (typically `"HC"` or `"LC"`).
#' @param polyphen_min Optional numeric cutoff; keeps rows with parsed PolyPhen
#'   probability greater than or equal to this value when available.
#' @param sift_max Optional numeric cutoff; keeps rows with parsed SIFT
#'   probability less than or equal to this value when available.
#'
#' @return A tibble with base VCF columns (`CHROM`, `POS`, `ID`, `REF`, `ALT`,
#'   `QUAL`, `FILTER` when present) plus the selected CSQ/ANN annotation
#'   columns. One row is returned per alternate allele consequence.
#'
#' @examples
#' \dontrun{
#' parsed <- parse_annotated_vcf(
#'   "path/to/annotated.vcf.gz",
#'   selected_fields = c("Consequence", "IMPACT", "SYMBOL", "SIFT", "PolyPhen")
#' )
#' }
#'
#' @export
parse_annotated_vcf <- function(vcf_path,
                                info_id = "CSQ",
                                selected_fields = c(
                                  "Consequence", "IMPACT", "SYMBOL", "Gene",
                                  "Feature", "BIOTYPE", "HGVSp", "HGVSc",
                                  "EXON", "INTRON", "SIFT", "PolyPhen",
                                  "LoF", "LoF_filter", "LoF_flags", "LoF_info"
                                ),
                                impact_filter = NULL,
                                lof_confidence = NULL,
                                polyphen_min = NULL,
                                sift_max = NULL) {

  header_info <- vep_header_summary(vcf_path, info_id = info_id)
  csq_fields <- header_info$csq_fields

  if (length(csq_fields) == 0) {
    stop("Unable to identify CSQ/ANN format in VCF header. Check the INFO line for ", info_id)
  }

  raw_lines <- readLines(vcf_path, warn = FALSE)
  header_line <- tail(grep("^#CHROM", raw_lines, value = TRUE), 1)
  if (length(header_line) == 0) {
    stop("VCF header with column names (#CHROM ...) not found.")
  }
  col_names <- strsplit(sub("^#", "", header_line), "\t")[[1]]

  vcf_data <- utils::read.delim(
    vcf_path,
    comment.char = "#",
    header = FALSE,
    stringsAsFactors = FALSE,
    sep = "\t",
    quote = "",
    check.names = FALSE
  )

  if (ncol(vcf_data) < length(col_names)) {
    stop("VCF file has fewer columns than expected from the header line.")
  }

  colnames(vcf_data)[seq_along(col_names)] <- col_names

  info_values <- vapply(strsplit(vcf_data$INFO, ";"), function(parts) {
    match_val <- parts[grepl(paste0("^", info_id, "="), parts)]
    if (length(match_val) == 0) return(NA_character_)
    sub(paste0("^", info_id, "="), "", match_val[1])
  }, character(1))

  base_cols <- intersect(c("CHROM", "POS", "ID", "REF", "ALT", "QUAL", "FILTER"), colnames(vcf_data))
  selected_fields <- intersect(selected_fields, csq_fields)

  tidy <- vcf_data %>%
    dplyr::mutate(.vep_raw = info_values) %>%
    dplyr::filter(!is.na(.vep_raw)) %>%
    tidyr::separate_rows(.vep_raw, sep = ",") %>%
    tidyr::separate(.vep_raw, into = csq_fields, sep = "\\|", fill = "right", extra = "drop") %>%
    dplyr::select(dplyr::all_of(base_cols), dplyr::any_of(selected_fields))

  if (!is.null(impact_filter) && "IMPACT" %in% colnames(tidy)) {
    tidy <- dplyr::filter(tidy, IMPACT %in% impact_filter)
  }

  if (!is.null(lof_confidence) && "LoF" %in% colnames(tidy)) {
    tidy <- dplyr::filter(tidy, LoF %in% lof_confidence)
  }

  numeric_from_annotation <- function(x) {
    suppressWarnings(as.numeric(sub(".*\\(([^()]*)\\).*", "\\1", x)))
  }

  if (!is.null(polyphen_min) && "PolyPhen" %in% colnames(tidy)) {
    tidy <- tidy %>%
      dplyr::mutate(.polyphen_prob = numeric_from_annotation(PolyPhen)) %>%
      dplyr::filter(!is.na(.polyphen_prob) & .polyphen_prob >= polyphen_min) %>%
      dplyr::select(-.polyphen_prob)
  }

  if (!is.null(sift_max) && "SIFT" %in% colnames(tidy)) {
    tidy <- tidy %>%
      dplyr::mutate(.sift_prob = numeric_from_annotation(SIFT)) %>%
      dplyr::filter(!is.na(.sift_prob) & .sift_prob <= sift_max) %>%
      dplyr::select(-.sift_prob)
  }

  tibble::as_tibble(tidy)
}


# Internal function to query ClinVar
#' @noRd
#' @keywords internal
.query_clinvar <- function(phenotype, api_key, max_results) {
  search_term <- phenotype
  search_result <- .clinvar_search(search_term, api_key, max_results)
  if (search_result$total_count == 0) {
    message("No variants found for phenotype: ", phenotype)
    return(.empty_result_table())
  }
  total_available <- search_result$total_count
  to_retrieve <- min(total_available, max_results)
  if (total_available > max_results) {
    message("Found ", total_available, " total variants for '", phenotype,
            "', retrieving first ", to_retrieve, " variants")
  } else {
    message("Found ", total_available, " variants for '", phenotype, "'")
  }
  batch_size <- 500
  n_batches <- ceiling(to_retrieve / batch_size)
  all_results <- list()
  show_progress <- to_retrieve > 1000
  for (i in seq_len(n_batches)) {
    retstart <- (i - 1) * batch_size
    retmax <- min(batch_size, to_retrieve - retstart)
    if (show_progress) {
      message("  Retrieving batch ", i, "/", n_batches, " (variants ",
              retstart + 1, "-", retstart + retmax, ")...")
    }
    .rate_limit(api_key)
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
  combined_results <- dplyr::bind_rows(all_results)
  if (show_progress) {
    message("Retrieved ", nrow(combined_results), " variants successfully")
  }
  return(combined_results)
}


# Internal function to perform ClinVar search (esearch)
.clinvar_search <- function(search_term, api_key, max_results) {
  base_url <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi"
  query_params <- list(
    db = "clinvar",
    term = search_term,
    retmax = max_results,
    retmode = "json",
    usehistory = "y"
  )
  if (!is.null(api_key)) query_params$api_key <- api_key
  .rate_limit(api_key)
  response <- httr::GET(base_url, query = query_params)
  if (httr::http_error(response)) {
    stop("ClinVar API request failed with status ", httr::status_code(response))
  }
  content <- httr::content(response, as = "parsed")
  result <- content$esearchresult
  list(
    total_count = as.integer(result$count),
    web_env = result$webenv,
    query_key = result$querykey,
    ids = if (!is.null(result$idlist) && length(result$idlist) > 0) unlist(result$idlist) else character(0)
  )
}


# Internal function to get variant summaries (esummary)
.clinvar_summary <- function(variant_ids = NULL,
                             api_key = NULL,
                             web_env = NULL,
                             query_key = NULL,
                             retstart = 0,
                             retmax = 500) {
  base_url <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi"
  if (!is.null(web_env) && !is.null(query_key)) {
    query_params <- list(
      db = "clinvar",
      query_key = query_key,
      WebEnv = web_env,
      retstart = retstart,
      retmax = retmax,
      retmode = "json"
    )
  } else if (!is.null(variant_ids)) {
    query_params <- list(db = "clinvar", id = paste(variant_ids, collapse = ","), retmode = "json")
  } else {
    stop("Either variant_ids or (web_env + query_key) must be provided")
  }
  if (!is.null(api_key)) query_params$api_key <- api_key
  response <- httr::GET(base_url, query = query_params)
  if (httr::http_error(response)) {
    warning("ESummary request failed with status ", httr::status_code(response))
    return(.empty_result_table())
  }
  content <- httr::content(response, as = "parsed")
  results <- .parse_clinvar_summary(content)
  return(results)
}


# Internal function to parse ClinVar ESummary JSON response
.parse_clinvar_summary <- function(content) {
  result_list <- content$result
  result_list$uids <- NULL
  if (length(result_list) == 0) return(.empty_result_table())
  parsed_variants <- lapply(result_list, function(variant) {
    gene_symbols <- tryCatch({
      genes <- variant$genes
      if (!is.null(genes) && length(genes) > 0) {
        paste(sapply(genes, function(g) g$symbol), collapse = "; ")
      } else {
        NA_character_
      }
    }, error = function(e) NA_character_)
    clin_sig <- tryCatch({
      if (!is.null(variant$germline_classification$description) &&
          nchar(variant$germline_classification$description) > 0) {
        variant$germline_classification$description
      } else if (!is.null(variant$clinical_significance$description)) {
        variant$clinical_significance$description
      } else {
        NA_character_
      }
    }, error = function(e) NA_character_)
    review_status <- tryCatch({
      if (!is.null(variant$germline_classification$review_status) &&
          nchar(variant$germline_classification$review_status) > 0) {
        variant$germline_classification$review_status
      } else if (!is.null(variant$clinical_significance$review_status)) {
        variant$clinical_significance$review_status
      } else {
        NA_character_
      }
    }, error = function(e) NA_character_)
    phenotypes <- tryCatch({
      trait_set <- NULL
      if (!is.null(variant$germline_classification$trait_set)) {
        trait_set <- variant$germline_classification$trait_set
      } else if (!is.null(variant$trait_set)) {
        trait_set <- variant$trait_set
      }
      if (!is.null(trait_set) && length(trait_set) > 0) {
        trait_names <- sapply(trait_set, function(t) {
          if (!is.null(t$trait_name)) t$trait_name else if (!is.null(t$name)) t$name else NA_character_
        })
        paste(na.omit(trait_names), collapse = "; ")
      } else {
        NA_character_
      }
    }, error = function(e) NA_character_)
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
    mol_consequence <- tryCatch({
      if (!is.null(variant$molecular_consequence_list) && length(variant$molecular_consequence_list) > 0) {
        paste(unlist(variant$molecular_consequence_list), collapse = "; ")
      } else if (!is.null(variant$variation_set) && length(variant$variation_set) > 0 && !is.null(variant$variation_set[[1]]$consequence_type)) {
        consequence <- variant$variation_set[[1]]$consequence_type
        if (!is.null(consequence$value)) consequence$value else NA_character_
      } else {
        NA_character_
      }
    }, error = function(e) NA_character_)
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
  result_table <- dplyr::bind_rows(parsed_variants)
  return(result_table)
}


# Internal function for rate limiting
.rate_limit <- function(api_key) {
  delay <- if (is.null(api_key)) 1/3 else 1/10
  Sys.sleep(delay)
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


# Internal function to clean gene symbols (formerly filter pseudogenes)
.clean_gene_symbols <- function(result_table) {
  if (nrow(result_table) == 0) return(result_table)
  pseudo_pattern <- "^(LOC|LINC|MIR)[0-9]"
  result_table$gene_symbol <- unname(sapply(result_table$gene_symbol, function(x) {
    if (is.na(x)) return(NA_character_)
    genes <- strsplit(x, ";\\s*")[[1]]
    real_genes <- genes[!grepl(pseudo_pattern, genes, ignore.case = FALSE)]
    if (length(real_genes) > 0) {
      real_genes[1]
    } else {
      genes[1]
    }
  }))
  return(result_table)
}


# Internal function to filter results to specific genes
.filter_genes <- function(result_table, gene_list) {
  if (nrow(result_table) == 0 || is.null(gene_list)) return(result_table)
  gene_list_upper <- toupper(gene_list)
  filtered <- result_table[sapply(result_table$gene_symbol, function(x) {
    if (is.na(x)) return(FALSE)
    genes <- strsplit(x, ";\\s*")[[1]]
    any(toupper(genes) %in% gene_list_upper)
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
#'
#' @param api_key Optional character string containing your NCBI API key.
#'
#' @param max_results Integer specifying maximum number of variants to query
#'   before aggregation. Default is 500. Large queries will automatically
#'   paginate in 500-record batches and may take several minutes due to API
#'   rate limits. There is no hard maximum.
#'
#' @param genes Optional character vector of gene symbols to filter results.
#'
#' @param clean_gene_symbols Logical indicating whether to clean gene symbols.
#'   Default is TRUE.
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
#' @export
query_genes_by_phenotype <- function(phenotype,
                                     database = "clinvar",
                                     api_key = NULL,
                                     max_results = 500,
                                     genes = NULL,
                                     clean_gene_symbols = TRUE) {
  variants <- query_genetic_variants(
    phenotype = phenotype,
    database = database,
    api_key = api_key,
    max_results = max_results,
    genes = genes,
    clean_gene_symbols = clean_gene_symbols
  )
  if (nrow(variants) == 0) {
    return(.empty_gene_summary_table())
  }
  gene_summary <- variants %>%
    dplyr::group_by(gene_symbol) %>%
    dplyr::summarise(
      n_variants = dplyr::n(),
      n_pathogenic = sum(
        grepl("pathogenic", clinical_significance, ignore.case = TRUE) &
        !grepl("benign", clinical_significance, ignore.case = TRUE),
        na.rm = TRUE
      ),
      n_benign = sum(grepl("benign", clinical_significance, ignore.case = TRUE), na.rm = TRUE),
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

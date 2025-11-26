# VCF & VEP Files ----

#' Read VCF/VEP Header Annotations
#'
#' Extract field names and descriptions from VCF or VEP-annotated file headers.
#' This helps you understand what annotations are available without reading the
#' entire file.
#'
#' @param vcf_path Path to a VCF file (standard or VEP-annotated).
#'
#' @return A named list where names are field names (e.g., `SYMBOL`, `Consequence`,
#'   `LoF`) and values are their descriptions.
#'
#' @details This function extracts annotation metadata from both standard VCF
#' headers (##INFO and ##FORMAT lines) and VEP text format headers (## Column
#' descriptions and ## Extra column keys sections). For VEP files, key
#' annotations include consequence predictions (SIFT, PolyPhen), impact
#' classifications (IMPACT), loss-of-function calls (LoF), and allele
#' frequencies (gnomAD, 1000 Genomes).
#'
#' @examples
#' \dontrun{
#' annotations <- read_vep_header("path/to/annotated.vcf.gz")
#' annotations$LoF
#' names(annotations)
#' }
#'
#' @export
read_vep_header <- function(vcf_path) {
  if (!file.exists(vcf_path)) {
    stop("File not found: ", vcf_path)
  }

  lines <- readLines(vcf_path, warn = FALSE)
  header_lines <- lines[grepl("^##", lines)]

  if (length(header_lines) == 0) {
    stop("No VCF header lines detected. Confirm the file is a valid VCF.")
  }

  # Check if this is VEP text format or standard VCF
  is_vep_text <- any(grepl("^## [Cc]olumn [Dd]escriptions:", header_lines))

  if (is_vep_text) {
    # VEP text format: extract from "Column descriptions" and "Extra column keys"
    fields <- character()
    descriptions <- character()

    # Find section boundaries
    col_start <- which(grepl("^## [Cc]olumn [Dd]escriptions:", header_lines))
    extra_start <- which(grepl("^## [Ee]xtra [Cc]olumn [Kk]eys:", header_lines))
    cmd_line <- which(grepl("^## VEP command-line:", header_lines))

    # Parse the column descriptions section
    if (length(col_start) > 0) {
      end_idx <- if (length(extra_start) > 0) extra_start - 1 else if (length(cmd_line) > 0) cmd_line - 1 else length(header_lines)
      if (end_idx > col_start) {
        section <- header_lines[(col_start + 1):end_idx]
        for (line in section) {
          parts <- strsplit(line, "\\s+:\\s+", perl = TRUE)[[1]]
          if (length(parts) == 2) {
            field <- sub("^##\\s+", "", parts[1])
            fields <- c(fields, field)
            descriptions <- c(descriptions, parts[2])
          }
        }
      }
    }

    # Parse the extra column keys section
    if (length(extra_start) > 0) {
      end_idx <- if (length(cmd_line) > 0) cmd_line - 1 else length(header_lines)
      if (end_idx > extra_start) {
        section <- header_lines[(extra_start + 1):end_idx]
        for (line in section) {
          parts <- strsplit(line, "\\s+:\\s+", perl = TRUE)[[1]]
          if (length(parts) == 2) {
            field <- sub("^##\\s+", "", parts[1])
            fields <- c(fields, field)
            descriptions <- c(descriptions, parts[2])
          }
        }
      }
    }
  } else {
    # Standard VCF format: parse INFO and FORMAT lines
    fields <- character()
    descriptions <- character()

    # Parse INFO lines
    info_lines <- header_lines[grepl("^##INFO=<", header_lines)]
    for (line in info_lines) {
      id_match <- regmatches(line, regexpr("ID=[^,>]+", line))
      desc_match <- regmatches(line, regexpr("Description=\"[^\"]+\"", line))
      if (length(id_match) > 0 && length(desc_match) > 0) {
        field <- sub("ID=", "", id_match)
        description <- sub("Description=\"(.+)\"", "\\1", desc_match)
        fields <- c(fields, field)
        descriptions <- c(descriptions, description)
      }
    }

    # Parse FORMAT lines
    format_lines <- header_lines[grepl("^##FORMAT=<", header_lines)]
    for (line in format_lines) {
      id_match <- regmatches(line, regexpr("ID=[^,>]+", line))
      desc_match <- regmatches(line, regexpr("Description=\"[^\"]+\"", line))
      if (length(id_match) > 0 && length(desc_match) > 0) {
        field <- sub("ID=", "", id_match)
        description <- sub("Description=\"(.+)\"", "\\1", desc_match)
        fields <- c(fields, field)
        descriptions <- c(descriptions, description)
      }
    }

    # Extract CSQ/ANN fields if present
    csq_line <- info_lines[grepl("Format:", info_lines)]
    if (length(csq_line) > 0) {
      format_match <- regmatches(csq_line[1], regexpr("Format: [^\"]+", csq_line[1]))
      if (length(format_match) > 0) {
        format_str <- sub("^Format: ", "", format_match)
        csq_fields <- strsplit(format_str, "\\|")[[1]]
        # Remove CSQ/ANN from main list and add individual fields
        keep_idx <- !(fields %in% c("CSQ", "ANN"))
        fields <- fields[keep_idx]
        descriptions <- descriptions[keep_idx]
        for (field in csq_fields) {
          fields <- c(fields, field)
          descriptions <- c(descriptions, paste0("VEP annotation: ", field))
        }
      }
    }
  }

  # Return as named list
  result <- as.list(descriptions)
  names(result) <- fields
  result
}


#' Read VCF/VEP Data into Tibble
#'
#' Read variant data from a VCF or VEP-annotated file and extract selected
#' columns into a tibble. This allows you to build variant databases by
#' combining data from multiple files using tibble's row-binding capabilities.
#'
#' @param vcf_path Path to a VCF file (standard or VEP-annotated).
#'
#' @param columns Character vector of column names to extract. If NULL (default),
#'   all available columns are returned. Use [read_vep_header()] to see available
#'   column names. For VEP text format files, this includes columns like
#'   "Uploaded_variation", "Location", "Allele", "Gene", "Consequence", and all
#'   fields in the Extra column (e.g., "SYMBOL", "IMPACT", "SIFT", "LoF"). For
#'   standard VCF files with CSQ annotations, this includes the pipe-delimited
#'   VEP fields.
#'
#' @return A tibble with one row per variant and columns for each selected field.
#'   Missing values are represented as NA.
#'
#' @details This function handles two VCF formats:
#'
#' **VEP text format**: Files with "## Column descriptions:" headers. These have
#' tab-delimited columns plus additional fields in an "Extra" column. The function
#' automatically parses the Extra column and creates separate columns for each
#' key=value pair.
#'
#' **Standard VCF with CSQ**: Files with standard VCF structure and a CSQ field
#' in the INFO column containing pipe-delimited VEP annotations. The function
#' parses the CSQ field according to the Format specification in the header.
#'
#' The resulting tibble can be easily combined with data from other VCF/VEP files
#' using standard tibble operations like [dplyr::bind_rows()].
#'
#' @examples
#' \dontrun{
#' # See available columns
#' annotations <- read_vep_header("annotated.vcf.gz")
#' names(annotations)
#'
#' # Read all columns
#' variants <- read_vep_data("annotated.vcf.gz")
#'
#' # Read selected columns only
#' variants <- read_vep_data(
#'   "annotated.vcf.gz",
#'   columns = c("Uploaded_variation", "SYMBOL", "Consequence", "IMPACT", "LoF")
#' )
#'
#' # Combine multiple files
#' v1 <- read_vep_data("file1.vcf", columns = c("SYMBOL", "Consequence"))
#' v2 <- read_vep_data("file2.vcf", columns = c("SYMBOL", "Consequence"))
#' combined <- dplyr::bind_rows(v1, v2)
#' }
#'
#' @export
read_vep_data <- function(vcf_path, columns = NULL) {
  if (!file.exists(vcf_path)) {
    stop("File not found: ", vcf_path)
  }

  lines <- readLines(vcf_path, warn = FALSE)
  header_lines <- lines[grepl("^##", lines)]

  if (length(header_lines) == 0) {
    stop("No VCF header lines detected. Confirm the file is a valid VCF.")
  }

  # Check if this is VEP text format or standard VCF
  is_vep_text <- any(grepl("^## [Cc]olumn [Dd]escriptions:", header_lines))

  if (is_vep_text) {
    # VEP text format
    result <- .read_vep_text_format(lines, columns)
  } else {
    # Standard VCF format with CSQ
    result <- .read_vcf_csq_format(lines, header_lines, columns)
  }

  return(result)
}

# VCF & VEP Helpers ----

# Internal function to read VEP text format files
.read_vep_text_format <- function(lines, columns = NULL) {
  # Find the column header line (starts with #Uploaded_variation)
  header_idx <- which(grepl("^#Uploaded_variation", lines))

  if (length(header_idx) == 0) {
    stop("Could not find VEP text format header line")
  }

  header_line <- lines[header_idx]
  col_names <- strsplit(header_line, "\t")[[1]]
  col_names <- sub("^#", "", col_names)

  # Find data lines (everything after header, not starting with #)
  data_start <- header_idx + 1
  data_lines <- lines[data_start:length(lines)]
  data_lines <- data_lines[!grepl("^#", data_lines) & nchar(data_lines) > 0]

  if (length(data_lines) == 0) {
    # Return empty tibble with requested columns
    if (is.null(columns)) {
      columns <- col_names
    }
    result <- tibble::tibble()
    for (col in columns) {
      result[[col]] <- character(0)
    }
    return(result)
  }

  # Parse data lines
  data_list <- lapply(data_lines, function(line) {
    fields <- strsplit(line, "\t")[[1]]
    row <- as.list(fields)
    names(row) <- col_names[1:length(fields)]

    # Parse Extra column if present
    if ("Extra" %in% names(row)) {
      extra_str <- row$Extra
      extra_pairs <- strsplit(extra_str, ";")[[1]]
      for (pair in extra_pairs) {
        parts <- strsplit(pair, "=")[[1]]
        if (length(parts) == 2) {
          key <- parts[1]
          value <- parts[2]
          row[[key]] <- value
        }
      }
      # Remove the Extra column
      row$Extra <- NULL
    }

    row
  })

  # Get all unique column names across all rows
  all_cols <- unique(unlist(lapply(data_list, names)))

  # If columns specified, validate and filter
  if (!is.null(columns)) {
    missing_cols <- setdiff(columns, all_cols)
    if (length(missing_cols) > 0) {
      warning("Requested columns not found in data: ", paste(missing_cols, collapse = ", "))
    }
    all_cols <- intersect(columns, all_cols)
  }

  # Build tibble column by column as a list
  result_list <- list()
  for (col in all_cols) {
    values <- sapply(data_list, function(row) {
      if (col %in% names(row)) row[[col]] else NA_character_
    })
    result_list[[col]] <- values
  }

  # Convert to tibble
  tibble::as_tibble(result_list)
}


# Internal function to read standard VCF with CSQ format
.read_vcf_csq_format <- function(lines, header_lines, columns = NULL) {
  # Find CSQ format from INFO header
  csq_line <- header_lines[grepl("^##INFO=<ID=CSQ", header_lines)]

  if (length(csq_line) == 0) {
    csq_line <- header_lines[grepl("^##INFO=<ID=ANN", header_lines)]
    csq_field <- "ANN"
  } else {
    csq_field <- "CSQ"
  }

  if (length(csq_line) == 0) {
    stop("No CSQ or ANN field found in VCF header. This may not be a VEP-annotated file.")
  }

  # Extract CSQ format
  format_match <- regmatches(csq_line[1], regexpr("Format: [^\"]+", csq_line[1]))
  if (length(format_match) == 0) {
    stop("Could not parse CSQ format from header")
  }

  format_str <- sub("^Format: ", "", format_match)
  csq_col_names <- strsplit(format_str, "\\|")[[1]]

  # Find column header line
  header_idx <- which(grepl("^#CHROM", lines))
  if (length(header_idx) == 0) {
    stop("Could not find VCF column header line")
  }

  # Find data lines
  data_start <- header_idx + 1
  data_lines <- lines[data_start:length(lines)]
  data_lines <- data_lines[!grepl("^#", data_lines) & nchar(data_lines) > 0]

  if (length(data_lines) == 0) {
    # Return empty tibble
    if (is.null(columns)) {
      columns <- csq_col_names
    }
    result <- tibble::tibble()
    for (col in columns) {
      result[[col]] <- character(0)
    }
    return(result)
  }

  # Parse data lines
  data_list <- list()
  for (line in data_lines) {
    fields <- strsplit(line, "\t")[[1]]
    if (length(fields) < 8) next

    info_field <- fields[8]

    # Extract CSQ value
    csq_pattern <- paste0(csq_field, "=([^;]+)")
    csq_match <- regmatches(info_field, regexpr(csq_pattern, info_field))

    if (length(csq_match) == 0) next

    csq_value <- sub(paste0(csq_field, "="), "", csq_match)

    # CSQ can have multiple annotations separated by comma
    csq_annotations <- strsplit(csq_value, ",")[[1]]

    for (annotation in csq_annotations) {
      csq_values <- strsplit(annotation, "\\|")[[1]]

      row <- list()
      for (i in seq_along(csq_col_names)) {
        if (i <= length(csq_values) && nchar(csq_values[i]) > 0) {
          row[[csq_col_names[i]]] <- csq_values[i]
        } else {
          row[[csq_col_names[i]]] <- NA_character_
        }
      }

      data_list[[length(data_list) + 1]] <- row
    }
  }

  if (length(data_list) == 0) {
    # Return empty tibble
    if (is.null(columns)) {
      columns <- csq_col_names
    }
    result <- tibble::tibble()
    for (col in columns) {
      result[[col]] <- character(0)
    }
    return(result)
  }

  # If columns specified, validate
  if (!is.null(columns)) {
    missing_cols <- setdiff(columns, csq_col_names)
    if (length(missing_cols) > 0) {
      warning("Requested columns not found in CSQ format: ", paste(missing_cols, collapse = ", "))
    }
    use_cols <- intersect(columns, csq_col_names)
  } else {
    use_cols <- csq_col_names
  }

  # Build tibble column by column as a list
  result_list <- list()
  for (col in use_cols) {
    values <- sapply(data_list, function(row) {
      if (col %in% names(row)) row[[col]] else NA_character_
    })
    result_list[[col]] <- values
  }

  # Convert to tibble
  tibble::as_tibble(result_list)
}


# Genetic Databases ----

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
query_genetic_variants <- function(
  phenotype,
  database = "clinvar",
  api_key = NULL,
  max_results = 100,
  genes = NULL,
  clean_gene_symbols = TRUE
) {
  # Input validation
  if (
    !is.character(phenotype) || length(phenotype) != 1 || nchar(phenotype) == 0
  ) {
    stop("'phenotype' must be a non-empty character string")
  }

  if (!is.character(database) || length(database) != 1) {
    stop("'database' must be a single character string")
  }

  if (!is.null(api_key) && (!is.character(api_key) || length(api_key) != 1)) {
    stop("'api_key' must be NULL or a single character string")
  }

  if (!is.numeric(max_results) || length(max_results) != 1 || max_results < 1) {
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
    stop(
      "Unsupported database: '",
      database,
      "'. Currently only 'clinvar' is supported."
    )
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
query_genes_by_phenotype <- function(
  phenotype,
  database = "clinvar",
  api_key = NULL,
  max_results = 500,
  genes = NULL,
  clean_gene_symbols = TRUE
) {
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

# Genetic Database Helpers ----

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
    message(
      "Found ",
      total_available,
      " total variants for '",
      phenotype,
      "', retrieving first ",
      to_retrieve,
      " variants"
    )
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
      message(
        "  Retrieving batch ",
        i,
        "/",
        n_batches,
        " (variants ",
        retstart + 1,
        "-",
        retstart + retmax,
        ")..."
      )
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
  if (!is.null(api_key)) {
    query_params$api_key <- api_key
  }
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
    ids = if (!is.null(result$idlist) && length(result$idlist) > 0) {
      unlist(result$idlist)
    } else {
      character(0)
    }
  )
}


# Internal function to get variant summaries (esummary)
.clinvar_summary <- function(
  variant_ids = NULL,
  api_key = NULL,
  web_env = NULL,
  query_key = NULL,
  retstart = 0,
  retmax = 500
) {
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
  if (length(result_list) == 0) {
    return(.empty_result_table())
  }
  parsed_variants <- lapply(result_list, function(variant) {
    gene_symbols <- tryCatch(
      {
        genes <- variant$genes
        if (!is.null(genes) && length(genes) > 0) {
          paste(sapply(genes, function(g) g$symbol), collapse = "; ")
        } else {
          NA_character_
        }
      },
      error = function(e) NA_character_
    )
    clin_sig <- tryCatch(
      {
        if (
          !is.null(variant$germline_classification$description) &&
            nchar(variant$germline_classification$description) > 0
        ) {
          variant$germline_classification$description
        } else if (!is.null(variant$clinical_significance$description)) {
          variant$clinical_significance$description
        } else {
          NA_character_
        }
      },
      error = function(e) NA_character_
    )
    review_status <- tryCatch(
      {
        if (
          !is.null(variant$germline_classification$review_status) &&
            nchar(variant$germline_classification$review_status) > 0
        ) {
          variant$germline_classification$review_status
        } else if (!is.null(variant$clinical_significance$review_status)) {
          variant$clinical_significance$review_status
        } else {
          NA_character_
        }
      },
      error = function(e) NA_character_
    )
    phenotypes <- tryCatch(
      {
        trait_set <- NULL
        if (!is.null(variant$germline_classification$trait_set)) {
          trait_set <- variant$germline_classification$trait_set
        } else if (!is.null(variant$trait_set)) {
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
      },
      error = function(e) NA_character_
    )
    chrom <- NA_character_
    position <- NA_integer_
    tryCatch(
      {
        if (
          !is.null(variant$variation_set) &&
            length(variant$variation_set) > 0 &&
            !is.null(variant$variation_set[[1]]$variation_loc)
        ) {
          loc_list <- variant$variation_set[[1]]$variation_loc
          if (length(loc_list) > 0) {
            loc <- loc_list[[1]]
            if (!is.null(loc$chr)) {
              chrom <- loc$chr
            }
            if (!is.null(loc$start)) position <- as.integer(loc$start)
          }
        }
      },
      error = function(e) NULL
    )
    mol_consequence <- tryCatch(
      {
        if (
          !is.null(variant$molecular_consequence_list) &&
            length(variant$molecular_consequence_list) > 0
        ) {
          paste(unlist(variant$molecular_consequence_list), collapse = "; ")
        } else if (
          !is.null(variant$variation_set) &&
            length(variant$variation_set) > 0 &&
            !is.null(variant$variation_set[[1]]$consequence_type)
        ) {
          consequence <- variant$variation_set[[1]]$consequence_type
          if (!is.null(consequence$value)) consequence$value else NA_character_
        } else {
          NA_character_
        }
      },
      error = function(e) NA_character_
    )
    tibble::tibble(
      gene_symbol = gene_symbols,
      variant_id = as.character(variant$uid),
      variant_name = if (!is.null(variant$title)) {
        variant$title
      } else {
        NA_character_
      },
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
  delay <- if (is.null(api_key)) 1 / 3 else 1 / 10
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
  if (nrow(result_table) == 0) {
    return(result_table)
  }
  pseudo_pattern <- "^(LOC|LINC|MIR)[0-9]"
  result_table$gene_symbol <- unname(sapply(
    result_table$gene_symbol,
    function(x) {
      if (is.na(x)) {
        return(NA_character_)
      }
      genes <- strsplit(x, ";\\s*")[[1]]
      real_genes <- genes[!grepl(pseudo_pattern, genes, ignore.case = FALSE)]
      if (length(real_genes) > 0) {
        real_genes[1]
      } else {
        genes[1]
      }
    }
  ))
  return(result_table)
}


# Internal function to filter results to specific genes
.filter_genes <- function(result_table, gene_list) {
  if (nrow(result_table) == 0 || is.null(gene_list)) {
    return(result_table)
  }
  gene_list_upper <- toupper(gene_list)
  filtered <- result_table[
    sapply(result_table$gene_symbol, function(x) {
      if (is.na(x)) {
        return(FALSE)
      }
      genes <- strsplit(x, ";\\s*")[[1]]
      any(toupper(genes) %in% gene_list_upper)
    }),
  ]
  if (nrow(filtered) == 0) {
    message(
      "No variants found for the specified genes: ",
      paste(gene_list, collapse = ", ")
    )
  }
  return(filtered)
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

#' Read VEP File Header
#'
#' Read only the metadata section of a VEP output file.
#'
#' This is useful when you want to inspect VEP version, assembly, parsed field
#' definitions, or column layout before loading the full annotation table.
#'
#' @param file Path to the VEP output file. Can be uncompressed or gzipped.
#' 
#' @param format Character string specifying the output format. One of:
#' 
#'     - _tab_ = Default VEP tab-delimited output format
#' 
#'     - _vcf_ = VCF format output (produced with `--vcf` flag)
#' 
#' @param n_max Maximum number of lines to scan for header content. Default is
#'   1000, which should be sufficient for most VEP files.
#'
#' @return A named list containing:
#' 
#'     - *meta* = List of file metadata including format, VEP version, assembly, command line, and raw metadata lines
#' 
#'     - *columns* = Named character vector where names are column names and values are descriptions (if available)
#' 
#'     - *annotations* = Named list where names are annotation field names and values are their definitions from the header. For tab format these are Extra column fields; for VCF format these are CSQ fields (with NA values since VCF doesn't include descriptions). NULL if no annotations.}
#'
#' @details
#' VEP can output results in different formats depending on command-line flags.
#' This function supports the two most common modes:
#'
#' **Tab-delimited format** (default or `--tab`):
#' - Metadata lines begin with `##`
#' - Column header line begins with single `#`
#' - Annotation fields are described under "Extra column keys"
#'
#' **VCF format** (`--vcf`):
#' - Standard VCF headers with `##` metadata
#' - Consequence annotations are stored in `INFO/CSQ`
#' - CSQ field order is declared in `##INFO=<ID=CSQ,...>` header line
#'
#' The returned structure is consistent across formats. If no annotation fields
#' are found, `annotations` is returned as `NULL`.
#'
#' @examples
#' \dontrun{
#' # Read header from tab-delimited VEP output
#' header <- read_vep_header("variants.vep.txt", format = "tab")
#' names(header$columns)
#' names(header$annotations)
#'
#' # Get description for a specific annotation
#' header$annotations[["CLIN_SIG"]]
#'
#' # Read header from VCF format VEP output
#' header <- read_vep_header("variants.vep.vcf", format = "vcf")
#' }
#'
#' @seealso
#' \url{https://www.ensembl.org/info/docs/tools/vep/vep_formats.html} for VEP
#' output format documentation
#'
#' @export
read_vep_header <- function(file, format = c("tab", "vcf"), n_max = 1000) {

	# Validate inputs
	format <- match.arg(format)
	if (!is.numeric(n_max) || length(n_max) != 1 || is.na(n_max) || n_max < 1) {
		stop("'n_max' must be a positive number.", call. = FALSE)
	}
	n_max <- as.integer(n_max)

	if (!file.exists(file)) {
		stop("File not found: ", file, call. = FALSE)
	}

	con <- .open_vep_connection(file)
	on.exit(close(con), add = TRUE)

	metadata <- character()
	column_line <- NULL
	header_line_count <- 0L

	while (header_line_count < n_max) {
		line <- readLines(con, n = 1, warn = FALSE)

		# End of file
		if (length(line) == 0) {
			break
		}

		# Header lines always start with '#'. Stop when first data row appears.
		if (!startsWith(line, "#")) {
			break
		}
		header_line_count <- header_line_count + 1L

		if (startsWith(line, "##")) {
			metadata <- c(metadata, line)
		} else {
			# Single-# table header line.
			column_line <- line
		}
	}

	result <- if (format == "tab") {
		.parse_tab_header(metadata, column_line)
	} else {
		.parse_vcf_header(metadata, column_line)
	}

	result$meta$format <- format
	result$meta$n_header_lines <- header_line_count
	result
}


#' Parse Tab-Delimited VEP Header
#'
#' @param metadata Character vector of metadata lines
#' @param column_line The column header line
#' @return Parsed header list
#' @keywords internal
.parse_tab_header <- function(metadata, column_line) {

	meta <- .extract_meta_info(metadata)
	columns <- .extract_column_definitions(metadata, column_line)
	annotations <- .extract_annotation_definitions(metadata)

	list(
		meta = meta,
		columns = columns,
		annotations = annotations
	)
}


#' Parse VCF Format VEP Header
#'
#' @param metadata Character vector of metadata lines
#' @param column_line The column header line
#' @return Parsed header list
#' @keywords internal
.parse_vcf_header <- function(metadata, column_line) {

	meta <- .extract_meta_info(metadata)

	# VCF columns are fixed fields and do not include per-column descriptions.
	columns <- character()
	if (!is.null(column_line)) {
		col_names <- strsplit(sub("^#", "", column_line), "\t", fixed = TRUE)[[1]]
		columns <- stats::setNames(rep(NA_character_, length(col_names)), col_names)
	}

	annotations <- .extract_csq_fields(metadata)

	list(
		meta = meta,
		columns = columns,
		annotations = annotations
	)
}


#' Extract Meta Information from Header
#'
#' @param metadata Character vector of metadata lines
#' @return List of meta information
#' @keywords internal
.extract_meta_info <- function(metadata) {

	# Version can appear in tab output header line or in VCF ##VEP= metadata.
	version_line <- grep("ENSEMBL VARIANT EFFECT PREDICTOR", metadata, value = TRUE)
	vep_meta_line <- grep("^##VEP=", metadata, value = TRUE)
	vep_version <- if (length(version_line) > 0L) {
		sub(".*PREDICTOR\\s*(v?[0-9.]+).*", "\\1", version_line[1])
	} else if (length(vep_meta_line) > 0L) {
		sub('^##VEP="?([^" ]+).*$' , "\\1", vep_meta_line[1])
	} else {
		NA_character_
	}

	# Assembly can appear as "## assembly version ..." or within ##VEP="...".
	assembly_line <- grep("^## assembly", metadata, value = TRUE)
	assembly <- if (length(assembly_line) > 0L) {
		sub("^## assembly version\\s*", "", assembly_line[1])
	} else if (length(vep_meta_line) > 0L && grepl('assembly="', vep_meta_line[1])) {
		sub('.*assembly="([^"]+)".*', "\\1", vep_meta_line[1])
	} else {
		NA_character_
	}

	# Command line differs between tab and VCF outputs.
	cmd_line <- grep("VEP command-line:", metadata, value = TRUE)
	vcf_cmd_line <- grep("^##VEP-command-line=", metadata, value = TRUE)
	command <- if (length(cmd_line) > 0L) {
		sub("^## VEP command-line:\\s*", "", cmd_line[1])
	} else if (length(vcf_cmd_line) > 0L) {
		sub("^##VEP-command-line=['\"](.*)['\"]$", "\\1", vcf_cmd_line[1])
	} else {
		NA_character_
	}

	list(
		vep_version = vep_version,
		assembly = assembly,
		command = command,
		raw = metadata
	)
}


#' Extract Column Definitions from Tab Format
#'
#' @param metadata Character vector of metadata lines
#' @param column_line The column header line
#' @return Named character vector of column descriptions
#' @keywords internal
.extract_column_definitions <- function(metadata, column_line) {

	if (is.null(column_line)) {
		return(character())
	}

	col_names <- strsplit(sub("^#", "", column_line), "\t", fixed = TRUE)[[1]]

	desc_start <- grep("^## Column descriptions:", metadata)
	extra_start <- grep("^## Extra column keys:", metadata)

	if (length(desc_start) == 0) {
		return(stats::setNames(rep(NA_character_, length(col_names)), col_names))
	}

	end_idx <- if (length(extra_start) > 0L) extra_start[1] - 1L else length(metadata)
	desc_lines <- metadata[(desc_start[1] + 1L):end_idx]

	descriptions <- stats::setNames(rep(NA_character_, length(col_names)), col_names)

	for (line in desc_lines) {
		match <- regmatches(line, regexec("^## ([A-Za-z0-9_]+) : (.+)$", line))[[1]]
		if (length(match) == 3L) {
			name <- match[2]
			desc <- match[3]
			if (name %in% col_names) {
				descriptions[name] <- desc
			}
		}
	}

	descriptions
}


#' Extract Annotation Field Definitions from Tab Format
#'
#' @param metadata Character vector of metadata lines
#' @return Named list where names are annotation field names and values are
#'   their definitions, or NULL if no annotations found
#' @keywords internal
.extract_annotation_definitions <- function(metadata) {

	marker_idx <- grep("^## Extra column keys:", metadata)
	if (length(marker_idx) == 0L || marker_idx[1] >= length(metadata)) {
		return(NULL)
	}

	extra_lines <- metadata[(marker_idx[1] + 1L):length(metadata)]
	field_pattern <- "^## ([A-Za-z0-9_]+) : (.+)$"
	annotations <- list()

	for (line in extra_lines) {
		match <- regmatches(line, regexec(field_pattern, line))[[1]]
		if (length(match) == 3L) {
			annotations[[match[2]]] <- match[3]
		}
	}

	if (length(annotations) == 0L) {
		return(NULL)
	}

	annotations
}


#' Extract CSQ Fields from VCF INFO Header
#'
#' @param metadata Character vector of VCF metadata lines
#' @return Named list where names are CSQ field names and values are NA (VCF
#'   format doesn't include field descriptions), or NULL if not found
#' @keywords internal
.extract_csq_fields <- function(metadata) {

	csq_line <- grep("^##INFO=<ID=CSQ,", metadata, value = TRUE)
	if (length(csq_line) == 0L) {
		return(NULL)
	}
	csq_line <- csq_line[1]

	fields <- NULL
	format_match <- regmatches(csq_line, regexpr("Format:\\s*[^\">]+", csq_line))

	if (length(format_match) > 0L && nzchar(format_match)) {
		fields_string <- sub("^Format:\\s*", "", format_match)
		fields <- strsplit(fields_string, "|", fixed = TRUE)[[1]]
		fields <- trimws(fields)
		fields <- fields[fields != ""]
	} else {
		desc_match <- regmatches(csq_line, regexpr('Description="([^"]+)"', csq_line))
		if (length(desc_match) > 0L) {
			desc_content <- sub('Description="([^"]+)"', "\\1", desc_match)
			if (grepl("\\|", desc_content)) {
				parts <- strsplit(desc_content, "\\s+")[[1]]
				pipe_part <- parts[grepl("\\|", parts)]
				if (length(pipe_part) > 0L) {
					fields <- strsplit(pipe_part[length(pipe_part)], "|", fixed = TRUE)[[1]]
				}
			}
		}
	}

	if (is.null(fields) || length(fields) == 0L) {
		return(NULL)
	}

	stats::setNames(as.list(rep(NA_character_, length(fields))), fields)
}


#' Read VEP Output File
#'
#' Read a VEP output file into a tidy tibble.
#'
#' `read_vep_data()` parses tab-delimited and VCF-style VEP outputs and returns one
#' row per annotated consequence record. By default it expands `Extra` (tab) or
#' `CSQ` (VCF) into separate columns for easier downstream filtering.
#'
#' @param file Path to the VEP output file. Can be uncompressed or gzipped.
#' @param format Character string specifying the output format. One of:
#'   \describe{
#'     \item{"tab"}{Default VEP tab-delimited output format (default)}
#'     \item{"vcf"}{VCF format output (produced with `--vcf` flag)}
#'   }
#' @param columns Character vector of column names to include in the output.
#'   Default is NULL (returns all columns). Can reference both fixed VEP
#'   columns (e.g., "Gene", "Consequence") and Extra/CSQ fields (e.g.,
#'   "SYMBOL", "IMPACT", "SIFT", "LoF").
#' @param parse_extra Logical. Whether to parse the Extra column (tab) or CSQ
#'   field (VCF) into individual columns. Default is TRUE.
#'
#' @return A tibble with one row per variant-transcript annotation. If
#'   parse_extra is TRUE, the Extra/CSQ column is replaced by its parsed
#'   key-value pairs as individual columns. VEP dashes ("-") are replaced with
#'   NA. The returned tibble has the following attributes:
#'   \describe{
#'     \item{vep_header}{The complete header structure from read_vep_header()}
#'     \item{source_file}{Normalized path to the source VEP file}
#'   }
#'
#' @details
#' This function uses \code{\link{read_vep_header}} to infer structure and then
#' reads the data body in a single pass.
#'
#' **Tab-delimited format:**
#' - 14 columns: 13 fixed fields plus "Extra" column
#' - Extra contains semicolon-delimited key=value pairs
#' - Standalone flags are converted to `YES` (for example, `CANONICAL`)
#'
#' **VCF format:**
#' - Standard VCF columns with CSQ in INFO field
#' - CSQ contains pipe-delimited values ordered by the CSQ header definition
#'
#' The function automatically:
#' - Detects and handles gzipped files
#' - Skips metadata + header lines
#' - Replaces VEP dash placeholders ("-") with NA
#' - Preserves metadata for provenance tracking
#'
#' @examples
#' \dontrun{
#' # Read all columns from tab-delimited VEP output
#' df <- read_vep_data("sample.filtered")
#'
#' # Read only specific columns of interest
#' df <- read_vep_data(
#'   "sample.filtered",
#'   columns = c("Uploaded_variation", "Gene", "Consequence",
#'               "SYMBOL", "IMPACT", "SIFT", "PolyPhen", "LoF")
#' )
#'
#' # Read without parsing Extra (faster, returns raw Extra string)
#' df <- read_vep_data("sample.filtered", parse_extra = FALSE)
#'
#' # Access metadata
#' attr(df, "vep_header")$meta$vep_version
#' attr(df, "vep_header")$meta$assembly
#'
#' # Read gzipped VCF format
#' df <- read_vep_data("sample.vep.vcf.gz", format = "vcf")
#' }
#'
#' @seealso
#' \code{\link{read_vep_header}} for reading just the header information
#'
#' @export
read_vep_data <- function(file,
                     format = c("tab", "vcf"),
                     columns = NULL,
                     parse_extra = TRUE) {

	format <- match.arg(format)

	if (!file.exists(file)) {
		stop("File not found: ", file, call. = FALSE)
	}

	header <- read_vep_header(file, format = format, n_max = 1000)

	con <- .open_vep_connection(file)
	on.exit(close(con), add = TRUE)

	# Advance connection past metadata + table header lines.
	n_skip <- header$meta$n_header_lines
	if (!is.null(n_skip) && n_skip > 0L) {
		readLines(con, n = n_skip, warn = FALSE)
	}

	df <- if (format == "tab") {
		.read_tab_data(con, header, parse_extra)
	} else {
		.read_vcf_data(con, header, parse_extra)
	}

	# VEP commonly uses '-' as a placeholder for missing character values.
	char_cols <- vapply(df, is.character, logical(1))
	df[char_cols] <- lapply(df[char_cols], function(x) {
		x[x == "-"] <- NA_character_
		x
	})

	if (!is.null(columns)) {
		available <- names(df)
		missing <- setdiff(columns, available)
		keep <- intersect(columns, available)
		if (length(keep) == 0L) {
			stop("None of the requested columns found in the file.", call. = FALSE)
		}
		if (length(missing) > 0L) {
			warning(
				"Requested columns not found: ",
				paste(missing, collapse = ", "),
				"\nAvailable columns: ",
				paste(head(available, 10), collapse = ", "),
				if (length(available) > 10) "...",
				call. = FALSE
			)
		}
		df <- df[, keep, drop = FALSE]
	}

	df <- tibble::as_tibble(df)
	attr(df, "vep_header") <- header
	attr(df, "source_file") <- normalizePath(file, mustWork = FALSE)
	df
}


#' Read Tab-Delimited VEP Data (Internal)
#'
#' @param con Connection to read from (already positioned at data start)
#' @param header Header structure from read_vep_header
#' @param parse_extra Whether to parse Extra column
#' @return Data frame
#' @keywords internal
.read_tab_data <- function(con, header, parse_extra) {

	col_names <- names(header$columns)
	data_lines <- .read_data_lines(con)

	if (length(data_lines) == 0L) {
		warning("No data rows found in file", call. = FALSE)
		return(.empty_data_frame(col_names))
	}

	df <- .parse_tabular_data(data_lines, col_names)

	if (parse_extra && "Extra" %in% names(df)) {
		extra_parsed <- .parse_extra_column(df$Extra)
		df$Extra <- NULL
		df <- cbind(df, extra_parsed, stringsAsFactors = FALSE)
	}

	df
}


#' Read VCF Format VEP Data (Internal)
#'
#' @param con Connection to read from (already positioned at data start)
#' @param header Header structure from read_vep_header
#' @param parse_extra Whether to parse CSQ field
#' @return Data frame
#' @keywords internal
.read_vcf_data <- function(con, header, parse_extra) {

	col_names <- names(header$columns)
	data_lines <- .read_data_lines(con)

	if (length(data_lines) == 0L) {
		warning("No data rows found in file", call. = FALSE)
		return(.empty_data_frame(col_names))
	}

	df <- .parse_tabular_data(data_lines, col_names)

	if (parse_extra && "INFO" %in% names(df) && !is.null(header$annotations)) {
		csq_parsed <- .parse_csq_field(df$INFO, names(header$annotations))
		df <- cbind(df, csq_parsed, stringsAsFactors = FALSE)
	}

	df
}


#' Parse VEP Extra Column (Internal)
#'
#' Parses the semicolon-delimited Extra column into individual columns.
#' Handles key=value format where not all keys are present in every row.
#'
#' @param extra_col Character vector of Extra column values
#' @return Data frame with one column per unique key
#' @keywords internal
.parse_extra_column <- function(extra_col) {

	split_pairs <- strsplit(ifelse(is.na(extra_col), "", extra_col), ";", fixed = TRUE)

	parsed <- lapply(split_pairs, function(pairs) {
		pairs <- trimws(pairs)
		pairs <- pairs[nzchar(pairs)]
		if (length(pairs) == 0L) {
			return(stats::setNames(character(), character()))
		}

		eq_pos <- regexpr("=", pairs, fixed = TRUE)
		has_eq <- eq_pos > 0L

		keys <- character(length(pairs))
		vals <- character(length(pairs))

		keys[has_eq] <- substr(pairs[has_eq], 1, eq_pos[has_eq] - 1L)
		vals[has_eq] <- substring(pairs[has_eq], eq_pos[has_eq] + 1L)

		keys[!has_eq] <- pairs[!has_eq]
		vals[!has_eq] <- "YES"

		keys <- trimws(keys)
		valid <- nzchar(keys)
		keys <- keys[valid]
		vals <- vals[valid]

		stats::setNames(vals, keys)
	})

	all_keys <- unique(unlist(lapply(parsed, names), use.names = FALSE))
	if (length(all_keys) == 0L) {
		return(as.data.frame(matrix(nrow = length(extra_col), ncol = 0)))
	}

	out <- vector("list", length(all_keys))
	names(out) <- all_keys

	for (i in seq_along(all_keys)) {
		key <- all_keys[i]
		out[[i]] <- vapply(parsed, function(row) {
			idx <- match(key, names(row))
			if (!is.na(idx)) row[[idx]] else NA_character_
		}, character(1), USE.NAMES = FALSE)
	}

	as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE)
}


#' Parse VCF CSQ Field (Internal)
#'
#' Extracts and parses CSQ (Consequence) field from VCF INFO column.
#'
#' @param info_col Character vector of INFO column values
#' @param csq_fields Character vector of CSQ field names (from header)
#' @return Data frame with one column per CSQ field
#' @keywords internal
.parse_csq_field <- function(info_col, csq_fields) {

	if (length(csq_fields) == 0L) {
		return(as.data.frame(matrix(nrow = length(info_col), ncol = 0)))
	}

	# NOTE: If a VCF record contains multiple CSQ entries (comma-separated),
	# this parser currently keeps the raw first-level CSQ string and maps fields
	# positionally. This matches existing behavior and keeps parsing predictable.
	csq_pattern <- "CSQ=([^;]+)"
	safe_info <- ifelse(is.na(info_col), "", info_col)
	csq_matches <- regmatches(safe_info, regexec(csq_pattern, safe_info))

	csq_values <- vapply(csq_matches, function(m) {
		if (length(m) >= 2L) m[2] else NA_character_
	}, character(1))

	split_values <- strsplit(ifelse(is.na(csq_values), "", csq_values), "|", fixed = TRUE)

	out <- vector("list", length(csq_fields))
	names(out) <- csq_fields

	for (i in seq_along(csq_fields)) {
		out[[i]] <- vapply(split_values, function(vals) {
			if (length(vals) >= i && nzchar(vals[i])) vals[i] else NA_character_
		}, character(1), USE.NAMES = FALSE)
	}

	as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE)
}


#' Open VEP File Connection (Internal)
#'
#' @param file File path.
#' @return Open connection.
#' @keywords internal
.open_vep_connection <- function(file) {
	is_gzipped <- grepl("\\.(gz|gzip)$", file, ignore.case = TRUE)
	if (is_gzipped) {
		gzfile(file, open = "r")
	} else {
		base::file(file, open = "r")
	}
}


#' Read Non-Empty Data Lines (Internal)
#'
#' @param con Open file connection.
#' @return Character vector of non-empty lines.
#' @keywords internal
.read_data_lines <- function(con) {
	lines <- readLines(con, warn = FALSE)
	lines[nchar(lines) > 0]
}


#' Build Empty Data Frame with Named Columns (Internal)
#'
#' @param col_names Character vector of column names.
#' @return Empty data.frame.
#' @keywords internal
.empty_data_frame <- function(col_names) {
	df <- as.data.frame(
		matrix(nrow = 0, ncol = length(col_names)),
		stringsAsFactors = FALSE
	)
	names(df) <- col_names
	df
}


#' Parse Tabular Data Block (Internal)
#'
#' @param data_lines Character vector of data lines.
#' @param col_names Character vector of output column names.
#' @return Parsed data.frame.
#' @keywords internal
.parse_tabular_data <- function(data_lines, col_names) {
	text_con <- textConnection(data_lines)
	on.exit(close(text_con), add = TRUE)
	read.delim(
		text_con,
		header = FALSE,
		sep = "\t",
		col.names = col_names,
		stringsAsFactors = FALSE,
		quote = "",
		comment.char = "",
		check.names = FALSE
	)
}

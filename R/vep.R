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

	format <- match.arg(format)
	if (!is.numeric(n_max) || length(n_max) != 1 || is.na(n_max) || n_max < 1) {
		stop("'n_max' must be a positive number.", call. = FALSE)
	}
	n_max <- as.integer(n_max)

	if (!file.exists(file)) {
		stop("File not found: ", file, call. = FALSE)
	}

	# Open connection, handling gzipped files
	is_gzipped <- grepl("\\.(gz|gzip)$", file, ignore.case = TRUE)
	con <- if (is_gzipped) gzfile(file, open = "r") else base::file(file, open = "r")
	on.exit(close(con), add = TRUE)

	# Read all header lines (starting with '#'), separating metadata from column header
	metadata <- character()
	column_line <- NULL
	header_line_count <- 0L

	while (header_line_count < n_max) {
		line <- readLines(con, n = 1, warn = FALSE)
		if (length(line) == 0) break       # EOF
		if (!startsWith(line, "#")) break   # first data row

		header_line_count <- header_line_count + 1L

		if (startsWith(line, "##")) {
			metadata <- c(metadata, line)
		} else {
			column_line <- line
		}
	}

	# Parse metadata common to both formats
	meta <- .extract_meta_info(metadata)
	meta$format <- format
	meta$n_header_lines <- header_line_count

	# Parse format-specific column and annotation information
	if (format == "tab") {
		columns <- .extract_column_definitions(metadata, column_line)
		annotations <- .extract_annotation_definitions(metadata)
	} else {
		# VCF columns are fixed fields without per-column descriptions
		columns <- character()
		if (!is.null(column_line)) {
			col_names <- strsplit(sub("^#", "", column_line), "\t", fixed = TRUE)[[1]]
			columns <- stats::setNames(rep(NA_character_, length(col_names)), col_names)
		}
		annotations <- .extract_csq_fields(metadata)
	}

	list(meta = meta, columns = columns, annotations = annotations)
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

	# Open connection, handling gzipped files
	is_gzipped <- grepl("\\.(gz|gzip)$", file, ignore.case = TRUE)
	con <- if (is_gzipped) gzfile(file, open = "r") else base::file(file, open = "r")
	on.exit(close(con), add = TRUE)

	# Advance past header lines to reach data
	n_skip <- header$meta$n_header_lines
	if (!is.null(n_skip) && n_skip > 0L) {
		readLines(con, n = n_skip, warn = FALSE)
	}

	# Read remaining data lines
	data_lines <- readLines(con, warn = FALSE)
	data_lines <- data_lines[nchar(data_lines) > 0]

	col_names <- names(header$columns)

	# Handle empty files
	if (length(data_lines) == 0L) {
		message("No data rows found in file")
		df <- stats::setNames(
			as.data.frame(
				lapply(col_names, function(x) NA_character_),
				stringsAsFactors = FALSE
			),
			col_names
		)
	} else {
		# Parse tab-delimited data body
		text_con <- textConnection(data_lines)
		on.exit(close(text_con), add = TRUE)
		df <- read.delim(
			text_con,
			header = FALSE,
			sep = "\t",
			col.names = col_names,
			stringsAsFactors = FALSE,
			quote = "",
			comment.char = "",
			check.names = FALSE
		)

		# Expand annotation fields into separate columns
		if (format == "tab") {
			if (parse_extra && "Extra" %in% names(df)) {
				extra_parsed <- .parse_extra_column(df$Extra)
				df$Extra <- NULL
				df <- cbind(df, extra_parsed, stringsAsFactors = FALSE)
			}
		} else {
			if (parse_extra && "INFO" %in% names(df) && !is.null(header$annotations)) {
				csq_parsed <- .parse_csq_field(df$INFO, names(header$annotations))
				df <- cbind(df, csq_parsed, stringsAsFactors = FALSE)
			}
		}
	}

	# Replace VEP dash placeholders with NA
	char_cols <- vapply(df, is.character, logical(1))
	df[char_cols] <- lapply(df[char_cols], function(x) {
		x[x == "-"] <- NA_character_
		x
	})

	# Subset to requested columns
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


# Internal helpers ---------------------------------------------------------
# These functions encapsulate substantive parsing logic and are kept separate
# for clarity. Each is called from the exported functions above.


#' Extract Meta Information from VEP Header
#'
#' Parses VEP version, genome assembly, and command line from header metadata.
#' Works for both tab-delimited and VCF format headers.
#'
#' @param metadata Character vector of `##`-prefixed metadata lines.
#' @return Named list with elements `vep_version`, `assembly`, `command`, and
#'   `raw` (the original metadata lines).
#' @keywords internal
.extract_meta_info <- function(metadata) {

	# Version: tab format uses "ENSEMBL VARIANT EFFECT PREDICTOR", VCF uses "##VEP="
	version_line <- grep("ENSEMBL VARIANT EFFECT PREDICTOR", metadata, value = TRUE)
	vep_meta_line <- grep("^##VEP=", metadata, value = TRUE)
	vep_version <- if (length(version_line) > 0L) {
		sub(".*PREDICTOR\\s*(v?[0-9.]+).*", "\\1", version_line[1])
	} else if (length(vep_meta_line) > 0L) {
		sub('^##VEP="?([^" ]+).*$' , "\\1", vep_meta_line[1])
	} else {
		NA_character_
	}

	# Assembly: tab uses "## assembly version ...", VCF embeds in ##VEP line
	assembly_line <- grep("^## assembly", metadata, value = TRUE)
	assembly <- if (length(assembly_line) > 0L) {
		sub("^## assembly version\\s*", "", assembly_line[1])
	} else if (length(vep_meta_line) > 0L && grepl('assembly="', vep_meta_line[1])) {
		sub('.*assembly="([^"]+)".*', "\\1", vep_meta_line[1])
	} else {
		NA_character_
	}

	# Command line: tab uses "VEP command-line:", VCF uses "##VEP-command-line="
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


#' Extract Column Definitions from Tab Format Header
#'
#' Parses the "Column descriptions" section of a tab-format VEP header to
#' produce a named character vector mapping column names to their descriptions.
#'
#' @param metadata Character vector of `##`-prefixed metadata lines.
#' @param column_line The single-`#` column header line, or NULL if absent.
#' @return Named character vector where names are column names and values are
#'   descriptions (NA if no description found for a column).
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


#' Extract Annotation Field Definitions from Tab Format Header
#'
#' Parses the "Extra column keys" section of a tab-format VEP header to extract
#' annotation field names and their descriptions.
#'
#' @param metadata Character vector of `##`-prefixed metadata lines.
#' @return Named list where names are annotation field names and values are
#'   their descriptions, or NULL if no annotations found.
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
#' Parses the `##INFO=<ID=CSQ,...>` line from a VCF header to extract the
#' ordered list of consequence annotation field names.
#'
#' @param metadata Character vector of VCF `##`-prefixed metadata lines.
#' @return Named list where names are CSQ field names and values are NA (VCF
#'   format doesn't include field descriptions), or NULL if no CSQ header found.
#' @keywords internal
.extract_csq_fields <- function(metadata) {

	csq_line <- grep("^##INFO=<ID=CSQ,", metadata, value = TRUE)
	if (length(csq_line) == 0L) {
		return(NULL)
	}
	csq_line <- csq_line[1]

	# Try "Format: field1|field2|..." first (standard VEP CSQ header)
	fields <- NULL
	format_match <- regmatches(csq_line, regexpr("Format:\\s*[^\">]+", csq_line))

	if (length(format_match) > 0L && nzchar(format_match)) {
		fields_string <- sub("^Format:\\s*", "", format_match)
		fields <- strsplit(fields_string, "|", fixed = TRUE)[[1]]
		fields <- trimws(fields)
		fields <- fields[fields != ""]
	} else {
		# Fallback: extract pipe-delimited fields from Description
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


#' Parse VEP Extra Column
#'
#' Parses the semicolon-delimited Extra column from tab-format VEP output into
#' individual columns. Handles `key=value` pairs and standalone flags (converted
#' to `"YES"`). Not all keys are present in every row.
#'
#' @param extra_col Character vector of Extra column values.
#' @return Data frame with one column per unique key found across all rows.
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

		# Standalone flags (no '=') are recorded as "YES"
		keys[!has_eq] <- pairs[!has_eq]
		vals[!has_eq] <- "YES"

		keys <- trimws(keys)
		valid <- nzchar(keys)
		stats::setNames(vals[valid], keys[valid])
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


#' Parse VCF CSQ Field
#'
#' Extracts and parses the CSQ (Consequence) annotation from the VCF INFO
#' column. Values are pipe-delimited and mapped positionally to field names
#' from the CSQ header.
#'
#' @param info_col Character vector of INFO column values.
#' @param csq_fields Character vector of CSQ field names (from header).
#' @return Data frame with one column per CSQ field.
#'
#' @note If a VCF record contains multiple CSQ entries (comma-separated), only
#'   the first entry is parsed. This keeps behavior predictable for single-
#'   transcript workflows.
#' @keywords internal
.parse_csq_field <- function(info_col, csq_fields) {

	if (length(csq_fields) == 0L) {
		return(as.data.frame(matrix(nrow = length(info_col), ncol = 0)))
	}

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

#' Read VEP File Header
#'
#' Efficiently reads only the header portion of Variant Effect Predictor (VEP)
#' output files without loading the entire file into memory.
#'
#' @param file Path to the VEP output file. Can be uncompressed or gzipped.
#' @param format Character string specifying the output format. One of:
#'   \describe{
#'     \item{"tab"}{Default VEP tab-delimited output format}
#'     \item{"vcf"}{VCF format output (produced with `--vcf` flag)}
#'   }
#' @param n_max Maximum number of lines to scan for header content. Default is
#'   1000, which should be sufficient for most VEP files.
#'
#' @return A named list containing:
#'   \describe{
#'     \item{meta}{List of file metadata including format, VEP version, assembly,
#'       command line, and raw metadata lines}
#'     \item{columns}{Named character vector where names are column names and
#'       values are descriptions (if available)}
#'     \item{annotations}{Named list where names are annotation field names and
#'       values are their definitions from the header. For tab format these are
#'       Extra column fields; for VCF format these are CSQ fields (with NA values
#'       since VCF doesn't include descriptions). NULL if no annotations.}
#'   }
#'
#' @details
#' VEP can output results in different formats depending on command-line flags:
#'
#' **Tab-delimited format** (default or `--tab`):
#' - Metadata lines begin with `##`
#' - Column header line begins with single `#`
#' - Annotation fields stored in "Extra" column as key=value pairs
#'
#' **VCF format** (`--vcf`):
#' - Standard VCF headers with `##` metadata
#' - CSQ (consequence) annotations stored in INFO field
#' - Field order defined in `##INFO=<ID=CSQ,...>` header line
#'
#' The returned structure is consistent across formats, with `annotations` being
#' NULL for unannotated files.
#'
#' @examples
#' \dontrun{
#' # Read header from tab-delimited VEP output
#' header <- read_vep_header("variants.vep.txt", format = "tab")
#' names(header$columns)
#' names(header$annotations)
#'
#' # Get description for a specific annotation
#' header$annotations["CLIN_SIG"]
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

	if (!file.exists(file)) {
		stop("File not found: ", file, call. = FALSE)
	}

	# Determine if file is gzipped
	is_gzipped <- grepl("\\.(gz|gzip)$", file, ignore.case = TRUE)

	# Open connection appropriately
	if (is_gzipped) {
		con <- gzfile(file, open = "r")
	} else {
		con <- file(file, open = "r")
	}
	on.exit(close(con), add = TRUE)

	# Read header lines
	metadata <- character()
	column_line <- NULL
	line_count <- 0

	while (line_count < n_max) {
		line <- readLines(con, n = 1, warn = FALSE)

		# Check for end of file
		if (length(line) == 0) {
			break
		}

		line_count <- line_count + 1

		# Header lines start with #
		if (!startsWith(line, "#")) {
			break
		}

		# Metadata lines start with ##
		if (startsWith(line, "##")) {
			metadata <- c(metadata, line)
		} else if (startsWith(line, "#")) {
			# Column header line (single #)
			column_line <- line
		}
	}

	# Build result based on format
	if (format == "tab") {
		result <- .parse_tab_header(metadata, column_line)
	} else {
		result <- .parse_vcf_header(metadata, column_line)
	}

	result$meta$format <- format
	result
}


#' Parse Tab-Delimited VEP Header
#'
#' @param metadata Character vector of metadata lines
#' @param column_line The column header line
#' @return Parsed header list
#' @keywords internal
.parse_tab_header <- function(metadata, column_line) {

	# Extract meta information
	meta <- .extract_meta_info(metadata)

	# Extract column names and descriptions
	columns <- .extract_column_definitions(metadata, column_line)

	# Extract annotation field definitions
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

	# Extract meta information
	meta <- .extract_meta_info(metadata)

	# Extract column names (VCF standard columns don't have descriptions)
	columns <- character()
	if (!is.null(column_line)) {
		col_names <- strsplit(sub("^#", "", column_line), "\t")[[1]]
		columns <- stats::setNames(rep(NA_character_, length(col_names)), col_names)
	}

	# Extract CSQ annotation fields
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

	# VEP version
	version_line <- grep("ENSEMBL VARIANT EFFECT PREDICTOR", metadata, value = TRUE)
	vep_version <- if (length(version_line) > 0) {
		sub(".*PREDICTOR\\s*(v?[0-9.]+).*", "\\1", version_line[1])
	} else {
		NA_character_
	}

	# Assembly
	assembly_line <- grep("^## assembly", metadata, value = TRUE)
	assembly <- if (length(assembly_line) > 0) {
		sub("^## assembly version\\s*", "", assembly_line[1])
	} else {
		NA_character_
	}

	# Command line
	cmd_line <- grep("VEP command-line:", metadata, value = TRUE)
	command <- if (length(cmd_line) > 0) {
		sub("^## VEP command-line:\\s*", "", cmd_line[1])
	} else {
		NA_character_
	}

	list(
		vep_version = vep_version,
		assembly = assembly,
		command = command,
		n_header_lines = length(metadata) + 1,
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

	# Get column names from header line
	if (is.null(column_line)) {
		return(character())
	}

	col_names <- strsplit(sub("^#", "", column_line), "\t")[[1]]

	# Find column description section (between header start and "Extra column keys")
	desc_start <- grep("^## Column descriptions:", metadata)
	extra_start <- grep("^## Extra column keys:", metadata)

	if (length(desc_start) == 0) {
		# No descriptions, return names with NA values
		return(stats::setNames(rep(NA_character_, length(col_names)), col_names))
	}

	# Get description lines
	end_idx <- if (length(extra_start) > 0) extra_start[1] - 1 else length(metadata)
	desc_lines <- metadata[(desc_start[1] + 1):end_idx]

	# Parse descriptions: "## Column_name : description"
	descriptions <- stats::setNames(rep(NA_character_, length(col_names)), col_names)

	for (line in desc_lines) {
		match <- regmatches(line, regexec("^## ([A-Za-z0-9_]+) : (.+)$", line))[[1]]
		if (length(match) == 3) {
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

	# Find "Extra column keys:" marker
	marker_idx <- grep("^## Extra column keys:", metadata)

	if (length(marker_idx) == 0) {
		return(NULL)
	}

	# Get lines after the marker
	extra_lines <- metadata[(marker_idx[1] + 1):length(metadata)]

	# Parse field definitions: "## FIELD_NAME : description"
	# Exclude lines like "## VEP command-line:" which don't follow the pattern
	field_pattern <- "^## ([A-Za-z0-9_]+) : (.+)$"

	annotations <- list()

	for (line in extra_lines) {
		match <- regmatches(line, regexec(field_pattern, line))[[1]]
		if (length(match) == 3) {
			name <- match[2]
			desc <- match[3]
			# Skip if it looks like a special line (contains "command" or similar)
			if (!grepl("command", name, ignore.case = TRUE)) {
				annotations[[name]] <- desc
			}
		}
	}

	if (length(annotations) == 0) {
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

	# Find the CSQ INFO line
	csq_pattern <- "^##INFO=<ID=CSQ,"
	csq_line <- grep(csq_pattern, metadata, value = TRUE)

	if (length(csq_line) == 0) {
		return(NULL)
	}

	# Use the first match if multiple exist
	csq_line <- csq_line[1]

	# Extract the Format: portion from the Description
	format_match <- regmatches(
		csq_line,
		regexpr("Format:\\s*([^\"]+)", csq_line)
	)

	fields <- NULL

	if (length(format_match) > 0 && format_match != "") {
		# Remove "Format: " prefix and split by pipe
		fields_string <- sub("^Format:\\s*", "", format_match)
		fields <- strsplit(fields_string, "\\|")[[1]]
		fields <- trimws(fields)
		fields <- fields[fields != ""]
	} else {
		# Try alternative pattern - look for pipe-separated fields in Description
		desc_match <- regmatches(
			csq_line,
			regexpr('Description="([^"]+)"', csq_line)
		)

		if (length(desc_match) > 0) {
			desc_content <- sub('Description="([^"]+)"', "\\1", desc_match)
			if (grepl("\\|", desc_content)) {
				parts <- strsplit(desc_content, "\\s+")[[1]]
				pipe_part <- parts[grepl("\\|", parts)]
				if (length(pipe_part) > 0) {
					fields <- strsplit(pipe_part[length(pipe_part)], "\\|")[[1]]
				}
			}
		}
	}

	if (is.null(fields) || length(fields) == 0) {
		return(NULL)
	}

	# Return as named list with NA descriptions (VCF doesn't include them)
	stats::setNames(as.list(rep(NA_character_, length(fields))), fields)
}

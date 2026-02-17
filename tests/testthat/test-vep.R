test_that("read_vep_header returns correct structure for tab format", {
	sample_file <- test_path("sample.vep.filtered")
	header <- read_vep_header(sample_file, format = "tab")

	# Check top-level structure
	expect_type(header, "list")
	expect_named(header, c("meta", "columns", "annotations"))

	# Check meta structure
	expect_type(header$meta, "list")
	expect_true("format" %in% names(header$meta))
	expect_true("vep_version" %in% names(header$meta))
	expect_true("assembly" %in% names(header$meta))
	expect_true("command" %in% names(header$meta))
	expect_true("raw" %in% names(header$meta))

	# Check format
	expect_equal(header$meta$format, "tab")
})


test_that("read_vep_header extracts meta information", {
	sample_file <- test_path("sample.vep.filtered")
	header <- read_vep_header(sample_file, format = "tab")

	# VEP version should be extracted
	expect_equal(header$meta$vep_version, "v115.2")

	# Assembly should be extracted
	expect_equal(header$meta$assembly, "GRCh38.p14")

	# Command should be extracted
	expect_true(grepl("^vep", header$meta$command))
	expect_true(grepl("--everything", header$meta$command))

	# Raw metadata should be preserved
	expect_true(length(header$meta$raw) > 100)
	expect_true(all(startsWith(header$meta$raw, "##")))
})


test_that("read_vep_header extracts column definitions with descriptions", {
	sample_file <- test_path("sample.vep.filtered")
	header <- read_vep_header(sample_file, format = "tab")

	# Check column names
	expect_equal(
		names(header$columns),
		c("Uploaded_variation", "Location", "Allele", "Gene", "Feature",
			"Feature_type", "Consequence", "cDNA_position", "CDS_position",
			"Protein_position", "Amino_acids", "Codons", "Existing_variation", "Extra")
	)

	# Check that descriptions are included
	expect_equal(
		header$columns["Uploaded_variation"],
		c(Uploaded_variation = "Identifier of uploaded variant")
	)
	expect_equal(
		header$columns["Location"],
		c(Location = "Location of variant in standard coordinate format (chr:start or chr:start-end)")
	)
	expect_equal(
		header$columns["Consequence"],
		c(Consequence = "Consequence type")
	)
})


test_that("read_vep_header extracts annotation definitions as a list", {
	sample_file <- test_path("sample.vep.filtered")
	header <- read_vep_header(sample_file, format = "tab")

	# Annotations should be a list
	expect_type(header$annotations, "list")
	expect_true(length(header$annotations) > 0)

	# Check specific annotations are present
	expect_true("CLIN_SIG" %in% names(header$annotations))
	expect_true("IMPACT" %in% names(header$annotations))
	expect_true("SYMBOL" %in% names(header$annotations))
	expect_true("gnomADe_AF" %in% names(header$annotations))
	expect_true("LoF" %in% names(header$annotations))

	# Check definitions are correct (accessed via list indexing)
	expect_equal(
		header$annotations[["CLIN_SIG"]],
		"ClinVar clinical significance of the dbSNP variant"
	)
	expect_equal(
		header$annotations[["IMPACT"]],
		"Subjective impact classification of consequence type"
	)
	expect_equal(
		header$annotations[["LoF"]],
		"Loss-of-function annotation (HC = High Confidence; LC = Low Confidence)"
	)

	# VEP command-line should NOT be extracted as a field
	expect_false("VEP" %in% names(header$annotations))
})


test_that("read_vep_header errors on missing file", {
	expect_error(
		read_vep_header("nonexistent_file.vep"),
		"File not found"
	)
})


test_that("read_vep_header validates format argument", {
	sample_file <- test_path("sample.vep.filtered")

	expect_error(
		read_vep_header(sample_file, format = "invalid"),
		"'arg' should be one of"
	)
})


# read_vep() tests --------------------------------------------------------

test_that("read_vep returns a tibble with correct structure", {
	sample_file <- test_path("sample.vep.filtered")
	df <- read_vep_data(sample_file, format = "tab")

	# Should be a tibble
	expect_s3_class(df, "tbl_df")

	# Should have rows
	expect_gt(nrow(df), 0)

	# Should have columns
	expect_gt(ncol(df), 0)

	# Should have attributes
	expect_true(!is.null(attr(df, "vep_header")))
	expect_true(!is.null(attr(df, "source_file")))
})


test_that("read_vep_data parses Extra column into individual columns", {
	sample_file <- test_path("sample.vep.filtered")
	df <- read_vep_data(sample_file, format = "tab", parse_extra = TRUE)

	# Extra column should be removed
	expect_false("Extra" %in% names(df))

	# Should have parsed annotation columns
	expect_true("SYMBOL" %in% names(df))
	expect_true("IMPACT" %in% names(df))
	expect_true("REF_ALLELE" %in% names(df))
	expect_true("VARIANT_CLASS" %in% names(df))

	# Check some values
	expect_true(all(!is.na(df$SYMBOL)))
	expect_true("PRDM16" %in% df$SYMBOL)
})


test_that("read_vep_data with parse_extra = FALSE keeps Extra column", {
	sample_file <- test_path("sample.vep.filtered")
	df <- read_vep_data(sample_file, format = "tab", parse_extra = FALSE)

	# Extra column should still be present
	expect_true("Extra" %in% names(df))

	# Extra should be character
	expect_type(df$Extra, "character")

	# Extra should contain semicolon-delimited data
	expect_true(grepl(";", df$Extra[1]))
})


test_that("read_vep_data replaces dashes with NA", {
	sample_file <- test_path("sample.vep.filtered")
	df <- read_vep_data(sample_file, format = "tab")

	# Character columns that were "-" should now be NA
	# Check cDNA_position which often has "-" for non-coding
	if ("cDNA_position" %in% names(df)) {
		expect_true(any(is.na(df$cDNA_position)))
		expect_false(any(df$cDNA_position == "-", na.rm = TRUE))
	}

	# Check Protein_position
	if ("Protein_position" %in% names(df)) {
		expect_true(any(is.na(df$Protein_position)))
		expect_false(any(df$Protein_position == "-", na.rm = TRUE))
	}
})


test_that("read_vep_data column selection works", {
	sample_file <- test_path("sample.vep.filtered")

	# Select only specific columns
	cols <- c("Uploaded_variation", "Gene", "SYMBOL", "IMPACT")
	df <- read_vep_data(sample_file, format = "tab", columns = cols)

	# Should only have the requested columns
	expect_equal(sort(names(df)), sort(cols))

	# Should still have data
	expect_gt(nrow(df), 0)
})


test_that("read_vep_data column selection warns on missing columns", {
	sample_file <- test_path("sample.vep.filtered")

	# Request some valid and some invalid columns
	cols <- c("Gene", "SYMBOL", "NONEXISTENT_COLUMN", "ANOTHER_FAKE")

	expect_warning(
		df <- read_vep_data(sample_file, format = "tab", columns = cols),
		"Requested columns not found"
	)

	# Should still return the valid columns
	expect_true("Gene" %in% names(df))
	expect_true("SYMBOL" %in% names(df))
	expect_false("NONEXISTENT_COLUMN" %in% names(df))
})


test_that("read_vep_data errors when no requested columns found", {
	sample_file <- test_path("sample.vep.filtered")

	expect_error(
		read_vep_data(sample_file, format = "tab", columns = c("FAKE1", "FAKE2")),
		"None of the requested columns found"
	)
})


test_that("read_vep_data_data preserves header metadata", {
	sample_file <- test_path("sample.vep.filtered")
	df <- read_vep_data(sample_file, format = "tab")

	header <- attr(df, "vep_header")

	# Should have header structure
	expect_type(header, "list")
	expect_named(header, c("meta", "columns", "annotations"))

	# Meta should have expected fields
	expect_equal(header$meta$vep_version, "v115.2")
	expect_equal(header$meta$assembly, "GRCh38.p14")
	expect_equal(header$meta$format, "tab")
})


test_that("read_vep_data handles annotation fields correctly", {
	sample_file <- test_path("sample.vep.filtered")
	df <- read_vep_data(sample_file, format = "tab")

	# Check for common annotation fields
	expect_true("IMPACT" %in% names(df))
	expect_true("SYMBOL" %in% names(df))
	expect_true("BIOTYPE" %in% names(df))

	# IMPACT should have valid values
	expect_true(all(df$IMPACT %in% c("HIGH", "MODERATE", "LOW", "MODIFIER", NA)))

	# Check for allele frequency fields
	if ("gnomADe_AF" %in% names(df)) {
		# Should be character (not yet converted to numeric)
		expect_type(df$gnomADe_AF, "character")
	}
})


test_that("read_vep_data handles LoF annotations", {
	sample_file <- test_path("sample.vep.filtered")
	df <- read_vep_data(sample_file, format = "tab")

	# Check for LoF-related columns
	expected_lof_cols <- c("LoF", "LoF_filter", "LoF_flags", "LoF_info")
	present_lof_cols <- intersect(expected_lof_cols, names(df))

	# At least LoF should be present (from header)
	expect_true("LoF" %in% names(df))
})


test_that("empty VEP file returns 1-row NA tibble with message", {
	sample_file <- test_path("sample.vep.filtered")

	# Read a populated file
	df_full <- read_vep_data(sample_file, format = "tab", parse_extra = FALSE)

	# Create a temp file with only header lines (no data rows)
	lines <- readLines(sample_file)
	header_lines <- lines[grepl("^##|^#", lines)]
	tmp <- tempfile(fileext = ".vep")
	writeLines(header_lines, tmp)
	on.exit(unlink(tmp))

	# Read the empty file — should message and return 1-row NA tibble
	expect_message(
		df_empty <- read_vep_data(tmp, format = "tab", parse_extra = FALSE),
		"No data rows found"
	)

	expect_s3_class(df_empty, "tbl_df")
	expect_equal(nrow(df_empty), 1)

	# All columns should be character with NA values
	col_types <- vapply(df_empty, typeof, character(1))
	expect_true(all(col_types == "character"))
	expect_true(all(is.na(df_empty[1, ])))

	# bind_rows should work without error
	combined <- dplyr::bind_rows(df_full, df_empty)
	expect_equal(nrow(combined), nrow(df_full) + 1)
})


test_that("read_vep_data errors on missing file", {
	expect_error(
		read_vep_data("nonexistent_file.vep"),
		"File not found"
	)
})


test_that("read_vep_data validates format argument", {
	sample_file <- test_path("sample.vep.filtered")

	expect_error(
		read_vep_data(sample_file, format = "invalid"),
		"'arg' should be one of"
	)
})


test_that("read_vep_data handles flags without values in Extra column", {
	sample_file <- test_path("sample.vep.filtered")
	df <- read_vep_data(sample_file, format = "tab")

	# CANONICAL is often a flag (present/absent)
	# When present, it should be converted to "YES"
	if ("CANONICAL" %in% names(df)) {
		canonical_values <- unique(df$CANONICAL)
		canonical_values <- canonical_values[!is.na(canonical_values)]
		# Should only have "YES" or NA (flags are converted to "YES")
		expect_true(all(canonical_values == "YES"))
	}
})


test_that("read_vep_data preserves source file path", {
	sample_file <- test_path("sample.vep.filtered")
	df <- read_vep_data(sample_file, format = "tab")

	source <- attr(df, "source_file")
	expect_type(source, "character")
	expect_true(file.exists(source))
	expect_true(grepl("sample\\.vep\\.filtered", source))
})

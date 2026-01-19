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

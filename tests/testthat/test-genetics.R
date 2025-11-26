# Tests for genetic variant query functions ----------------------------

test_that("query_genetic_variants validates all parameters", {
  # Phenotype: must be non-empty character string
  expect_error(
    query_genetic_variants(123),
    "'phenotype' must be a non-empty character string"
  )
  expect_error(
    query_genetic_variants(c("condition1", "condition2")),
    "'phenotype' must be a non-empty character string"
  )
  expect_error(
    query_genetic_variants(""),
    "'phenotype' must be a non-empty character string"
  )

  # Database: must be supported single character string
  expect_error(
    query_genetic_variants("atrial fibrillation", database = 123),
    "'database' must be a single character string"
  )
  expect_error(
    query_genetic_variants("atrial fibrillation", database = c("clinvar", "gnomad")),
    "'database' must be a single character string"
  )
  expect_error(
    query_genetic_variants("atrial fibrillation", database = "unsupported_db"),
    "Unsupported database.*Currently only 'clinvar' is supported"
  )

  # API key: must be NULL or single character string
  expect_error(
    query_genetic_variants("atrial fibrillation", api_key = 123),
    "'api_key' must be NULL or a single character string"
  )
  expect_error(
    query_genetic_variants("atrial fibrillation", api_key = c("key1", "key2")),
    "'api_key' must be NULL or a single character string"
  )

  # Max results: must be single number between 1 and 10,000
  expect_error(
    query_genetic_variants("atrial fibrillation", max_results = "100"),
    "'max_results' must be a single number between 1 and 10,000"
  )
  expect_error(
    query_genetic_variants("atrial fibrillation", max_results = c(10, 20)),
    "'max_results' must be a single number between 1 and 10,000"
  )
  expect_error(
    query_genetic_variants("atrial fibrillation", max_results = 0),
    "'max_results' must be a single number between 1 and 10,000"
  )
  expect_error(
    query_genetic_variants("atrial fibrillation", max_results = 10001),
    "'max_results' must be a single number between 1 and 10,000"
  )

  # Genes: must be NULL or character vector
  expect_error(
    query_genetic_variants("atrial fibrillation", genes = 123),
    "'genes' must be NULL or a character vector"
  )

  # Clean gene symbols: must be single logical value
  expect_error(
    query_genetic_variants("atrial fibrillation", clean_gene_symbols = "yes"),
    "'clean_gene_symbols' must be a single logical value"
  )
  expect_error(
    query_genetic_variants("atrial fibrillation", clean_gene_symbols = c(TRUE, FALSE)),
    "'clean_gene_symbols' must be a single logical value"
  )
})


## Return structure ----

test_that("query_genetic_variants returns tibble with correct structure", {
  skip_if_offline()
  skip_on_cran()

  result <- query_genetic_variants("Brugada syndrome", max_results = 10)

  expect_s3_class(result, "tbl_df")

  expected_cols <- c(
    "gene_symbol",
    "variant_id",
    "variant_name",
    "chromosome",
    "position",
    "clinical_significance",
    "review_status",
    "phenotypes",
    "molecular_consequence",
    "database"
  )

  expect_true(all(expected_cols %in% names(result)))
})


## Core functionality ----

test_that("query_genetic_variants returns results for known phenotype", {
  skip_if_offline()
  skip_on_cran()

  result <- query_genetic_variants("hypertrophic cardiomyopathy", max_results = 20)

  expect_true(nrow(result) > 0)
  expect_true(all(result$database == "ClinVar"))
  expect_true(any(!is.na(result$gene_symbol)))
})


test_that("query_genetic_variants respects max_results parameter", {
  skip_if_offline()
  skip_on_cran()

  result <- query_genetic_variants("atrial fibrillation", max_results = 15)
  expect_true(nrow(result) <= 15)
})


test_that("query_genetic_variants handles non-existent phenotype gracefully", {
  skip_if_offline()
  skip_on_cran()

  expect_message(
    result <- query_genetic_variants("xyzabc123nonexistent999", max_results = 10),
    "No variants found for phenotype"
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)

  expected_cols <- c(
    "gene_symbol", "variant_id", "variant_name", "chromosome", "position",
    "clinical_significance", "review_status", "phenotypes",
    "molecular_consequence", "database"
  )
  expect_true(all(expected_cols %in% names(result)))
})


test_that("query_genetic_variants is case-insensitive for database parameter", {
  skip_if_offline()
  skip_on_cran()

  r1 <- query_genetic_variants("arrhythmia", database = "clinvar", max_results = 5)
  r2 <- query_genetic_variants("arrhythmia", database = "CLINVAR", max_results = 5)

  expect_s3_class(r1, "tbl_df")
  expect_s3_class(r2, "tbl_df")
})


## Integration test ----

test_that("integration test: familial hypercholesterolemia workflow", {
  skip_if_offline()
  skip_on_cran()

  result <- query_genetic_variants(
    "familial hypercholesterolemia",
    database = "clinvar",
    max_results = 30
  )

  # Should find variants
  expect_true(nrow(result) > 0)

  # Should have LDLR gene (most common for this condition)
  expect_true(any(grepl("LDLR", result$gene_symbol, ignore.case = TRUE)))

  # Should have clinical significance and phenotype data
  expect_true(any(!is.na(result$clinical_significance)))
  expect_true(any(!is.na(result$phenotypes)))

  # Variant IDs should be unique
  expect_equal(length(unique(result$variant_id)), nrow(result))
})


## Helper functions ----

test_that("empty result table has correct structure", {
  empty_table <- card:::.empty_result_table()

  expect_s3_class(empty_table, "tbl_df")
  expect_equal(nrow(empty_table), 0)

  expected_cols <- c(
    "gene_symbol", "variant_id", "variant_name", "chromosome", "position",
    "clinical_significance", "review_status", "phenotypes",
    "molecular_consequence", "database"
  )
  expect_true(all(expected_cols %in% names(empty_table)))
})


test_that("clean_gene_symbols extracts real genes from mixed entries", {
  test_data <- tibble::tibble(
    gene_symbol = c("BRCA1", "LOC123456", "LOC123456; TP53", "TP53; LOC999", "BRCA1; BRCA2"),
    variant_id = as.character(1:5),
    variant_name = rep("test", 5),
    chromosome = rep("1", 5),
    position = 1:5,
    clinical_significance = rep(NA_character_, 5),
    review_status = rep(NA_character_, 5),
    phenotypes = rep(NA_character_, 5),
    molecular_consequence = rep(NA_character_, 5),
    database = rep("ClinVar", 5)
  )

  cleaned <- card:::.clean_gene_symbols(test_data)

  expect_equal(nrow(cleaned), 5)
  # Real genes preserved
  expect_equal(cleaned$gene_symbol[1], "BRCA1")
  # Pure pseudogenes kept conservatively
  expect_equal(cleaned$gene_symbol[2], "LOC123456")
  # Real gene extracted from mixed entries
  expect_equal(cleaned$gene_symbol[3], "TP53")
  expect_equal(cleaned$gene_symbol[4], "TP53")
  # First real gene kept from multiple real genes
  expect_equal(cleaned$gene_symbol[5], "BRCA1")
})


test_that("filter_genes works with case-insensitive matching", {
  test_data <- tibble::tibble(
    gene_symbol = c("BRCA1", "BRCA2", "TP53", "MYH7"),
    variant_id = as.character(1:4),
    variant_name = rep("test", 4),
    chromosome = rep("1", 4),
    position = 1:4,
    clinical_significance = rep(NA_character_, 4),
    review_status = rep(NA_character_, 4),
    phenotypes = rep(NA_character_, 4),
    molecular_consequence = rep(NA_character_, 4),
    database = rep("ClinVar", 4)
  )

  filtered <- card:::.filter_genes(test_data, c("brca1", "tp53"))

  expect_equal(nrow(filtered), 2)
  expect_true(all(filtered$gene_symbol %in% c("BRCA1", "TP53")))
})


# VCF/VEP file handling ----

test_that("read_vep_header extracts field metadata", {
  vep_file <- test_path("sample-filtered-vep.vcf")
  header_info <- read_vep_header(vep_file)

  expect_type(header_info, "list")
  expect_true(length(header_info) > 0)

  # Check key VEP fields
  expect_true(all(c("Consequence", "SYMBOL", "IMPACT", "LoF") %in% names(header_info)))
  expect_match(header_info$LoF, "Loss-of-function")
})


test_that("read_vep_data reads and filters VCF/VEP files", {
  vep_file <- test_path("sample-filtered-vep.vcf")

  # Read all data
  vep_data <- read_vep_data(vep_file)
  expect_s3_class(vep_data, "tbl_df")
  expect_true(nrow(vep_data) > 0)
  expect_true(all(c("SYMBOL", "Consequence", "IMPACT") %in% names(vep_data)))

  # Column filtering
  selected <- read_vep_data(vep_file, columns = c("SYMBOL", "Consequence"))
  expect_equal(ncol(selected), 2)
  expect_true(all(c("SYMBOL", "Consequence") %in% names(selected)))
})


test_that("read_vep_data handles errors and warnings", {
  vep_file <- test_path("sample-filtered-vep.vcf")

  # File not found
  expect_error(read_vep_data("/nonexistent/file.vcf"), "File not found")

  # Missing columns warning
  expect_warning(
    read_vep_data(vep_file, columns = c("SYMBOL", "InvalidColumn")),
    "Requested columns not found"
  )
})


test_that("read_vep_data tibbles can be combined", {
  vep_file <- test_path("sample-filtered-vep.vcf")

  # Demonstrate combining data from multiple sources
  data1 <- read_vep_data(vep_file, columns = c("SYMBOL", "Consequence"))
  data2 <- read_vep_data(vep_file, columns = c("SYMBOL", "Consequence"))
  combined <- dplyr::bind_rows(data1, data2)

  expect_s3_class(combined, "tbl_df")
  expect_equal(nrow(combined), nrow(data1) + nrow(data2))
  expect_true(all(c("SYMBOL", "Consequence") %in% names(combined)))
})

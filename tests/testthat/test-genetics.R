# Tests for genetic variant query functions ----------------------------

test_that("query_genetic_variants validates phenotype parameter", {
  # Must be character
  expect_error(
    query_genetic_variants(123),
    "'phenotype' must be a non-empty character string"
  )

  # Must be length 1
  expect_error(
    query_genetic_variants(c("condition1", "condition2")),
    "'phenotype' must be a non-empty character string"
  )

  # Must be non-empty
  expect_error(
    query_genetic_variants(""),
    "'phenotype' must be a non-empty character string"
  )
})


test_that("query_genetic_variants validates database parameter", {
  # Must be character
  expect_error(
    query_genetic_variants("atrial fibrillation", database = 123),
    "'database' must be a single character string"
  )

  # Must be length 1
  expect_error(
    query_genetic_variants(
      "atrial fibrillation",
      database = c("clinvar", "gnomad")
    ),
    "'database' must be a single character string"
  )

  # Must be supported database
  expect_error(
    query_genetic_variants("atrial fibrillation", database = "unsupported_db"),
    "Unsupported database.*Currently only 'clinvar' is supported"
  )
})


test_that("query_genetic_variants validates api_key parameter", {
  # Must be NULL or character
  expect_error(
    query_genetic_variants("atrial fibrillation", api_key = 123),
    "'api_key' must be NULL or a single character string"
  )

  # Must be length 1 if not NULL
  expect_error(
    query_genetic_variants("atrial fibrillation", api_key = c("key1", "key2")),
    "'api_key' must be NULL or a single character string"
  )
})


test_that("query_genetic_variants validates max_results parameter", {
  # Must be numeric
  expect_error(
    query_genetic_variants("atrial fibrillation", max_results = "100"),
    "'max_results' must be a single number between 1 and 10,000"
  )

  # Must be length 1
  expect_error(
    query_genetic_variants("atrial fibrillation", max_results = c(10, 20)),
    "'max_results' must be a single number between 1 and 10,000"
  )

  # Must be >= 1
  expect_error(
    query_genetic_variants("atrial fibrillation", max_results = 0),
    "'max_results' must be a single number between 1 and 10,000"
  )

  # Must be <= 10,000
  expect_error(
    query_genetic_variants("atrial fibrillation", max_results = 10001),
    "'max_results' must be a single number between 1 and 10,000"
  )
})


# Test return structure ----

test_that("query_genetic_variants returns tibble with correct columns", {
  skip_if_offline()
  skip_on_cran()

  # Use a well-known condition with limited results
  result <- query_genetic_variants(
    "Brugada syndrome",
    max_results = 10
  )

  expect_s3_class(result, "tbl_df")

  # Check all expected columns are present
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


test_that("query_genetic_variants returns correct column types", {
  skip_if_offline()
  skip_on_cran()
  skip()

  result <- query_genetic_variants(
    "long QT syndrome",
    max_results = 5
  )

  expect_type(result$gene_symbol, "character")
  expect_type(result$variant_id, "character")
  expect_type(result$variant_name, "character")
  expect_type(result$chromosome, "character")
  expect_type(result$position, "integer")
  expect_type(result$clinical_significance, "character")
  expect_type(result$review_status, "character")
  expect_type(result$phenotypes, "character")
  expect_type(result$molecular_consequence, "character")
  expect_type(result$database, "character")
})


# Test functionality ----

test_that("query_genetic_variants returns results for known phenotype", {
  skip_if_offline()
  skip_on_cran()

  # Hypertrophic cardiomyopathy is well-studied with many variants
  result <- query_genetic_variants(
    "hypertrophic cardiomyopathy",
    max_results = 20
  )

  # Should return at least some results
  expect_true(nrow(result) > 0)

  # Database column should all be "ClinVar"
  expect_true(all(result$database == "ClinVar"))

  # Should have some gene symbols
  expect_true(any(!is.na(result$gene_symbol)))
})


test_that("query_genetic_variants respects max_results parameter", {
  skip_if_offline()
  skip_on_cran()

  result <- query_genetic_variants(
    "atrial fibrillation",
    max_results = 15
  )

  # Should return at most max_results rows
  expect_true(nrow(result) <= 15)
})


test_that("query_genetic_variants handles non-existent phenotype gracefully", {
  skip_if_offline()
  skip_on_cran()

  # Use a nonsense phenotype that shouldn't exist
  expect_message(
    result <- query_genetic_variants(
      "xyzabc123nonexistent999",
      max_results = 10
    ),
    "No variants found for phenotype"
  )

  # Should return empty tibble with correct structure
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)

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


test_that("query_genetic_variants works with case variations", {
  skip_if_offline()
  skip_on_cran()

  # Database parameter should be case-insensitive
  result_lower <- query_genetic_variants(
    "arrhythmia",
    database = "clinvar",
    max_results = 5
  )

  result_upper <- query_genetic_variants(
    "arrhythmia",
    database = "CLINVAR",
    max_results = 5
  )

  result_mixed <- query_genetic_variants(
    "arrhythmia",
    database = "ClinVar",
    max_results = 5
  )

  # All should succeed and return same structure
  expect_s3_class(result_lower, "tbl_df")
  expect_s3_class(result_upper, "tbl_df")
  expect_s3_class(result_mixed, "tbl_df")
})


# Test empty result handling ----

test_that("empty result table has correct structure", {
  empty_table <- card:::.empty_result_table()

  expect_s3_class(empty_table, "tbl_df")
  expect_equal(nrow(empty_table), 0)

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

  expect_true(all(expected_cols %in% names(empty_table)))
})


# Integration test ----

test_that("full workflow integration test with known cardiovascular phenotype", {
  skip_if_offline()
  skip_on_cran()

  # Test with a well-characterized cardiovascular condition
  result <- query_genetic_variants(
    "familial hypercholesterolemia",
    database = "clinvar",
    max_results = 30
  )

  # Should find variants
  expect_true(nrow(result) > 0)

  # Should have LDLR gene (most common gene for this condition)
  expect_true(any(grepl("LDLR", result$gene_symbol, ignore.case = TRUE)))

  # Should have clinical significance data
  expect_true(any(!is.na(result$clinical_significance)))

  # Should have phenotype information
  expect_true(any(!is.na(result$phenotypes)))

  # Variant IDs should be unique
  expect_equal(length(unique(result$variant_id)), nrow(result))
})


# Test new gene filtering features ----

test_that("query_genetic_variants filters to specific genes", {
  skip_if_offline()
  skip_on_cran()
  skip()

  # Query with gene filter
  result <- query_genetic_variants(
    "long QT syndrome",
    genes = c("KCNQ1", "KCNH2"),
    max_results = 20
  )

  # All results should be from specified genes
  expect_true(all(toupper(result$gene_symbol) %in% c("KCNQ1", "KCNH2")))
})


test_that("query_genetic_variants validates genes parameter", {
  # Must be character or NULL
  expect_error(
    query_genetic_variants("atrial fibrillation", genes = 123),
    "'genes' must be NULL or a character vector"
  )
})


test_that("query_genetic_variants filters pseudogenes by default", {
  skip_if_offline()
  skip_on_cran()
  skip()

  # Query without pseudogene cleaning
  result_with_pseudo <- query_genetic_variants(
    "cardiomyopathy",
    max_results = 30,
    clean_gene_symbols = FALSE
  )

  # Query with pseudogene cleaning (default)
  result_without_pseudo <- query_genetic_variants(
    "cardiomyopathy",
    max_results = 30,
    clean_gene_symbols = TRUE
  )

  # Cleaned results should not have LOC/LINC/MIR genes
  pseudo_pattern <- "^(LOC|LINC|MIR)[0-9]"
  expect_false(any(grepl(pseudo_pattern, result_without_pseudo$gene_symbol)))
})


test_that("query_genetic_variants validates clean_gene_symbols parameter", {
  # Must be logical
  expect_error(
    query_genetic_variants("atrial fibrillation", clean_gene_symbols = "yes"),
    "'clean_gene_symbols' must be a single logical value"
  )

  # Must be length 1
  expect_error(
    query_genetic_variants(
      "atrial fibrillation",
      clean_gene_symbols = c(TRUE, FALSE)
    ),
    "'clean_gene_symbols' must be a single logical value"
  )
})


# Test gene-level summary function ----

test_that("query_genes_by_phenotype returns correct structure", {
  skip_if_offline()
  skip_on_cran()
  skip()

  result <- query_genes_by_phenotype(
    "Brugada syndrome",
    max_results = 20
  )

  expect_s3_class(result, "tbl_df")

  # Check all expected columns are present
  expected_cols <- c(
    "gene_symbol",
    "n_variants",
    "n_pathogenic",
    "n_benign",
    "n_vus",
    "phenotypes",
    "chromosomes",
    "database"
  )

  expect_true(all(expected_cols %in% names(result)))

  # Check column types
  expect_type(result$gene_symbol, "character")
  expect_type(result$n_variants, "integer")
  expect_type(result$n_pathogenic, "integer")
  expect_type(result$n_benign, "integer")
  expect_type(result$n_vus, "integer")
})


test_that("query_genes_by_phenotype aggregates correctly", {
  skip_if_offline()
  skip_on_cran()
  skip()

  result <- query_genes_by_phenotype(
    "hypertrophic cardiomyopathy",
    max_results = 50
  )

  # Each gene should appear only once
  expect_equal(nrow(result), length(unique(result$gene_symbol)))

  # Counts should be non-negative
  expect_true(all(result$n_variants >= 0))
  expect_true(all(result$n_pathogenic >= 0))
  expect_true(all(result$n_benign >= 0))
  expect_true(all(result$n_vus >= 0))

  # Total of clinical significance categories should not exceed total variants
  expect_true(all(
    result$n_pathogenic + result$n_benign + result$n_vus <= result$n_variants
  ))
})


test_that("query_genes_by_phenotype accepts gene filter", {
  skip_if_offline()
  skip_on_cran()
  skip()

  result <- query_genes_by_phenotype(
    "cardiomyopathy",
    genes = c("MYH7", "MYBPC3"),
    max_results = 30
  )

  # Should only include specified genes
  expect_true(all(toupper(result$gene_symbol) %in% c("MYH7", "MYBPC3")))
})


test_that("query_genes_by_phenotype handles no results gracefully", {
  skip_if_offline()
  skip_on_cran()

  result <- query_genes_by_phenotype(
    "xyzabc123nonexistent999",
    max_results = 10
  )

  # Should return empty tibble with correct structure
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)

  expected_cols <- c(
    "gene_symbol",
    "n_variants",
    "n_pathogenic",
    "n_benign",
    "n_vus",
    "phenotypes",
    "chromosomes",
    "database"
  )
  expect_true(all(expected_cols %in% names(result)))
})


# Test helper functions ----

test_that("clean_gene_symbols extracts real genes from mixed strings", {
  # Create test data
  test_data <- tibble::tibble(
    gene_symbol = c(
      "BRCA1",
      "LOC123456",
      "LOC123456; TP53",
      "TP53; LOC999",
      "LINC00123"
    ),
    variant_id = c("1", "2", "3", "4", "5"),
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

  # All rows should be retained (conservative - no data loss)
  expect_equal(nrow(cleaned), 5)

  # Real genes should be preserved
  expect_equal(cleaned$gene_symbol[1], "BRCA1")

  # Pure pseudogenes kept conservatively
  expect_equal(cleaned$gene_symbol[2], "LOC123456")

  # Real gene extracted from "LOC123456; TP53" (order independent)
  expect_equal(cleaned$gene_symbol[3], "TP53")

  # Real gene extracted from "TP53; LOC999" (order independent)
  expect_equal(cleaned$gene_symbol[4], "TP53")
})


test_that("clean_gene_symbols keeps first real gene from multiple", {
  # Create test data with multiple real genes
  test_data <- tibble::tibble(
    gene_symbol = c("BRCA1; BRCA2", "TP53; TP63", "LOC123; TTN; MYH7"),
    variant_id = c("1", "2", "3"),
    variant_name = rep("test", 3),
    chromosome = rep("1", 3),
    position = 1:3,
    clinical_significance = rep(NA_character_, 3),
    review_status = rep(NA_character_, 3),
    phenotypes = rep(NA_character_, 3),
    molecular_consequence = rep(NA_character_, 3),
    database = rep("ClinVar", 3)
  )

  cleaned <- card:::.clean_gene_symbols(test_data)

  # Should keep first real gene from each
  expect_equal(cleaned$gene_symbol[1], "BRCA1")
  expect_equal(cleaned$gene_symbol[2], "TP53")

  # From "LOC123; TTN; MYH7", should extract TTN (first real gene after filtering)
  expect_equal(cleaned$gene_symbol[3], "TTN")
})


test_that("filter_genes works with case insensitive matching", {
  # Create test data
  test_data <- tibble::tibble(
    gene_symbol = c("BRCA1", "BRCA2", "TP53", "MYH7"),
    variant_id = c("1", "2", "3", "4"),
    variant_name = rep("test", 4),
    chromosome = rep("1", 4),
    position = 1:4,
    clinical_significance = rep(NA_character_, 4),
    review_status = rep(NA_character_, 4),
    phenotypes = rep(NA_character_, 4),
    molecular_consequence = rep(NA_character_, 4),
    database = rep("ClinVar", 4)
  )

  # Filter with lowercase
  filtered <- card:::.filter_genes(test_data, c("brca1", "tp53"))

  # Should match case-insensitively
  expect_equal(nrow(filtered), 2)
  expect_true(all(filtered$gene_symbol %in% c("BRCA1", "TP53")))
})
# Functionality tests ----

test_that("query_genetic_variants returns results for known phenotype", {
  skip_if_offline()
  skip_on_cran()

  result <- query_genetic_variants(
    "hypertrophic cardiomyopathy",
    max_results = 20
  )
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
    result <- query_genetic_variants(
      "xyzabc123nonexistent999",
      max_results = 10
    ),
    "No variants found for phenotype"
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)

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


test_that("case-insensitive database argument works", {
  skip_if_offline()
  skip_on_cran()

  r1 <- query_genetic_variants(
    "arrhythmia",
    database = "clinvar",
    max_results = 5
  )
  r2 <- query_genetic_variants(
    "arrhythmia",
    database = "CLINVAR",
    max_results = 5
  )
  expect_s3_class(r1, "tbl_df")
  expect_s3_class(r2, "tbl_df")
})


# Empty result helper ----

test_that("empty result table has correct structure", {
  empty_table <- card:::.empty_result_table()
  expect_s3_class(empty_table, "tbl_df")
  expect_equal(nrow(empty_table), 0)

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
  expect_true(all(expected_cols %in% names(empty_table)))
})


# Integration test (light) ----

test_that("integration smoke test for familial hypercholesterolemia", {
  skip_if_offline()
  skip_on_cran()

  result <- query_genetic_variants(
    "familial hypercholesterolemia",
    database = "clinvar",
    max_results = 20
  )
  expect_true(nrow(result) > 0)
  expect_true(any(grepl("LDLR", result$gene_symbol, ignore.case = TRUE)))
  expect_true(any(!is.na(result$clinical_significance)))
  expect_true(any(!is.na(result$phenotypes)))
  expect_equal(length(unique(result$variant_id)), nrow(result))
})


# Gene filtering and pseudogene helpers ----

test_that("clean_gene_symbols preserves/cleans gene_symbol entries", {
  test_data <- tibble::tibble(
    gene_symbol = c(
      "BRCA1",
      "LOC123456",
      "LOC123456; TP53",
      "TP53; LOC999",
      "LINC00123"
    ),
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
  expect_equal(cleaned$gene_symbol[1], "BRCA1")
  expect_equal(cleaned$gene_symbol[2], "LOC123456")
  expect_equal(cleaned$gene_symbol[3], "TP53")
  expect_equal(cleaned$gene_symbol[4], "TP53")
})


test_that("clean_gene_symbols keeps first real gene from multiple entries", {
  test_data <- tibble::tibble(
    gene_symbol = c("BRCA1; BRCA2", "TP53; TP63", "LOC123; TTN; MYH7"),
    variant_id = as.character(1:3),
    variant_name = rep("test", 3),
    chromosome = rep("1", 3),
    position = 1:3,
    clinical_significance = rep(NA_character_, 3),
    review_status = rep(NA_character_, 3),
    phenotypes = rep(NA_character_, 3),
    molecular_consequence = rep(NA_character_, 3),
    database = rep("ClinVar", 3)
  )

  cleaned <- card:::.clean_gene_symbols(test_data)
  expect_equal(cleaned$gene_symbol[1], "BRCA1")
  expect_equal(cleaned$gene_symbol[2], "TP53")
  expect_equal(cleaned$gene_symbol[3], "TTN")
})


test_that("filter_genes matches case-insensitively", {
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

# VCF files ----------------------------------------------------

test_that("can read VCF headers", {
  vep_file <- test_path("sample-filtered-vep.vcf")
  header_info <- read_vcf_header(vep_file)
  expect_type(header_info, "list")
  expect_true("info_fields" %in% names(header_info))
  expect_true("csq_fields" %in% names(header_info))
  expect_true("lof_codes" %in% names(header_info))

  # Verify CSQ fields were extracted from VEP text format
  expect_gt(length(header_info$csq_fields), 0)
  expect_true("Uploaded_variation" %in% header_info$csq_fields)
  expect_true("Gene" %in% header_info$csq_fields)
  expect_true("Consequence" %in% header_info$csq_fields)
  expect_true("LoF" %in% header_info$csq_fields)
})

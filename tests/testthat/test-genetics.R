# Tests for genetic variant query functions ----------------------------

test_that("query_genetic_variants validates all parameters correctly", {
  # Phenotype validation
  expect_error(query_genetic_variants(123), "'phenotype' must be a non-empty character string")
  expect_error(query_genetic_variants(c("a", "b")), "'phenotype' must be a non-empty character string")
  expect_error(query_genetic_variants(""), "'phenotype' must be a non-empty character string")

  # Database validation
  expect_error(query_genetic_variants("af", database = 123), "'database' must be a single character string")
  expect_error(query_genetic_variants("af", database = c("a", "b")), "'database' must be a single character string")
  expect_error(query_genetic_variants("af", database = "invalid"), "Unsupported database.*Currently only 'clinvar' is supported")

  # API key validation
  expect_error(query_genetic_variants("af", api_key = 123), "'api_key' must be NULL or a single character string")
  expect_error(query_genetic_variants("af", api_key = c("a", "b")), "'api_key' must be NULL or a single character string")

  # Max results validation
  expect_error(query_genetic_variants("af", max_results = "100"), "'max_results' must be a single number between 1 and 10,000")
  expect_error(query_genetic_variants("af", max_results = c(10, 20)), "'max_results' must be a single number between 1 and 10,000")
  expect_error(query_genetic_variants("af", max_results = 0), "'max_results' must be a single number between 1 and 10,000")
  expect_error(query_genetic_variants("af", max_results = 10001), "'max_results' must be a single number between 1 and 10,000")

  # Genes validation
  expect_error(query_genetic_variants("af", genes = 123), "'genes' must be NULL or a character vector")

  # Clean gene symbols validation
  expect_error(query_genetic_variants("af", clean_gene_symbols = "yes"), "'clean_gene_symbols' must be a single logical value")
  expect_error(query_genetic_variants("af", clean_gene_symbols = c(TRUE, FALSE)), "'clean_gene_symbols' must be a single logical value")
})


# Test return structure ----

test_that("query_genetic_variants returns tibble with correct columns", {
  skip_if_offline()
  skip_on_cran()

  result <- query_genetic_variants("Brugada syndrome", max_results = 10)

  expect_s3_class(result, "tbl_df")

  expected_cols <- c(
    "gene_symbol", "variant_id", "variant_name", "chromosome", "position",
    "clinical_significance", "review_status", "phenotypes",
    "molecular_consequence", "database"
  )

  expect_true(all(expected_cols %in% names(result)))
})


# Test functionality ----

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


test_that("query_genetic_variants database parameter is case-insensitive", {
  skip_if_offline()
  skip_on_cran()

  result <- query_genetic_variants("arrhythmia", database = "CLINVAR", max_results = 5)
  expect_s3_class(result, "tbl_df")
})




# Integration test ----

test_that("integration test with known cardiovascular phenotype", {
  skip_if_offline()
  skip_on_cran()

  result <- query_genetic_variants(
    "familial hypercholesterolemia",
    database = "clinvar",
    max_results = 30
  )

  expect_true(nrow(result) > 0)
  expect_true(any(grepl("LDLR", result$gene_symbol, ignore.case = TRUE)))
  expect_true(any(!is.na(result$clinical_significance)))
  expect_true(any(!is.na(result$phenotypes)))
  expect_equal(length(unique(result$variant_id)), nrow(result))
})


# Test gene filtering features ----

test_that("query_genetic_variants filters to specific genes", {
  skip_if_offline()
  skip_on_cran()
  skip()

  result <- query_genetic_variants(
    "long QT syndrome",
    genes = c("KCNQ1", "KCNH2"),
    max_results = 20
  )

  expect_true(all(toupper(result$gene_symbol) %in% c("KCNQ1", "KCNH2")))
})


test_that("query_genetic_variants filters pseudogenes by default", {
  skip_if_offline()
  skip_on_cran()
  skip()

  result_with_pseudo <- query_genetic_variants(
    "cardiomyopathy",
    max_results = 30,
    clean_gene_symbols = FALSE
  )

  result_without_pseudo <- query_genetic_variants(
    "cardiomyopathy",
    max_results = 30,
    clean_gene_symbols = TRUE
  )

  # Cleaned results should not have LOC/LINC/MIR genes
  pseudo_pattern <- "^(LOC|LINC|MIR)[0-9]"
  expect_false(any(grepl(pseudo_pattern, result_without_pseudo$gene_symbol)))
})


# Test gene-level summary function ----

test_that("query_genes_by_phenotype returns correct structure and aggregates", {
  skip_if_offline()
  skip_on_cran()
  skip()

  result <- query_genes_by_phenotype("hypertrophic cardiomyopathy", max_results = 50)

  expect_s3_class(result, "tbl_df")

  expected_cols <- c(
    "gene_symbol", "n_variants", "n_pathogenic", "n_benign", "n_vus",
    "phenotypes", "chromosomes", "database"
  )
  expect_true(all(expected_cols %in% names(result)))

  # Each gene should appear only once
  expect_equal(nrow(result), length(unique(result$gene_symbol)))

  # Counts should be non-negative and logically consistent
  expect_true(all(result$n_variants >= 0))
  expect_true(all(result$n_pathogenic + result$n_benign + result$n_vus <= result$n_variants))
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

  expect_true(all(toupper(result$gene_symbol) %in% c("MYH7", "MYBPC3")))
})


# Test helper functions ----

test_that("clean_gene_symbols extracts real genes from mixed strings", {
  test_data <- tibble::tibble(
    gene_symbol = c("BRCA1", "LOC123456", "LOC123456; TP53", "TP53; LOC999", "LINC00123"),
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

  expect_equal(nrow(cleaned), 5)
  expect_equal(cleaned$gene_symbol[1], "BRCA1")
  expect_equal(cleaned$gene_symbol[2], "LOC123456")  # Pure pseudogenes kept conservatively
  expect_equal(cleaned$gene_symbol[3], "TP53")
  expect_equal(cleaned$gene_symbol[4], "TP53")
})


test_that("clean_gene_symbols keeps first real gene from multiple", {
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

  expect_equal(cleaned$gene_symbol[1], "BRCA1")
  expect_equal(cleaned$gene_symbol[2], "TP53")
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

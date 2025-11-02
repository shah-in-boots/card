# Tests for genetic variant query functions

# Test input validation ----

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
    query_genetic_variants("atrial fibrillation", database = c("clinvar", "gnomad")),
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
    "'max_results' must be a single number between 1 and 500"
  )

  # Must be length 1
  expect_error(
    query_genetic_variants("atrial fibrillation", max_results = c(10, 20)),
    "'max_results' must be a single number between 1 and 500"
  )

  # Must be >= 1
  expect_error(
    query_genetic_variants("atrial fibrillation", max_results = 0),
    "'max_results' must be a single number between 1 and 500"
  )

  # Must be <= 500
  expect_error(
    query_genetic_variants("atrial fibrillation", max_results = 501),
    "'max_results' must be a single number between 1 and 500"
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

  result <- query_genetic_variants(
    "long QT syndrome",
    max_results = 5
  )

  # Skip if no results (might happen with very restrictive queries)
  skip_if(nrow(result) == 0, "No results returned from API")

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
    "gene_symbol", "variant_id", "variant_name", "chromosome",
    "position", "clinical_significance", "review_status",
    "phenotypes", "molecular_consequence", "database"
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


# Test rate limiting ----

test_that("rate limiting function delays appropriately", {

  # Test without API key (should use 3 requests/second)
  start_time <- Sys.time()

  # Clear any existing rate limit state
  if (exists(".clinvar_last_request", envir = .GlobalEnv)) {
    rm(".clinvar_last_request", envir = .GlobalEnv)
  }

  # Make 3 consecutive calls
  card:::.rate_limit(NULL)
  card:::.rate_limit(NULL)
  card:::.rate_limit(NULL)

  end_time <- Sys.time()
  elapsed <- as.numeric(difftime(end_time, start_time, units = "secs"))

  # Should take at least ~0.6 seconds (2 intervals of ~0.33 seconds each)
  # Using a conservative threshold to avoid test flakiness
  expect_true(elapsed >= 0.5)

  # Clean up
  if (exists(".clinvar_last_request", envir = .GlobalEnv)) {
    rm(".clinvar_last_request", envir = .GlobalEnv)
  }
})


test_that("rate limiting with API key allows faster requests", {

  # Test with API key (should use 10 requests/second)
  start_time <- Sys.time()

  # Clear any existing rate limit state
  if (exists(".clinvar_last_request", envir = .GlobalEnv)) {
    rm(".clinvar_last_request", envir = .GlobalEnv)
  }

  # Make 3 consecutive calls with fake API key
  card:::.rate_limit("fake_api_key_for_testing")
  card:::.rate_limit("fake_api_key_for_testing")
  card:::.rate_limit("fake_api_key_for_testing")

  end_time <- Sys.time()
  elapsed <- as.numeric(difftime(end_time, start_time, units = "secs"))

  # Should take at least ~0.2 seconds (2 intervals of ~0.1 seconds each)
  # Should be faster than the no-API-key case
  expect_true(elapsed >= 0.15)
  expect_true(elapsed < 0.5)  # Should be notably faster than without key

  # Clean up
  if (exists(".clinvar_last_request", envir = .GlobalEnv)) {
    rm(".clinvar_last_request", envir = .GlobalEnv)
  }
})


# Test empty result handling ----

test_that("empty result table has correct structure", {

  empty_table <- card:::.empty_result_table()

  expect_s3_class(empty_table, "tbl_df")
  expect_equal(nrow(empty_table), 0)

  expected_cols <- c(
    "gene_symbol", "variant_id", "variant_name", "chromosome",
    "position", "clinical_significance", "review_status",
    "phenotypes", "molecular_consequence", "database"
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

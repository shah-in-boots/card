test_that("query_gene_database returns expected tibble structure", {
  mock_responses <- list(
    list(
      search = list(
        hits = list(
          list(id = "EFO_0000275", name = "Atrial fibrillation", entity = "DISEASE", score = 0.95)
        )
      )
    ),
    list(
      disease = list(
        id = "EFO_0000275",
        name = "Atrial fibrillation",
        associatedTargets = list(
          edges = list(
            list(
              node = list(
                score = 0.87,
                target = list(
                  id = "ENSG000001",
                  approvedSymbol = "GENE1",
                  approvedName = "Gene 1",
                  geneId = "ENSG000001"
                ),
                datasourceScores = list(
                  list(id = "eva", score = 0.45),
                  list(datasourceId = "orphanet", score = 0.22)
                ),
                datatypeScores = list(
                  list(id = "genetic_association", score = 0.67),
                  list(datatypeId = "literature", score = 0.21)
                )
              )
            )
          ),
          pageInfo = list(cursor = NULL, hasNextPage = FALSE)
        )
      )
    )
  )

  call_counter <- 0L
  mock_request <- function(query, variables) {
    call_counter <<- call_counter + 1L
    mock_responses[[call_counter]]
  }

  result <- query_gene_database(
    disease = " atrial fibrillation ",
    database = "open_targets",
    limit = 5,
    request_fun = mock_request
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
  expect_true(all(c("disease_id", "disease_name", "target_id", "gene_symbol", "association_score") %in% names(result)))
  expect_equal(result$disease_id, "EFO_0000275")
  expect_equal(result$gene_symbol, "GENE1")
  expect_true("datatype_genetic_association" %in% names(result))
  expect_true("datatype_literature" %in% names(result))
  expect_equal(result$datatype_genetic_association, 0.67)
  expect_equal(result$datatype_literature, 0.21)
  expect_s3_class(result$datasource_scores[[1]], "tbl_df")
  expect_identical(result$datasource_scores[[1]]$datasource_id, c("eva", "orphanet"))
})

test_that("query_gene_database errors when disease cannot be matched", {
  mock_responses <- list(
    list(search = list(hits = list()))
  )

  mock_request <- function(query, variables) {
    mock_responses[[1]]
  }

  expect_error(
    query_gene_database(
      disease = "Unknown disease",
      database = "open_targets",
      limit = 10,
      request_fun = mock_request
    ),
    "No matching disease"
  )
})

test_that("query_gene_database validates arguments", {
  expect_error(query_gene_database(disease = "", request_fun = function(...) NULL))
  expect_error(query_gene_database(disease = 123, request_fun = function(...) NULL))
  expect_error(query_gene_database(disease = "AF", limit = 0, request_fun = function(...) NULL))
  expect_error(query_gene_database(disease = "AF", database = "other", request_fun = function(...) NULL))
})

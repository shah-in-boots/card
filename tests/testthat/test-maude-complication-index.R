normalize_maude_test_term <- function(x) {
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", " ", x)
  trimws(x)
}

test_that("maude_complication_index covers all MAUDE problem terms", {
  source_terms <-
    dplyr::bind_rows(
      load_maude_codes("A"),
      load_maude_codes("E"),
      load_maude_codes("F")
    ) |>
    dplyr::distinct(annex, imdrf_code, .keep_all = TRUE) |>
    dplyr::transmute(term = normalize_maude_test_term(term)) |>
    dplyr::distinct(term) |>
    dplyr::pull(term)

  indexed_terms <- unique(unlist(maude_complication_index, use.names = FALSE))

  expect_true("not_indexed" %in% names(maude_complication_index))
  expect_true(length(maude_complication_index$not_indexed) > 0)
  expect_setequal(indexed_terms, source_terms)
})

test_that("maude_complication_index includes expected clinical mappings", {
  expect_true("cardiac tamponade" %in% maude_complication_index$pericardial)
  expect_true("low blood pressure hypotension" %in% maude_complication_index$coronary)
  expect_true("arrhythmia" %in% maude_complication_index$arrhythmia)
  expect_true("no health consequences or impact" %in% maude_complication_index$no_harm)
})

test_that("maude_term_to_complication groups terms and preserves originals", {
  out <- maude_term_to_complication(c(
    "Cardiac Tamponade",
    "Low blood pressure / hypotension",
    "No Health Consequences or Impact"
  ),
  definitions = complication_definitions,
  index = maude_complication_index)

  expect_equal(out$pericardial, c(
    "Cardiac Tamponade",
    "Low blood pressure / hypotension"
  ))
  expect_equal(out$coronary, "Low blood pressure / hypotension")
  expect_equal(out$vascular, "Low blood pressure / hypotension")
  expect_equal(out$no_harm, "No Health Consequences or Impact")
})

test_that("maude_term_to_complication preserves unmatched terms as not_indexed", {
  out <- maude_term_to_complication(
    c("Pericardial Effusion", "Unlisted MAUDE term from a future code table"),
    definitions = complication_definitions["pericardial"],
    index = list(pericardial = "pericardial effusion")
  )

  expect_equal(out$pericardial, "Pericardial Effusion")
  expect_equal(
    out$not_indexed,
    "Unlisted MAUDE term from a future code table"
  )
})

test_that("maude_term_to_complication validates custom index names", {
  bad_index <- list(
    unknown_complication = "cardiac tamponade"
  )

  expect_error(
    maude_term_to_complication(
      "cardiac tamponade",
      definitions = complication_definitions,
      index = bad_index
    ),
    "contains names not present"
  )
})

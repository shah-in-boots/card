test_that("load_maude_codes returns annex E (clinical signs)", {
  e <- load_maude_codes("E")
  expect_s3_class(e, "tbl_df")
  expect_true(nrow(e) > 0)
  expect_true(all(c("term", "definition", "fda_code", "imdrf_code") %in% names(e)))
  expect_true("Arrhythmia" %in% e$term)
})

test_that("load_maude_codes returns annex F (health impact)", {
  f <- load_maude_codes("F")
  expect_s3_class(f, "tbl_df")
  expect_true(nrow(f) > 0)
  expect_true(all(c("term", "definition", "fda_code", "imdrf_code") %in% names(f)))
})

test_that("load_maude_codes returns annex A (device problems)", {
  a <- load_maude_codes("A")
  expect_s3_class(a, "tbl_df")
  expect_true(nrow(a) > 0)
})

test_that("load_maude_codes rejects invalid annex", {
  expect_error(load_maude_codes("Z"), "Invalid annex")
})

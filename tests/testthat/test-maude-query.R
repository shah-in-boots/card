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

test_that("query_maude validates web description fill option", {
  expect_error(
    query_maude("pacemaker", fill_descriptions_from_web = NA),
    "'fill_descriptions_from_web' must be TRUE or FALSE"
  )
})

test_that("flatten_maude_record keeps all narrative text blocks", {
  rec <- list(
    report_number = "RPT-1",
    event_type = "Malfunction",
    date_received = "20260115",
    device = list(list(
      generic_name = "Mapping Catheter",
      brand_name = "Alpha",
      manufacturer_d_name = "Acme",
      device_problem_codes = c("Detachment", "Migration")
    )),
    patient = list(list(
      patient_problems = c("Pain", "Bleeding")
    )),
    mdr_text = list(
      list(text_type_code = "B5", text = "First narrative block."),
      list(text_type_code = "H10", text = "Second narrative block.")
    )
  )

  out <- getFromNamespace("flatten_maude_record", "card")(rec)

  expect_identical(out$report_number, "RPT-1")
  expect_identical(
    out$event_description,
    "First narrative block. | Second narrative block."
  )
  expect_identical(out$patient_problem, "Pain; Bleeding")
  expect_identical(out$device_problem, "Detachment; Migration")
})

test_that("flatten_maude_record preserves narrative text from simplified shapes", {
  device <- data.frame(
    generic_name = "Ablation Catheter",
    brand_name = "Bravo",
    manufacturer_d_name = "Example Devices",
    stringsAsFactors = FALSE
  )
  device$device_problem_codes <- I(list(c("Failure to Fire", "Arcing")))

  patient <- list(
    patient_problems = c("Arrhythmia", "Hypotension")
  )

  mdr_text <- data.frame(
    text_type_code = c("B5", "H10"),
    text = c("Narrative from manufacturer.", "Follow-up narrative."),
    stringsAsFactors = FALSE
  )

  rec <- list(
    report_number = "RPT-2",
    event_type = "Injury",
    date_received = "20260201",
    device = device,
    patient = patient,
    mdr_text = mdr_text
  )

  out <- getFromNamespace("flatten_maude_record", "card")(rec)

  expect_identical(out$device_generic_name, "Ablation Catheter")
  expect_identical(out$device_brand_name, "Bravo")
  expect_identical(out$manufacturer_name, "Example Devices")
  expect_identical(
    out$event_description,
    "Narrative from manufacturer. | Follow-up narrative."
  )
  expect_identical(out$patient_problem, "Arrhythmia; Hypotension")
  expect_identical(out$device_problem, "Failure to Fire; Arcing")
})

test_that("query_maude can handle larger limits", {

  dat <- query_maude(search = "PFA", limit = 1000)
  expect_s3_class(dat, "tbl_df")
  expect_equal(nrow(dat), 1000)

})

test_that("can fill out blank descriptions from the web", {
  # First get data and find missing. 
  # Need to pull during testing so its the same
  dat <- query_maude(search = "PFA", fill_descriptions_from_web = FALSE)
  missing_desc <- dat |> 
    dplyr::filter(is.na(event_description)) |> 
    dplyr::pull(report_number)

  # Now check if filling from web works
  dat_filled <- query_maude(search = "PFA", fill_descriptions_from_web = TRUE)
  filled_desc <- dat_filled |> 
    dplyr::filter(report_number %in% missing_desc) |> 
    dplyr::pull(event_description)

  # Check to see if they are the same
  expect_length(filled_desc, length(missing_desc))
})

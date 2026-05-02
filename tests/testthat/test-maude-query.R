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
    query_maude("pacemaker", descriptions_from_web = NA),
    "'descriptions_from_web' must be TRUE or FALSE"
  )
})

test_that("query_maude handles dates appropriately for R", {

  # Check if returns dates appropriately
  out <- query_maude(search = "pacemaker", limit = 3, verbose = FALSE)
  expect_s3_class(out$date_received, "Date")

  # Check if can input dates to query in date format (Date object)
  out <- query_maude(
    search = "pacemaker",
    date_start = as.Date("2026-01-01"),
    date_end = as.Date("2026-01-31"),
    limit = 1,
    verbose = FALSE
  )
  expect_s3_class(out$date_received, "Date")
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
  calls <- list()

  testthat::local_mocked_bindings(
    maude_fda_api_call = function(search_query, limit, skip, ...) {
      calls[[length(calls) + 1L]] <<- list(
        search_query = search_query,
        limit = as.integer(limit),
        skip = as.integer(skip)
      )

      tibble::tibble(
        report_number = paste0("RPT-", skip + seq_len(limit)),
        event_type = "Malfunction",
        date_received = "20260115"
      )
    },
    .package = "card"
  )

  dat <- query_maude(search = "PFA", limit = 1000, verbose = FALSE)

  expect_s3_class(dat, "tbl_df")
  expect_equal(nrow(dat), 1000)
  expect_equal(vapply(calls, `[[`, integer(1), "limit"), c(999L, 1L))
  expect_equal(vapply(calls, `[[`, integer(1), "skip"), c(0L, 999L))
  expect_true(all(vapply(
    calls,
    function(x) grepl("PFA", x$search_query, fixed = TRUE),
    logical(1)
  )))

})

test_that("can fill out blank descriptions from the web", {
  fake_result <- tibble::tibble(
    report_number = c("RPT-1", "RPT-2"),
    mdr_report_key = c("KEY-1", "KEY-2"),
    event_type = c("Malfunction", "Injury"),
    date_received = c("20260115", "20260116"),
    event_description = c(NA_character_, "Existing description")
  )

  fill_called <- FALSE

  testthat::local_mocked_bindings(
    maude_fda_api_call = function(...) fake_result,
    get_maude_file_descriptions = function(events, ...) events,
    get_maude_web_descriptions = function(events, quiet = FALSE) {
      fill_called <<- TRUE
      missing_description <- is.na(events$event_description)
      events$event_description[missing_description] <- "Filled description"
      events
    },
    .package = "card"
  )

  dat_filled <- query_maude(
    search = "PFA",
    descriptions_from_web = TRUE,
    verbose = FALSE
  )

  expect_true(fill_called)
  expect_equal(
    dat_filled$event_description,
    c("Filled description", "Existing description")
  )
  expect_s3_class(dat_filled$date_received, "Date")
})

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

test_that("every annex row carries its own term and its ancestry", {
  # A term coded at the family level is a value in the data, not a heading:
  # "Material Integrity Problem" and "Use of Device Problem" are both coded in
  # MAUDE. Filling the level columns downward gave 26 of the 27 Annex A families
  # the previous family's last child as their `term`, so joining returned terms
  # to this table dropped every family-coded report.
  for (annex in c("A", "E", "F")) {
    dat <- load_maude_codes(annex)
    families <- dat[nchar(dat$imdrf_code) == 3L, ]

    expect_false(anyNA(dat$term))
    expect_false(anyNA(dat$level_1))
    expect_gt(nrow(families), 20)
    expect_identical(families$term, families$level_1)
    expect_true(all(is.na(families$level_2)))
    expect_true(all(is.na(families$level_3)))
  }
})

test_that("annex terms roll up to the family that owns them", {
  a <- load_maude_codes("A")

  expect_identical(
    a$term[a$imdrf_code == "A05"],
    "Mechanical Problem"
  )
  expect_identical(
    a$level_1[a$imdrf_code == "A0501"],
    "Mechanical Problem"
  )

  # Annex E is a polyhierarchy: "Brain Injury" sits under two families, so the
  # code appears once per parent rather than once.
  e <- load_maude_codes("E")
  expect_setequal(
    e$level_1[e$imdrf_code == "E0102"],
    c("Nervous System", "Injury")
  )
})

test_that("maude_query validates web description fill option", {
  expect_error(
    maude_query("pacemaker", descriptions_from_web = NA),
    "'descriptions_from_web' must be TRUE or FALSE"
  )
})

test_that("maude_query handles dates appropriately for R", {
  calls <- list()

  testthat::local_mocked_bindings(
    maude_fda_api_call = function(search_query, limit, skip, ...) {
      calls[[length(calls) + 1L]] <<- list(
        search_query = search_query,
        limit = limit,
        skip = skip
      )

      tibble::tibble(
        report_number = paste0("RPT-", seq_len(limit)),
        event_type = "Malfunction",
        date_received = rep("20260115", limit)
      )
    },
    .package = "card"
  )

  # Check if returns dates appropriately
  out <- maude_query(search = "pacemaker", limit = 3, verbose = FALSE)
  expect_s3_class(out$date_received, "Date")

  # Check if can input dates to query in date format (Date object)
  out <- maude_query(
    search = "pacemaker",
    date_start = as.Date("2026-01-01"),
    date_end = as.Date("2026-01-31"),
    limit = 1,
    verbose = FALSE
  )

  expect_s3_class(out$date_received, "Date")
  expect_true(grepl(
    "date_received:[20260101 TO 20260131]",
    calls[[2]]$search_query,
    fixed = TRUE
  ))
})

test_that("flatten_maude_record keeps all narrative text blocks", {
  rec <- list(
    report_number = "RPT-1",
    event_type = "Malfunction",
    date_received = "20260115",
    product_problems = c("Detachment", "Migration"),
    device = list(list(
      generic_name = "Mapping Catheter",
      brand_name = "Alpha",
      manufacturer_d_name = "Acme"
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

test_that("device problems are read from product_problems, not from device[]", {
  # openFDA accepts `device.device_problem_codes` as a search field but never
  # returns it. Reading the column from there gave an NA for every record ever
  # returned, which reads as "MAUDE does not code device problems for these
  # reports" rather than as a bug.
  rec <- list(
    report_number = "RPT-3",
    product_problems = c("Material Deformation", "Material Integrity Problem"),
    device = list(list(
      generic_name = "Ablation Catheter",
      device_problem_codes = c("Should Not Be Read", "Nor This")
    ))
  )

  out <- getFromNamespace("flatten_maude_record", "card")(rec)

  expect_identical(
    out$device_problem,
    "Material Deformation; Material Integrity Problem"
  )

  # And a record with no top-level terms is missing, not filled from device[].
  rec$product_problems <- NULL
  out <- getFromNamespace("flatten_maude_record", "card")(rec)
  expect_identical(out$device_problem, NA_character_)
})

test_that("repeated coded terms are collapsed once, narrative blocks are not", {
  # openFDA emits the coded-term arrays twice for most records; the repetition
  # is an artefact of the join, not two separate problems. Narrative blocks each
  # carry their own key and a supplement may legitimately repeat its text.
  rec <- list(
    report_number = "RPT-4",
    product_problems = c("Material Deformation", "Material Deformation"),
    patient = list(list(
      patient_problems = c("Pericarditis", "Pericarditis")
    )),
    mdr_text = list(
      list(mdr_text_key = "1", text = "Repeated narrative."),
      list(mdr_text_key = "2", text = "Repeated narrative.")
    )
  )

  out <- getFromNamespace("flatten_maude_record", "card")(rec)

  expect_identical(out$device_problem, "Material Deformation")
  expect_identical(out$patient_problem, "Pericarditis")
  expect_identical(
    out$event_description,
    "Repeated narrative. | Repeated narrative."
  )
})

test_that("flatten_maude_record preserves narrative text from simplified shapes", {
  device <- data.frame(
    generic_name = "Ablation Catheter",
    brand_name = "Bravo",
    manufacturer_d_name = "Example Devices",
    stringsAsFactors = FALSE
  )
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
    product_problems = c("Failure to Fire", "Arcing"),
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

test_that("maude_query can handle larger limits", {
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

  dat <- maude_query(search = "PFA", limit = 1000, verbose = FALSE)

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

  dat_filled <- maude_query(
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

test_that("a truncated query warns rather than returning quietly", {
  # `limit` caps the call, not the query. A query matching 41,000 reports and a
  # query matching exactly `limit` of them otherwise come back identical.
  testthat::local_mocked_bindings(
    maude_fda_api_call = function(search_query, limit, skip, ...) {
      out <- tibble::tibble(
        report_number = paste0("RPT-", seq_len(limit)),
        date_received = rep("20260115", limit)
      )
      attr(out, "total") <- 41403L
      out
    },
    .package = "card"
  )

  expect_warning(
    dat <- maude_query(search = "PFA", limit = 10, verbose = FALSE),
    "matched 41403 report"
  )
  expect_identical(attr(dat, "total"), 41403L)

  # No warning when everything that matched came back.
  testthat::local_mocked_bindings(
    maude_fda_api_call = function(search_query, limit, skip, ...) {
      out <- tibble::tibble(report_number = paste0("RPT-", seq_len(limit)))
      attr(out, "total") <- limit
      out
    },
    .package = "card"
  )
  expect_no_warning(maude_query(search = "PFA", limit = 10, verbose = FALSE))
})

test_that("maude_fda_api_call rejects a malformed count field", {
  expect_error(
    maude_fda_api_call("pacemaker", count = c("a", "b")),
    "'count' must be NULL or a single openFDA field name"
  )
  expect_error(
    maude_fda_api_call("pacemaker", count = ""),
    "'count' must be NULL or a single openFDA field name"
  )
})

# openFDA answers an anonymous caller who has exceeded the per-minute rate with
# HTTP 403 and the message "No api_key was supplied", which is indistinguishable
# from a real refusal. Skip rather than fail the suite on it.
skip_if_openfda_refuses <- function(expr) {
  out <- tryCatch(expr, error = function(e) {
    testthat::skip(paste("openFDA unavailable:", conditionMessage(e)))
  })
  out
}

test_that("VALIDATION the openFDA count endpoint returns a term frequency table", {
  skip_if_offline()
  skip_on_cran()

  counts <- skip_if_openfda_refuses(maude_fda_api_call(
    search_query = "device.device_report_product_code:QZI",
    count = "product_problems.exact",
    limit = 999
  ))

  expect_named(counts, c("term", "count"))
  expect_gt(nrow(counts), 1)
  expect_type(counts$count, "integer")
  expect_false(anyNA(counts$term))

  # Counts are of mentions, so they arrive ordered and the leading term is the
  # most frequent rather than the alphabetically first.
  expect_identical(counts$count, sort(counts$count, decreasing = TRUE))
})

test_that("VALIDATION device_problem is populated against the live API", {
  skip_if_offline()
  skip_on_cran()

  # The column was previously read from `device[].device_problem_codes`, which
  # openFDA does not return, so it was NA for all 41,403 rows of a real extract
  # without anything erroring or warning.
  dat <- skip_if_openfda_refuses(suppressWarnings(maude_query(
    search = "device.device_report_product_code:QZI",
    limit = 20,
    verbose = FALSE
  )))

  expect_gt(nrow(dat), 0)
  expect_false(all(is.na(dat$device_problem)))
})

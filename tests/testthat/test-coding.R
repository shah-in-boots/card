test_that("code data sets can be pulled in", {
  # ICD9 and year is 2014
  dat <- get_procedure_codes("icd9", 2014)
  expect_s3_class(dat, "tbl_df")

  # ICD10 and year is 2023
  dat <- get_procedure_codes("icd10", 2023)
  expect_s3_class(dat, "tbl_df")

  # HCPCS and year is 2023
  dat <- get_procedure_codes("hcpcs", 2023)
  expect_s3_class(dat, "tbl_df")

  # CPT and year is 2023
  dat <- get_procedure_codes("cpt", 2023)
  expect_s3_class(dat, "tbl_df")

  # Accept character options as well for year
  dat <- get_procedure_codes("icd9", "2014")
  expect_s3_class(dat, "tbl_df")

  # But do not accept characters that do not convert to years
  expect_error(get_procedure_codes("icd9", "2014-10-01"))

  # THe format must also be acceptable
  expect_error(get_procedure_codes("icd11", 2014))

  # The format and version argument can only accept one scalar each
  expect_error(get_procedure_codes("icd9", c(2014, 2015)))
  expect_error(get_procedure_codes("icd9", c(2014, 2015), "2023"))
  expect_error(get_procedure_codes(c("icd9", "icd10"), 2014))

  # The version must be appropriate for the format
  expect_error(get_procedure_codes("hcpcs", 2014))
  expect_error(get_procedure_codes("icd10", 2014))
  expect_error(get_procedure_codes("cpt", 2014))
})

test_that("procedure_codes still works under its old name but warns once", {
  # Renamed to `get_procedure_codes()` to follow the package's `get_*()`
  # convention; the old name tab-completed next to `complication_definitions`
  # and `maude_complication_index`, both of which are data, and so read as a
  # dataset rather than as an accessor.
  deprecated <- get(".deprecated", envir = asNamespace("card"))
  previously_warned <- deprecated$procedure_codes
  on.exit(deprecated$procedure_codes <- previously_warned, add = TRUE)
  deprecated$procedure_codes <- NULL

  expect_warning(
    dat <- procedure_codes("icd9", 2014),
    "use 'get_procedure_codes\\(\\)'"
  )
  expect_identical(dat, get_procedure_codes("icd9", 2014))

  # Warned once per session rather than once per call, since these are looked up
  # in a loop over codes.
  expect_no_warning(procedure_codes("icd9", 2014))
})

test_that("VALIDATION every bundled procedure code survives parsing", {
  # A parser that drops codes leaves the row count untouched, so the table goes
  # on looking complete. `vroom::vroom()` took the CPT code column for a double
  # from the numeric rows leading the file, and the 13 Category III codes --
  # which end in `T` -- arrived as NA and could not be looked up at all.
  for (format in c("icd9", "icd10", "hcpcs", "cpt")) {
    dat <- get_procedure_codes(format, if (format == "icd9") 2014 else 2023)

    expect_false(anyNA(dat$code))
    expect_true(all(nzchar(dat$code)))

    # The ICD10 codes are read out of a fixed-width field, which pads the 901
    # category headers to seven characters, and both HCPCS fields carry padding
    # ahead of their tab. Either leaves a code no caller can match.
    expect_identical(dat$code, trimws(dat$code))
  }

  cpt <- get_procedure_codes("cpt", 2023)
  expect_true(any(grepl("T$", cpt$code)))

  # Descriptions arrive wrapped in literal quote characters, which were once
  # stripped from the code alone.
  expect_false(any(grepl("\"", cpt$description)))
})

test_that("the format argument is case-insensitive as documented", {
  expect_identical(
    get_procedure_codes("ICD9", 2014),
    get_procedure_codes("icd9", 2014)
  )
})

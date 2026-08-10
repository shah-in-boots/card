# CMS Procedure Codes
# Data sourced from the Centers for Medicare and Medicaid Services
# https://www.cms.gov/medicare/coding-billing
#
# Run this script to rebuild `cms_codes`, the table behind
# `get_procedure_codes()`. It is written to R/sysdata.rda by data-raw/sysdata.R
# rather than here, since `usethis::use_data(internal = TRUE)` rewrites that
# file wholesale and every internal dataset has to be named in one call.
#
# source("data-raw/sysdata.R")

# ICD9 Procedure Codes ----
# Most recent update is from 2014-10-01

# The source drops the trailing zero from a code whose fourth digit is zero, so
# `016` arrives for `01.60`. Padding restores the four-digit form. Checked
# against the 3,661 codes that arrive with four digits already: the 221 padded
# codes collide with none of them and introduce no duplicates.
icd9_2014 <-
  vroom::vroom_lines(file.path("data-raw", "icd9-2014-10-01.txt")) |>
  stringr::str_split_fixed(pattern = " ", n = 2) |>
  tibble::as_tibble(.name_repair = NULL) |>
  dplyr::rename(code = V1, description = V2) |>
  dplyr::mutate(
    code = dplyr::if_else(nchar(code) == 3, paste0(code, "0"), code)
  )

# ICD10 Procedure Codes ----
# Most recent update is from 2023-01-11

# The CMS order file is fixed width: a five-character order number, the code in
# columns 7-13, a validity flag in column 15, a 60-character short description,
# and the long description from column 78 onward. Both extracted fields have to
# be trimmed. The code field is padded out to its full seven characters, so the
# 901 category headers -- the rows whose validity flag is `0` -- arrive as
# "001    " and match no lookup a caller would write.
icd10_2023 <-
  vroom::vroom_lines(file.path("data-raw", "icd10-2023-01-11.txt")) |>
  tibble::as_tibble(.name_repair = "unique") |>
  tibble::tibble(
    code = trimws(stringr::str_sub(value, 7, 13)),
    description = trimws(stringr::str_sub(value, 78, nchar(value)))
  ) |>
  dplyr::mutate(description = tolower(description)) |>
  dplyr::select(-value)

# HCPCS Codes ----
# Most recent update is from 2023-11-29

# `trimws()` above applies to the whole line, which leaves the padding that sits
# inside each field ahead of the tab, so both fields are trimmed again after the
# split. Both also arrive wrapped in literal quote characters; those were
# previously stripped from `code` alone, leaving 79 descriptions reading
# "\"Fluzone vacc, 3 yrs & >, im\"".
hcpcs_2023 <-
  vroom::vroom_lines(file.path("data-raw", "hcpcs-2023-11-29.txt")) |>
  trimws() |>
  stringr::str_split_fixed(pattern = "\t", n = 2) |>
  tibble::as_tibble(.name_repair = NULL) |>
  dplyr::rename(code = V1, description = V2) |>
  dplyr::mutate(
    dplyr::across(c(code, description), \(x) trimws(gsub("\"", "", x)))
  )

# CPT Codes ----
# Most recent update is from 2023-11-29

# `code` has to be declared as character. Left to guess, `vroom::vroom()` takes
# the column for a double from the numeric rows that lead the file, and then the
# 13 Category III codes -- which end in `T`, such as `0051T` and `0585T` -- fail
# to parse and arrive as NA. The row count is unchanged either way, so the table
# still looks complete while those procedures cannot be looked up at all.
cpt_2023 <-
  vroom::vroom(
    file.path("data-raw", "cpt-2023-11-29.txt"),
    col_names = c("category", "code", "description"),
    col_types = vroom::cols(.default = vroom::col_character()),
    delim = "\t"
  ) |>
  dplyr::mutate(description = trimws(description))

# Dataset ----

cms_codes <- list(
  icd9 = list("2014" = icd9_2014),
  icd10 = list("2023" = icd10_2023),
  hcpcs = list("2023" = hcpcs_2023),
  cpt = list("2023" = cpt_2023)
)

# Checks ----

# A parsing change that drops codes leaves the row count untouched, which is how
# the CPT column-type guess went unnoticed. Check the codes themselves.
for (fmt in names(cms_codes)) {
  for (version in names(cms_codes[[fmt]])) {
    codes <- cms_codes[[fmt]][[version]]$code
    stopifnot(
      "a code is missing" = !anyNA(codes),
      "a code is empty" = all(nzchar(codes)),
      "a code carries surrounding whitespace" = identical(codes, trimws(codes))
    )
  }
}

stopifnot(
  "the CPT Category III codes were dropped" =
    any(grepl("T$", cms_codes$cpt[["2023"]]$code))
)

# MAUDE Adverse Event Codes
# Data sourced from FDA Coding Resources for Medical Device Reports
# https://www.fda.gov/medical-devices/mdr-adverse-event-codes/coding-resources-medical-device-reports
#
# Run this script to rebuild `maude_annex_codes`. It is written to R/sysdata.rda
# by data-raw/sysdata.R rather than here.
#
# source("data-raw/sysdata.R")
#
# Annex A release 2024.1, approved 2024-04-24
# Annex E release 2024.1
# Annex F approved 2024-01-18

# Reading ----

# All three annex files share the same first seven columns and then diverge:
# Annex E carries a MedDRA name in column 8 and its CodeHierarchy in 13, the
# other two carry theirs in 11. Naming every column of the file positionally --
# rather than asking `vroom::vroom()` for a subset -- keeps the names attached
# to the columns they were meant for. `col_select` does not: given fewer names
# than the file has columns it applies them to the *selection*, so column 13 of
# Annex E came back named for column 9 and the hierarchy was silently lost.
annex_columns <- function(n, code_hierarchy = 11L, meddra_name = NULL) {
  nms <- paste0("x", seq_len(n))
  nms[1:7] <- c(
    "level_1", "level_2", "level_3",
    "fda_code", "ncit_code", "imdrf_code", "definition"
  )
  nms[code_hierarchy] <- "code_hierarchy"
  if (!is.null(meddra_name)) {
    nms[meddra_name] <- "meddra_name"
  }
  nms
}

# Hierarchy ----

# Every annex file lays its terms out as a staircase: a term sits in the Level 1,
# Level 2 or Level 3 column according to its own depth, and the other two columns
# are blank. So each row carries its own term but says nothing about its
# ancestry.
#
# The ancestry is instead carried by the IMDRF code, which is built by
# concatenation -- A01 is a family, A0101 one of its children, A010101 one of
# theirs -- and spelled out in the file's own CodeHierarchy column as
# "A01|A0101|A010101". Reading it from there is exact.
#
# Filling the level columns downward is the obvious alternative and it is wrong:
# it carries the previous family's last child into the next family row, since a
# family row's Level 2 cell is blank for the good reason that a family has no
# Level 2 term. That gave A02 ("Manufacturing, Packaging or Shipping Problem")
# a `term` of "Implant subsidence", the last Level 2 term of A01, and did the
# same to 25 of the 27 Annex A families.
add_hierarchy <- function(dat, annex) {
  # Each row's own term is whichever level column the file put it in.
  dat <- dplyr::mutate(
    dat,
    annex = annex,
    term = dplyr::coalesce(level_3, level_2, level_1)
  )

  # Ancestor code -> that ancestor's own term, for the lookups below.
  termOfCode <- stats::setNames(dat$term, dat$imdrf_code)

  ancestors <- stringr::str_split(dat$code_hierarchy, stringr::fixed("|"))
  level_at <- function(depth) {
    unname(termOfCode[vapply(
      ancestors,
      function(x) if (length(x) >= depth) x[[depth]] else NA_character_,
      character(1)
    )])
  }

  dat |>
    dplyr::mutate(
      level_1 = level_at(1L),
      level_2 = level_at(2L),
      level_3 = level_at(3L)
    ) |>
    dplyr::select(
      annex, imdrf_code, fda_code, ncit_code, term,
      level_1, level_2, level_3, definition
    ) |>
    dplyr::distinct()
}

# FDA Annex A: Device Problem Codes ----

annex_a <-
  vroom::vroom(
    file.path("data-raw", "FDA-Annex-A.txt"),
    delim = "\t",
    col_names = annex_columns(11L),
    skip = 8,
    show_col_types = FALSE
  ) |>
  add_hierarchy(annex = "A")

# FDA Annex E: Health Effects - Clinical Signs and Symptoms or Conditions ----

# Two files:
# 1. FDA-Annex-E.csv - Code table with hierarchical health effects terms
# 2. FDA-Annex-E-Mapping.txt - Maps IMDRF codes to MedDRA terminology
annex_e_data <-
  vroom::vroom(
    file.path("data-raw", "FDA-Annex-E.csv"),
    col_names = annex_columns(15L, code_hierarchy = 13L, meddra_name = 8L),
    skip = 8,
    show_col_types = FALSE
  ) |>
  dplyr::select(
    level_1:definition, meddra_name, code_hierarchy
  )

# Skip first 4 lines which contain no information (header)
annex_e_mapping <-
  vroom::vroom(
    file.path("data-raw", "FDA-Annex-E-Mapping.txt"),
    delim = "\t",
    skip = 3,
    col_names = c(
      "imdrf_code",
      "meddra_name",
      "unused",
      "meddra_code",
      "meddra_term"
    ),
    show_col_types = FALSE
  ) |>
  dplyr::select(-unused)

# Combine the mapping table with the main Annex E data
# Make sure hte most advanced term falls into the "patient problem column"
# We are defining it here for users
annex_e <-
  dplyr::full_join(
    annex_e_data,
    annex_e_mapping,
    by = c("imdrf_code", "meddra_name"),
    relationship = "many-to-many"
  ) |>
  dplyr::select(-meddra_name, -meddra_code, -meddra_term) |>
  add_hierarchy(annex = "E")

# FDA Annex F ----

annex_f <-
  vroom::vroom(
    file.path("data-raw", "FDA-Annex-F.csv"),
    col_names = annex_columns(11L),
    skip = 8,
    show_col_types = FALSE
  ) |>
  add_hierarchy(annex = "F")

# Checks ----

# The family rows are the ones the fill-downward approach corrupted, and they
# are heavily used: FDA codes a large share of reports at the family level, so
# a wrong `term` on one of these silently drops every report coded to it.
for (dat in list(annex_a, annex_e, annex_f)) {
  families <- dat[nchar(dat$imdrf_code) == 3L, ]
  stopifnot(
    "every row has an ancestry" = !anyNA(dat$level_1),
    "every row has its own term" = !anyNA(dat$term),
    # Annex E is a polyhierarchy -- "Brain Injury" sits under both "Nervous
    # System" and "Injury" -- so a code appears on more than one row. The
    # ancestor lookup takes the first match, which is only safe because a
    # repeated code always carries the same term. Compared after normalising,
    # because E120501 is written both as "Elevated ketones/Diabetic
    # Ketoacidosis" and "Elevated ketones/ Diabetic Ketoacidosis" in the FDA
    # file itself.
    "a code means one term" = !any(duplicated(
      unique(data.frame(
        imdrf_code = dat$imdrf_code,
        term = card:::normalize_maude_terms(dat$term)
      ))$imdrf_code
    )),
    "a family term is its own level 1" = identical(
      families$term,
      families$level_1
    ),
    "a family has no level 2 or 3" = all(is.na(families$level_2)) &&
      all(is.na(families$level_3))
  )
}

# Dataset ----

maude_annex_codes <- list(
  "device_problems" = annex_a,
  "clinical_signs" = annex_e,
  "health_impact" = annex_f
)

# Written to R/sysdata.rda by data-raw/sysdata.R rather than here.
# `usethis::use_data(internal = TRUE)` rewrites that file wholesale, so a second
# script calling it evicts whatever the first one wrote.

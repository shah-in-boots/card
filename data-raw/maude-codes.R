# MAUDE Adverse Event Codes
# Data sourced from FDA Coding Resources for Medical Device Reports
# https://www.fda.gov/medical-devices/mdr-adverse-event-codes/coding-resources-medical-device-reports

# FDA Annex A: Device Problem Codes ----

annex_a <-
  vroom::vroom(
    file.path("data-raw", "FDA-Annex-A.txt"),
    delim = "\t",
    skip = 8,
    col_select = 1:7,
    col_names = c(
      "level_1",
      "level_2",
      "level_3",
      "fda_code",
      "ncit_code",
      "imdrf_code",
      "definition"
    ),
    show_col_types = FALSE
  ) |>
  tidyr::fill(level_1, level_2, .direction = "down") |>
  dplyr::mutate(
    annex = "A",
    term = dplyr::coalesce(level_3, level_2, level_1)
  ) |>
  dplyr::select(
    annex, imdrf_code, fda_code, ncit_code, term,
    level_1, level_2, level_3, definition
  ) |>
  dplyr::distinct()

# FDA Annex E: Health Effects - Clinical Signs and Symptoms or Conditions ----

# Two CSV files:
# 1. FDA-Annex-E.csv - Code table with hierarchical health effects terms
# 2. FDA-Annex-E-Mapping-Table.csv - Maps IMDRF codes to MedDRA terminology

annex_e_data <-
  vroom::vroom(
    file.path("data-raw", "FDA-Annex-E.csv"),
    skip = 8,
    col_select = 1:8,
    col_names = c(
      "level_1",
      "level_2",
      "level_3",
      "fda_code",
      "ncit_code",
      "imdrf_code",
      "definition",
      "meddra_name"
    ),
    show_col_types = FALSE
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
  dplyr::mutate(
    annex = "E",
    term = dplyr::coalesce(level_3, level_2, level_1)
  ) |>
  dplyr::select(
    annex, imdrf_code, fda_code, ncit_code, term,
    level_1, level_2, level_3, definition
  ) |>
  dplyr::distinct()

# FDA Annex F ----
  
annex_f <-
  vroom::vroom(
    file.path("data-raw", "FDA-Annex-F.csv"),
    skip = 8,
    col_select = 1:7,
    col_names = c(
      "level_1",
      "level_2",
      "level_3",
      "fda_code",
      "ncit_code",
      "imdrf_code",
      "definition"
    ),
    show_col_types = FALSE
  ) |>
  dplyr::mutate(
    annex = "F",
    term = dplyr::coalesce(level_3, level_2, level_1)
  ) |>
  dplyr::select(
    annex, imdrf_code, fda_code, ncit_code, term,
    level_1, level_2, level_3, definition
  ) |>
  dplyr::distinct()


# Dataset ----

.maude_codes <- list(
  "device_problems" = annex_a,
  "clinical_signs" = annex_e,
  "health_impact" = annex_f
)

# Create dataset
usethis::use_data(
  .maude_codes, 
  overwrite = TRUE,
  internal = TRUE
)

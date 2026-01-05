# MAUDE Adverse Event Codes
# Data sourced from FDA Coding Resources for Medical Device Reports
# https://www.fda.gov/medical-devices/mdr-adverse-event-codes/coding-resources-medical-device-reports

# FDA Annex E: Health Effects - Clinical Signs and Symptoms or Conditions ----

# Two CSV files:
# 1. FDA-Annex-E.csv - Code table with hierarchical health effects terms
# 2. FDA-Annex-E-Mapping-Table.csv - Maps IMDRF codes to MedDRA terminology

annex_e_data <-
  vroom::vroom(
    system.file("data-raw", "FDA-Annex-E.csv", package = "card"),
    skip = 8,
    col_select = 1:8,
    col_names = c(
      "level_1_term",
      "level_2_term",
      "level_3_term",
      "fda_code",
      "ncit_code",
      "imdrf_code",
      "definition",
      "meddra_name"
    )
  ) |>
  tidyr::fill(level_1_term, .direction = "down") |>
  tidyr::fill(level_2_term, .direction = "down")

# Skip first 4 lines which contain no information (header)
annex_e_mapping <-
  vroom::vroom(
    system.file("data-raw", "FDA-Annex-E-Mapping.txt", package = "card"),
    delim = "\t",
    skip = 3,
    col_names = c(
      "imdrf_code",
      "meddra_name",
      "unused",
      "meddra_code",
      "meddra_term"
    )
  ) |>
  dplyr::select(-unused)

# Combine the mapping table with the main Annex E data
annex_e <-
  dplyr::full_join(
    annex_e_data,
    annex_e_mapping,
    by = c("imdrf_code", "meddra_name"),
    relationship = "many-to-many"
  ) |>
  dplyr::select(-meddra_name)

# Dataset ----

.maude_codes <- list(
  "annex_e" = annex_e
)

# Create dataset
usethis::use_data(
  .maude_codes, 
  overwrite = TRUE,
  internal = FALSE
)

# Code to prepare `coding-data` dataset goes here

# ICD9 Procedure codes
# Most recent update is from 2014-10-01
icd9 <-
	vroom::vroom_lines(system.file('data-raw', 'icd9-2014-10-01.txt', package = 'card')) |>
	stringr::str_split_fixed(pattern = ' ', n = 2) |>
	tibble::as_tibble(.name_repair = NULL) |>
	dplyr::rename(code = V1, description = V2) |>
	dplyr::mutate(code = dplyr::if_else(nchar(code) == 3, paste0(code, '0'), code))

# ICD10 Procedure Codes
# Most recent update is from 2023-01-11
icd10 <-
	vroom::vroom_lines(system.file('data-raw', 'icd10-2023-01-11.txt', package = 'card')) |>
	tibble::as_tibble(.name_repair = 'unique') |>
	tibble::tibble(
		code = stringr::str_sub(value, 7, 13),
		description = stringr::str_sub(value, 78, nchar(value))
	) |>
	dplyr::mutate(description = tolower(description)) |>
	dplyr::select(-value)

# HCPCS codes
# Most recent update is from 2023-11-29
hcpcs <-
	vroom::vroom_lines(system.file('data-raw', 'hcpcs-2023-11-29.txt', package = 'card')) |>
	trimws() |>
	stringr::str_split_fixed(pattern = '\t', n = 2) |>
	tibble::as_tibble(.name_repair = NULL) |>
	dplyr::rename(code = V1, description = V2) |>
	dplyr::mutate(description = gsub('\\t', '', description)) |>
	dplyr::mutate(code = gsub('"', '', code)) |>
	dplyr::mutate(code = gsub(' ', '', code))

# CPT codes
# Most recent update is from 2023-11-29
cpt <-
	vroom::vroom(system.file('data-raw', 'cpt-2023-11-29.txt', package = 'card'), col_names = c('category', 'code', 'description'), delim = '\t', show_col_types = FALSE)

# Save all this data to the package as internal data, compressed
usethis::use_data(icd9, icd10, hcpcs, cpt, overwrite = TRUE, internal = TRUE)

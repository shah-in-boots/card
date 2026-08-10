# Internal Datasets
#
# Run this script to regenerate R/sysdata.rda
#
# source("data-raw/sysdata.R")
#
# `usethis::use_data(internal = TRUE)` rewrites R/sysdata.rda wholesale rather
# than adding to it, so exactly one script may call it and that call has to name
# every internal dataset. This is how `.cms_codes` was lost: it was written from
# data-raw/cms-codes.R, and the MAUDE annex codes were later saved from their
# own script, silently evicting it. `get_procedure_codes()` then errored for
# every format and version until the object was rebuilt here.
#
# Add new internal data by sourcing its builder below and naming it in the
# `use_data()` call. Data meant to be visible to users belongs in data/ instead,
# written with `internal = FALSE` from its own script.

source("data-raw/cms-codes.R")
source("data-raw/maude-annex-codes.R")

usethis::use_data(
  cms_codes,
  maude_annex_codes,
  overwrite = TRUE,
  internal = TRUE
)

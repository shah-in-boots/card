# MAUDE Free-Text Normalisation Tables
#
# Run this script to regenerate data/maude_manufacturer_index.rda,
# data/maude_ownership.rda and data/maude_ablation_index.rda, and to rewrite
# data-raw/maude-entities/coverage.md
#
# source("data-raw/maude-entities.R")
#
# The curated content lives in CSVs under data-raw/maude-entities/ rather than
# in this file, so that a diff shows one fact per line, so that the tables can
# be edited in a spreadsheet by someone who does not write R, and so that the
# script stays a reader rather than a repository. `data-raw/complications.R` is
# the counter-example: 1,141 lines of R holding curated content.
#
# # The two layers
#
# `*-patterns.csv` answers "which entity is this string?" and changes when MAUDE
# spelling varies. `ownership.csv` answers "who owned that entity, and when?"
# and changes when a deal closes. Keeping them apart is the point: an
# acquisition is one row in `ownership.csv` and nothing else, and an acquisition
# of a parent carries every entity beneath it automatically.
#
# Three rules keep `ownership.csv` from becoming a corporate database:
#
#   1. An entity earns a row only if a pattern table names it, or if it sits on
#      a chain between one and its parent. Nothing speculative.
#   2. An ownership row requires a date that can be stated. A relationship that
#      is real but undated -- Stockert building generators for Biosense Webster,
#      Venusa building under contract for Abbott -- resolves at the pattern
#      layer instead, by mapping the string straight to the entity whose product
#      it is. Same for plants: MPRI is not a company, it is Medtronic.
#   3. The chain stops at the operating company the field would name. ZOLL has
#      been an Asahi Kasei subsidiary since 2012, and nobody calls it an Asahi
#      Kasei defibrillator.
#
# # Precedence
#
# Both pattern tables carry a `priority` column, low first, defaulting to 100.
# Only a row that exists to pre-empt another needs a different value:
# `CRYOCATH` at 10 because reports arrive as "MEDTRONIC CRYOCATH LP" and the
# plain `MEDTRONIC` pattern would otherwise claim them. Declaring it in the row
# means the CSV can be sorted alphabetically for review without changing what
# it does.
#
# # Adding a device domain
#
# A future domain -- CIEDs, valves, stents -- is two more CSVs on the same
# shape: `<domain>-patterns.csv` of `pattern, platform, priority`, and
# `<domain>-platforms.csv` of `platform, <facts...>, entity, note, source`.
# They reuse `read_index()` below, `ownership.csv`, and `match_maude_index()`,
# and need no change to R/maude-normalize.R.

# Reading ----

source_dir <- file.path("data-raw", "maude-entities")

#' Read one curated CSV and check it carries the columns it must
read_index <- function(file, required) {
  dat <- vroom::vroom(
    file.path(source_dir, file),
    delim = ",",
    col_types = vroom::cols(.default = vroom::col_character()),
    show_col_types = FALSE
  )

  missing_columns <- setdiff(required, names(dat))
  if (length(missing_columns) > 0) {
    stop(
      file, " is missing the column(s) ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  # Empty cells arrive as NA and mean "not stated" everywhere in these files.
  # Trim first, so a stray space does not become a distinct entity name.
  dat[] <- lapply(dat, function(x) {
    x <- trimws(x)
    x[!nzchar(x)] <- NA_character_
    x
  })

  dat
}

manufacturer_patterns <- read_index(
  "manufacturer-patterns.csv",
  c("pattern", "entity", "priority", "note", "source")
)

ownership_rows <- read_index(
  "ownership.csv",
  c("entity", "parent", "from", "to", "note", "source")
)

ablation_patterns <- read_index(
  "ablation-patterns.csv",
  c("pattern", "platform", "priority")
)

ablation_platforms <- read_index(
  "ablation-platforms.csv",
  c("platform", "modality", "entity", "note", "source")
)

# Assembly ----

# Priority orders the table; file order breaks ties, so a group of equal-priority
# rows keeps whatever order the CSV was left in.
by_priority <- function(dat) {
  priority <- as.integer(dat$priority)
  if (anyNA(priority)) {
    stop("every 'priority' must be an integer", call. = FALSE)
  }
  dat[order(priority, seq_len(nrow(dat))), ]
}

maude_manufacturer_index <-
  manufacturer_patterns |>
  by_priority() |>
  dplyr::mutate(priority = as.integer(priority)) |>
  dplyr::select(pattern, entity, priority, note, source)

# Parsed before anything reorders the rows, so a date that does not parse is
# reported against the string it came from.
as_stated_date <- function(x, column) {
  parsed <- suppressWarnings(as.Date(x))
  unparseable <- !is.na(x) & is.na(parsed)
  if (any(unparseable)) {
    stop(
      "ownership.csv has ", sum(unparseable), " unparseable '", column,
      "' value(s): ", paste(unique(x[unparseable]), collapse = ", "),
      ". Dates must be written YYYY-MM-DD.",
      call. = FALSE
    )
  }
  parsed
}

maude_ownership <-
  ownership_rows |>
  dplyr::mutate(
    from = as_stated_date(from, "from"),
    to = as_stated_date(to, "to")
  ) |>
  dplyr::arrange(entity, from) |>
  dplyr::select(entity, parent, from, to, note, source)

# The two ablation CSVs are an authoring convenience, joined here and shipped as
# one table. One brand alias per row in the pattern file means adding a spelling
# is a one-line diff rather than an edit inside a seven-way alternation.
maude_ablation_index <-
  ablation_patterns |>
  by_priority() |>
  dplyr::mutate(priority = as.integer(priority)) |>
  dplyr::left_join(ablation_platforms, by = "platform") |>
  dplyr::select(pattern, platform, modality, entity, priority, note, source)

# Structural checks ----

# These run offline and are duplicated as package tests in
# tests/testthat/test-maude-entities.R, so a bad edit fails `R CMD check` and
# not only a manual run of this script.
pkgload::load_all(".", quiet = TRUE)

known_modalities <- c(
  "cryoablation", "radiofrequency", "pulsed field",
  "pulsed field or radiofrequency", "laser", NA_character_
)

stopifnot(
  "no duplicate manufacturer patterns" =
    !anyDuplicated(maude_manufacturer_index$pattern),
  "no duplicate ablation patterns" =
    !anyDuplicated(maude_ablation_index$pattern),
  "no duplicate platform definitions" =
    !anyDuplicated(ablation_platforms$platform),
  "every ablation pattern names a defined platform" =
    !anyNA(maude_ablation_index$entity),
  "every modality is a known value" =
    all(maude_ablation_index$modality %in% known_modalities),
  "an ownership interval starts before it ends" = all(
    is.na(maude_ownership$from) | is.na(maude_ownership$to) |
      maude_ownership$from < maude_ownership$to
  )
)

# An entity owned by two parents at once would make `parent_of()` pick one
# silently, so the intervals per entity must not overlap.
overlapping <- vapply(
  split(maude_ownership, maude_ownership$entity),
  function(rows) {
    if (nrow(rows) < 2L) {
      return(FALSE)
    }
    starts <- dplyr::coalesce(rows$from, as.Date("1900-01-01"))
    ends <- dplyr::coalesce(rows$to, as.Date("2999-12-31"))
    any(outer(starts, ends, `<`) & outer(ends, starts, `>`) &
          !diag(TRUE, nrow(rows)))
  },
  logical(1)
)
stopifnot("no entity has two owners at once" = !any(overlapping))

# Referential integrity: every entity either has an ownership row, is named as
# a parent, or is terminal on its own. What must not happen is an index naming
# an entity nobody has ever heard of, which is usually a typo.
referenced <- sort(unique(c(
  maude_manufacturer_index$entity,
  maude_ablation_index$entity
)))
known <- sort(unique(c(maude_ownership$entity, maude_ownership$parent)))

# An ownership row earns its place if an index names its entity, or if another
# row names it as a parent -- BTG is in the table only because Galil Medical
# passed through it on the way to Boston Scientific, and a chain intermediate is
# not an orphan.
orphan_owners <- sort(unique(setdiff(
  maude_ownership$entity,
  c(referenced, maude_ownership$parent)
)))

# Ownership must resolve for every entity it knows about, at both ends of time.
for (when in as.Date(c("1992-01-01", Sys.Date()))) {
  invisible(resolve_maude_owner(
    known,
    as_of = as.Date(when, origin = "1970-01-01"),
    ownership = maude_ownership
  ))
}

# Coverage ----

# Set `check_maude_coverage <- FALSE` before sourcing to skip the API calls and
# keep whatever coverage.md already says.
if (!exists("check_maude_coverage") || isTRUE(check_maude_coverage)) {

  measure <- function(counts, index, stream) {
    matched <- match_maude_index(counts$term, index$pattern)
    missed <- counts[is.na(matched), ]

    list(
      stream = stream,
      mentions = sum(counts$count),
      matched = sum(counts$count[!is.na(matched)]) / sum(counts$count),
      used = sort(unique(matched)),
      missed = utils::head(missed, 10L)
    )
  }

  ablation_query <- paste0(
    "device.device_report_product_code:",
    "(QZI OR OAE OR LPB OR OAD OR MTD OR NIY OR OCL)"
  )

  # Cardiovascular manufacturers, by mention. The count endpoint caps at 999
  # terms, which is the head of a much longer tail.
  manufacturer_cv <- measure(
    maude_fda_api_call(
      search_query = "device.openfda.medical_specialty_description:Cardiovascular",
      count = "device.manufacturer_d_name.exact",
      limit = 999
    ),
    maude_manufacturer_index,
    "manufacturers, cardiovascular"
  )

  # The ablation specialists are far below that cap -- CardioFocus and Farapulse
  # do not appear in a list led by defibrillator and monitor makers -- so
  # measure the ablation stream separately or their patterns look unused when
  # they are simply out of frame.
  manufacturer_ablation <- measure(
    maude_fda_api_call(
      search_query = ablation_query,
      count = "device.manufacturer_d_name.exact",
      limit = 999
    ),
    maude_manufacturer_index,
    "manufacturers, cardiac ablation"
  )

  brands <- measure(
    maude_fda_api_call(
      search_query = ablation_query,
      count = "device.brand_name.exact",
      limit = 999
    ),
    maude_ablation_index,
    "ablation brands"
  )

  measured_on <- Sys.Date()

  never_fired_manufacturer <- setdiff(
    maude_manufacturer_index$pattern,
    maude_manufacturer_index$pattern[
      union(manufacturer_cv$used, manufacturer_ablation$used)
    ]
  )
  never_fired_brand <- setdiff(
    maude_ablation_index$pattern,
    maude_ablation_index$pattern[brands$used]
  )

  attr(maude_manufacturer_index, "coverage") <- tibble::tibble(
    stream = c(manufacturer_cv$stream, manufacturer_ablation$stream),
    mentions = c(manufacturer_cv$mentions, manufacturer_ablation$mentions),
    matched = c(manufacturer_cv$matched, manufacturer_ablation$matched),
    measured_on = measured_on
  )
  attr(maude_ablation_index, "coverage") <- tibble::tibble(
    stream = brands$stream,
    mentions = brands$mentions,
    matched = brands$matched,
    measured_on = measured_on
  )

  # coverage.md is committed. A pull request that edits a CSV shows its effect
  # here -- coverage moving, a string stopping falling through -- without the
  # reviewer having to run anything.
  bullet_counts <- function(x) {
    if (nrow(x) == 0) {
      return("- none")
    }
    paste0("- `", x$term, "` (", format(x$count, big.mark = ",", trim = TRUE), ")")
  }
  bullet_patterns <- function(x) {
    if (length(x) == 0) {
      return("- none")
    }
    paste0("- `", x, "`")
  }

  writeLines(
    c(
      "# MAUDE entity table coverage",
      "",
      paste0(
        "Measured against the openFDA `count` endpoint on ",
        format(measured_on, "%Y-%m-%d"),
        ". Regenerate with `source(\"data-raw/maude-entities.R\")`."
      ),
      "",
      "| stream | mentions | matched |",
      "|---|---:|---:|",
      vapply(
        list(manufacturer_cv, manufacturer_ablation, brands),
        function(m) {
          sprintf(
            "| %s | %s | %.1f%% |",
            m$stream, format(m$mentions, big.mark = ",", trim = TRUE), 100 * m$matched
          )
        },
        character(1)
      ),
      "",
      "## Patterns that never fired",
      "",
      "A pattern matching none of the 999 terms the count endpoint returns per",
      "stream is a typo, an entity that does not report, or one whose strings",
      "sit below the cut-off: `CAMERON HEALTH` names 2,770 reports and still",
      "lands here. Check with a direct search before dropping a row.",
      "",
      "### Manufacturers",
      "",
      bullet_patterns(never_fired_manufacturer),
      "",
      "### Ablation brands",
      "",
      bullet_patterns(never_fired_brand),
      "",
      "## Ownership rows nothing references",
      "",
      bullet_patterns(orphan_owners),
      "",
      "## Ownership rows without a source",
      "",
      paste0(
        "The closing dates below were recorded from general knowledge rather ",
        "than from a citation, and their `note` says how precise each one is. ",
        "A row here is not wrong, only unverified."
      ),
      "",
      bullet_patterns(
        sort(unique(maude_ownership$entity[is.na(maude_ownership$source)]))
      ),
      "",
      "## Largest unmatched strings",
      "",
      "Where the next rows should come from, if anywhere.",
      "",
      "### Manufacturers, cardiovascular",
      "",
      bullet_counts(manufacturer_cv$missed),
      "",
      "### Manufacturers, cardiac ablation",
      "",
      bullet_counts(manufacturer_ablation$missed),
      "",
      "### Ablation brands",
      "",
      bullet_counts(brands$missed)
    ),
    file.path(source_dir, "coverage.md")
  )

  message(paste(
    sprintf(
      "%s: %.1f%% of %s mentions matched",
      c(manufacturer_cv$stream, manufacturer_ablation$stream, brands$stream),
      100 * c(manufacturer_cv$matched, manufacturer_ablation$matched, brands$matched),
      format(
        c(manufacturer_cv$mentions, manufacturer_ablation$mentions, brands$mentions),
        big.mark = ",", trim = TRUE
      )
    ),
    collapse = "\n"
  ))
}

# Data saving ----

usethis::use_data(
  maude_manufacturer_index,
  maude_ownership,
  maude_ablation_index,
  overwrite = TRUE
)

# MAUDE Free-Text Normalisation ----

# MAUDE stores the manufacturer and the brand as whatever the reporter typed,
# and neither field is a controlled vocabulary. Bundled tables map the strings
# that actually occur onto canonical values, and one internal matcher applies
# all of them, because the matching policy is the interesting part and it should
# not be written twice.
#
# The policy is first-match-wins over a list ordered by an explicit `priority`
# column. That is deliberately simpler than scoring every pattern and taking the
# best: a scored match is unpredictable when the table grows, whereas an ordered
# one is read top to bottom. Priority is a column rather than the row's position
# so that the source CSV can be sorted for review without changing behaviour --
# `CRYOCATH` has to be tested before `MEDTRONIC`, and saying so in the row is
# better than relying on a line number that a spreadsheet will happily move.
#
# The work is split in two, along a line the package's naming already draws.
# `normalize_*()` canonicalises a string, which does not depend on a date:
# "MEDTRONIC CRYOCATH LP" has always named CryoCath. `resolve_*()` settles one
# value from several sources, and here that is corporate ownership, which does
# depend on a date and says so by requiring one.

#' Find the first index row whose pattern matches each string
#'
#' @description Internal matcher shared by [normalize_maude_manufacturer()] and
#'   [normalize_maude_ablation()]. Walks the index in order and assigns each
#'   input the first pattern that matches it, so a table sorted by `priority`
#'   resolves ambiguity by declaration rather than by scoring.
#'
#' @param x Character vector of raw MAUDE strings.
#' @param patterns Character vector of regular expressions, in priority order.
#'
#' @return An integer vector the same length as `x`, giving the index of the
#'   matching pattern, or `NA_integer_` where nothing matched.
#'
#' @keywords internal
#' @noRd
match_maude_index <- function(x, patterns) {
  out <- rep(NA_integer_, length(x))
  x <- as.character(x)

  for (i in seq_along(patterns)) {
    # Only rows still unassigned are tested, which is what makes the first
    # match win and stops a later general pattern overwriting a specific one.
    pending <- which(is.na(out) & !is.na(x))
    if (!length(pending)) {
      break
    }

    hit <- grepl(patterns[[i]], x[pending], ignore.case = TRUE, perl = TRUE)
    out[pending[hit]] <- i
  }

  out
}

#' Find each entity's immediate parent on a given date
#'
#' @description Internal helper for [resolve_maude_owner()]. Looks up one hop of
#'   the ownership chain, taking the row whose date interval contains `when`.
#'
#' @param child Character vector of entity names.
#' @param when Date vector the same length as `child`.
#' @param ownership Ownership table with `entity`, `parent`, `from` and `to`.
#'
#' @return A character vector of parents, `NA_character_` where the entity has
#'   no owner on that date, which is how an entity that owns itself is
#'   expressed.
#'
#' @keywords internal
#' @noRd
parent_of <- function(child, when, ownership) {
  out <- rep(NA_character_, length(child))

  # Intervals are validated as non-overlapping per entity when the table is
  # built, so taking the first matching row cannot hide an ambiguity.
  for (i in seq_len(nrow(ownership))) {
    hit <- is.na(out) &
      !is.na(child) & child == ownership$entity[[i]] &
      (is.na(ownership$from[[i]]) | when >= ownership$from[[i]]) &
      (is.na(ownership$to[[i]]) | when < ownership$to[[i]])
    out[which(hit)] <- ownership$parent[[i]]
  }

  out
}

#' Normalise MAUDE Manufacturer and Device Strings
#'
#' @description
#' `normalize_maude_manufacturer()` maps the `manufacturer_name` returned by
#' [maude_query()] onto the entity that made the device, collapsing plants,
#' spelling variants, and contract manufacturers onto one name.
#'
#' `normalize_maude_ablation()` maps a cardiac ablation `device_brand_name` onto
#' its platform, the energy modality it delivers, and the entity that makes it.
#'
#' `resolve_maude_owner()` follows an entity up its chain of acquisitions to the
#' company that owned it on a given date.
#'
#' @details
#' All three are lookups over bundled tables, and all three return `NA` rather
#' than a guess for a value the tables do not cover. See
#' [maude_manufacturer_index], [maude_ablation_index] and [maude_ownership] for
#' measured coverage and for how to extend them.
#'
#' # Why normalising and resolving are separate
#'
#' Which company a string names does not change with time. Who owns that company
#' does. Keeping them apart is what lets one acquisition be one row rather than
#' a rewrite of every pattern beneath it, and it is why `resolve_maude_owner()`
#' requires `as_of` while the two `normalize_*()` functions take no date at all:
#'
#' ```r
#' events |>
#'   dplyr::mutate(
#'     entity  = normalize_maude_manufacturer(manufacturer_name),
#'     company = resolve_maude_owner(entity, as_of = Sys.Date()),
#'     at_time = resolve_maude_owner(entity, as_of = date_received)
#'   )
#' ```
#'
#' `company` counts every St. Jude report as Abbott, which is what a question
#' about today's market wants. `at_time` counts a 2010 report as St. Jude, which
#' is what a question about who was shipping the device then wants. Neither is a
#' default, because picking one silently would answer a question the caller did
#' not ask.
#'
#' # Why the manufacturer needs normalising
#'
#' `manufacturer_name` is the manufacturing entity, not the company. Counting
#' the raw strings splits a single maker across its plants -- Biosense Webster
#' reports arrive under at least four -- and hides a company behind its contract
#' manufacturer, as Abbott is behind `"VENUSA DE MEXICO S.A. DE C.V."`.
#'
#' # Why the modality needs the brand
#'
#' Product code does not determine ablation modality. `OAE` covers cryoablation
#' and radiofrequency alike, so a rule that resolves it from the product code
#' and falls back to radiofrequency will put cryoballoon, pulsed-field, and
#' laser devices into a radiofrequency arm. Deriving the modality from the brand
#' is what separates them.
#'
#' Where a study already assigns a modality from the product code, the useful
#' move is to compare the two rather than to silently prefer one:
#'
#' ```r
#' events |>
#'   dplyr::mutate(
#'     from_brand = normalize_maude_ablation(device_brand_name)$modality
#'   ) |>
#'   dplyr::count(from_product_code, from_brand)
#' ```
#'
#' Every off-diagonal cell is a report whose modality depends on which rule was
#' used, and the `NA` column is the reports neither rule can place -- including
#' the mapping and access catheters that share the ablation product codes and
#' deliver no energy at all.
#'
#' @param manufacturer_name Character vector of raw MAUDE manufacturer strings,
#'   as returned in the `manufacturer_name` column of [maude_query()].
#'
#' @param device_brand_name Character vector of raw MAUDE brand strings, as
#'   returned in the `device_brand_name` column of [maude_query()].
#'
#' @param entity Character vector of entity names, as returned by
#'   `normalize_maude_manufacturer()` or in the `entity` column of
#'   `normalize_maude_ablation()`.
#'
#' @param as_of Date vector giving the point in time to resolve ownership at,
#'   recycled against `entity`. There is deliberately no default: the answer
#'   changes with it, and a `Sys.Date()` default would make the same code return
#'   different results after a future acquisition without anything in the call
#'   having changed. Pass `Sys.Date()` for the company that owns the entity now,
#'   or a report's `date_received` for the company that owned it when the report
#'   was filed.
#'
#' @param index Lookup table to match against, defaulting to
#'   [maude_manufacturer_index] and [maude_ablation_index] respectively. Supply
#'   your own to extend or override the bundled tables; it must carry a
#'   `pattern` column of regular expressions in priority order, plus the value
#'   columns the function returns.
#'
#' @param ownership Ownership table to walk, defaulting to [maude_ownership].
#'   Must carry `entity`, `parent`, `from` and `to` columns, the last two being
#'   `Date` and allowed to be missing for an unbounded interval.
#'
#' @return `normalize_maude_manufacturer()` returns a character vector the same
#'   length as `manufacturer_name`, holding the entity name or `NA` where the
#'   index does not cover the string.
#'
#'   `resolve_maude_owner()` returns a character vector the same length as
#'   `entity`, holding the company that owned it on `as_of`. An entity that
#'   nobody owned on that date resolves to itself, which is how a divestiture is
#'   expressed: Physio-Control resolves to Medtronic in 2005, to itself in 2014,
#'   and to Stryker in 2020.
#'
#'   `normalize_maude_ablation()` returns a `tbl_df` with one row per input and
#'   the columns:
#'   \describe{
#'     \item{device_brand_name}{The input string, unchanged}
#'     \item{platform}{The device platform, such as `"Arctic Front"`}
#'     \item{modality}{One of `"cryoablation"`, `"radiofrequency"`,
#'       `"pulsed field"`, `"pulsed field or radiofrequency"`, or `"laser"`.
#'       `NA` both for an unmatched brand and for a matched catheter that
#'       delivers no energy, such as a mapping catheter -- check `platform` to
#'       tell the two apart}
#'     \item{entity}{The entity that makes the platform. Pass it to
#'       `resolve_maude_owner()` for the corporate parent}
#'   }
#'
#' @seealso [maude_manufacturer_index], [maude_ablation_index] and
#'   [maude_ownership] for the bundled lookups, and [maude_query()] for the
#'   columns these read.
#'
#' @examples
#' # Plants, spelling variants, and a contract manufacturer collapse onto one
#' # entity, without anyone having to say when
#' normalize_maude_manufacturer(c(
#'   "BOSTON SCIENTIFIC CORPORATION",
#'   "FARAPULSE, INC.",
#'   "BIOSENSE WEBSTER, INC. (JUAREZ)",
#'   "VENUSA DE MEXICO S.A. DE C.V.",
#'   "SOME COMPANY NOT IN THE INDEX"
#' ))
#'
#' # Ownership is the part that needs a date
#' resolve_maude_owner(
#'   "Farapulse",
#'   as_of = as.Date(c("2019-01-01", "2024-01-01"))
#' )
#'
#' # A two-hop chain, resolved rather than flattened
#' resolve_maude_owner(
#'   "Telectronics Pacing Systems",
#'   as_of = as.Date(c("2000-01-01", "2020-01-01"))
#' )
#'
#' # A divestiture, which a single-parent table cannot express
#' resolve_maude_owner(
#'   "Physio-Control",
#'   as_of = as.Date(c("2005-01-01", "2014-01-01", "2020-01-01"))
#' )
#'
#' # Modality comes from the brand, not the product code
#' normalize_maude_ablation(c(
#'   "ARCTIC FRONT ADVANCE PRO CARDIAC CRYOABLATION CATHETER",
#'   "POLARX FIT",
#'   "FARAWAVE PULSED FIELD ABLATION CATHETER",
#'   "THERMOCOOL SMARTTOUCH SF",
#'   "PENTARAY NAV",
#'   "ARCTIC SUN 5000"
#' ))
#'
#' @name maude_normalization
NULL

#' @rdname maude_normalization
#' @export
normalize_maude_manufacturer <- function(
  manufacturer_name,
  index = maude_manufacturer_index
) {
  if (!is.character(manufacturer_name) && !all(is.na(manufacturer_name))) {
    stop("'manufacturer_name' must be a character vector", call. = FALSE)
  }
  if (!is.data.frame(index) || !all(c("pattern", "entity") %in% names(index))) {
    stop(
      "'index' must be a data frame with 'pattern' and 'entity' columns",
      call. = FALSE
    )
  }

  index$entity[match_maude_index(manufacturer_name, index$pattern)]
}

#' @rdname maude_normalization
#' @export
normalize_maude_ablation <- function(
  device_brand_name,
  index = maude_ablation_index
) {
  if (!is.character(device_brand_name) && !all(is.na(device_brand_name))) {
    stop("'device_brand_name' must be a character vector", call. = FALSE)
  }
  required <- c("pattern", "platform", "modality", "entity")
  if (!is.data.frame(index) || !all(required %in% names(index))) {
    stop(
      "'index' must be a data frame with 'pattern', 'platform', 'modality' ",
      "and 'entity' columns",
      call. = FALSE
    )
  }

  matched <- match_maude_index(device_brand_name, index$pattern)

  tibble::tibble(
    device_brand_name = as.character(device_brand_name),
    platform = index$platform[matched],
    modality = index$modality[matched],
    entity = index$entity[matched]
  )
}

#' @rdname maude_normalization
#' @export
resolve_maude_owner <- function(entity, as_of, ownership = maude_ownership) {
  # The message names the two things a caller is actually choosing between,
  # because the cost of a required argument is someone who does not know what
  # to pass, and "must be a Date" would not tell them.
  if (missing(as_of)) {
    stop(
      "'as_of' must be given, as a Date. There is no default because the ",
      "answer changes with it: use 'as_of = Sys.Date()' for the company that ",
      "owns the entity now, or 'as_of = <the report's date_received>' for the ",
      "company that owned it when the report was filed.",
      call. = FALSE
    )
  }
  if (!inherits(as_of, "Date")) {
    as_of <- tryCatch(
      as.Date(as_of),
      error = function(e) {
        stop(
          "'as_of' must be a Date, or something 'as.Date()' accepts",
          call. = FALSE
        )
      }
    )
  }
  if (!is.data.frame(ownership) ||
      !all(c("entity", "parent", "from", "to") %in% names(ownership))) {
    stop(
      "'ownership' must be a data frame with 'entity', 'parent', 'from' and ",
      "'to' columns",
      call. = FALSE
    )
  }

  entity <- as.character(entity)
  n <- max(length(entity), length(as_of))
  if (n > 0 && (length(entity) == 0 || length(as_of) == 0)) {
    return(character(0))
  }
  if (length(entity) != n) {
    entity <- rep_len(entity, n)
  }
  if (length(as_of) != n) {
    as_of <- rep_len(as_of, n)
  }

  # Walk the whole vector one hop at a time rather than one entity at a time:
  # chains are short and the lookup is vectorised, so this costs a handful of
  # passes over 30-odd rows.
  max_depth <- 20L
  current <- entity
  for (step in seq_len(max_depth)) {
    parent <- parent_of(current, as_of, ownership)
    if (!any(!is.na(parent))) {
      return(current)
    }
    current[!is.na(parent)] <- parent[!is.na(parent)]
  }

  # Still moving after 20 hops means the table contains a cycle. Name the
  # entities it caught, since a chain that long is otherwise invisible.
  stuck <- unique(current[!is.na(parent_of(current, as_of, ownership))])
  stop(
    "'ownership' does not resolve within ", max_depth, " steps, which means it ",
    "contains a cycle. Still resolving: ", paste(stuck, collapse = ", "),
    call. = FALSE
  )
}

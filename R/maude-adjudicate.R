#' Matching MAUDE terms to complications
#'
#' @description
#' `maude_term_to_complication()` groups MAUDE problem terms into the
#' complication categories defined by a user-supplied matching index. Input
#' terms are normalized before matching, so differences in capitalization,
#' punctuation, and spacing do not affect the result.
#'
#' A term may appear in more than one complication category if the underlying
#' MAUDE index overlaps across categories. Returned values preserve the original
#' user-supplied terms rather than the normalized versions used for matching.
#'
#' @param term Character vector of MAUDE problem terms to group into
#'   complications.
#' @param definitions Named list of complication definitions. Names
#'   should be complication identifiers, and each element must contain at least
#'   a `"definition"` entry. If you want to use the package defaults, pass
#'   [complication_definitions] explicitly.
#' @param index Named list mapping complication identifiers to
#'   normalized MAUDE problem terms. Names must be a subset of
#'   `complication_definitions`, with optional `"not_indexed"` allowed as a
#'   residual bucket. If you want to use the package defaults, pass
#'   [maude_complication_index] explicitly.
#'
#' @return A named list. Each list name is a complication identifier and each
#'   element is a character vector of the original input terms that matched that
#'   complication. Empty complication groups are omitted.
#'
#' @examples
#' maude_term_to_complication(c(
#'   "Cardiac Tamponade",
#'   "Low blood pressure / hypotension",
#'   "No Health Consequences or Impact"
#' ),
#' definitions = complication_definitions,
#' index = maude_complication_index)
#'
#' @export
maude_term_to_complication <- function(
  term,
  definitions,
  index
) {
  # Validate the incoming MAUDE problem terms before any matching happens.
  if (!is.character(term)) {
    stop("'term' must be a character vector", call. = FALSE)
  }
  if (anyNA(term)) {
    stop("'term' must not contain missing values", call. = FALSE)
  }
  if (length(term) == 0) {
    return(setNames(list(), character()))
  }

  # Confirm the supplied complication metadata and index line up with each other.
  validate_complication_definitions(definitions)
  validate_complication_index(
    index,
    names(definitions)
  )

  # Normalize the incoming terms once so matching is robust to spacing and punctuation.
  normalized_terms <- normalize_maude_terms(term)

  # Match original user-supplied terms back to each complication bucket.
  matches <- lapply(index, function(x) {
    term[normalized_terms %in% normalize_maude_terms(x)]
  })

  # Drop empty buckets so the output only contains observed complication matches.
  # Drop empty spots so its only observed complications
  matches[lengths(matches) > 0]
}

# MAUDE helper functions -------------------------------------------------------

#' Utility function to help manage problem terms such they are normalized and spacing or common typos, capitalizations, etc., don't become an issue
#' @keywords internal
normalize_maude_terms <- function(terms) {
  terms |>
    tolower() |>
    gsub(pattern = "[^a-z0-9]+", replacement = " ", x = _) |>
    trimws()
}

#' @description Internal helper to validate that complication definitions are a
#'   named list and that each definition includes a `"definition"` field.
#' @keywords internal
validate_complication_definitions <- function(definitions) {
  validate_named_list(definitions)

  has_definition <- vapply(
    definitions,
    function(x) is.list(x) && "definition" %in% names(x),
    logical(1)
  )

  if (!all(has_definition)) {
    stop(
      "Each element of 'complication_definitions' must contain a 'definition' entry",
      call. = FALSE
    )
  }
}

#' Internal helper to validate that a complication index is a
#' named list of character vectors whose names are present in the supplied
#' complication definitions, with optional `"not_indexed"` allowed.
#' @keywords internal
validate_complication_index <- function(
  index,
  index_names
) {
  validate_named_list(index)

  if (!all(vapply(index, is.character, logical(1)))) {
    stop(
      "Each element of 'complication_index' must be a character vector",
      call. = FALSE
    )
  }

  invalid_names <- setdiff(
    names(index),
    c(index_names, "not_indexed")
  )

  if (length(invalid_names) > 0) {
    stop(
      "'index' contains names not present in 'index_names': ",
      paste(invalid_names, collapse = ", "),
      call. = FALSE
    )
  }
}

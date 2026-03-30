#' Utility function to help manage problem terms such they are normalized and spacing or common typos, capitalizations, etc., don't become an issue
#' @keywords internal
normalize_maude_terms <- function(terms) {
  terms |>
    tolower() |>
    gsub(pattern = "[^a-z0-9]+", replacement = " ", x = _) |>
    trimws()
}
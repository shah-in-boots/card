#' Validate that a list is named, with no element left unnamed
#'
#' @param x The object to check. Returns nothing; exists for its error.
#' @noRd
validate_named_list <- function(x) {
  if (!is.list(x) || is.null(names(x)) || any(names(x) == "")) {
    stop(
      "Expected a named list with non-empty names for each element",
      call. = FALSE
    )
  }
}

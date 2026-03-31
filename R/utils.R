#' Validate that a list is a named list where each element has is named
validate_named_list <- function(x) {
  if (!is.list(x) || is.null(names(x)) || any(names(x) == "")) {
    stop(
      "Expected a named list with non-empty names for each element",
      call. = FALSE
    )
  }
}

# Specific Aims {{{ ====

#' @title Create a modeling plan using `aims`
#'
#' @description `aims` is a modeling plan that allows for an epidemiological, supervised approach to creating modeling hypothesis.
#'
#' @param f Represents the basic formula that will be used to help identify how the analyses should be performed
#'
#' @param ... Not used currently, but available for extensibility
#' @return A `aims` object
aims <- function(f, ...) {
	UseMethod("aims")
}

#' @rdname aims
aims.default <- function(f, ...) {
  stop("`aims()` is not defined for a '", class(f)[1], "'.", call. = FALSE)
}

#' @rdname aims
aims.formula <- function(f, ...) {

}
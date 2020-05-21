# Generic Methods {{{ ====

#' @description Generic print method
#'
#'
#' @param x Model of class `cosinor`
#'
#' @param ... arguments to pass on
#'
#' @noRd
#' @export
print.cosinor <- function(x, ...) {

	cat("Call: \n")
	print(x$call)
	cat("\n")
	cat("\n")
	cat("Coefficients: \n")
	print(x$coefficients)

}


# }}}

# Statistical Methods {{{ ====

# }}}
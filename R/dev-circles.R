#' @title Tidy a(n) circular object
#' @description Tidy summarizes information about the components of a `circular`
#'   model.
#' @details `circular` object are made using the [circular::lm.circular()] function
#' @param x A `circular` regression object
#' @param conf.int Logical indicating whether or not to include confidence
#'   interval in tidied output
#' @param conf.level The confidence level to use if `conf.int = TRUE`. Must be
#'   between 0 and 1, with default to 0.95 (the 95% confidence interval).
#' @param ... For extensibility
#' @return a `tibble` object
#' @export
tidy.circular <- function(x, conf.int = FALSE, conf.level = 0.95, ...) {

  # Get base data
  names(x$coefficients) <- colnames(x$x)
	coefs <- x$coefficients
	se <- x$se.coef
	l <- list(coefs = coefs, se = se)
	mat <- do.call(cbind, lapply(l, function(x) {x[match(names(l[[1]]), names(x))]}))

	# Tibble it
	result <-
	  mat %>%
	  dplyr::as_tibble(rownames = "term") %>%
	  dplyr::rename(estimate = coefs, std.error = se) %>%
		tibble::add_column(
			statistic = x$t.values,
			p.value = x$p.values
		)

	# Add confidence intervals if needed
	if(conf.int) {
	  tdist <- stats::qt(1 - (1 - conf.level)/2, df = nrow(x$x) - 2)
	  result <-
	  	result %>%
	  	tibble::add_column(
	  		conf.low = coefs - tdist * se,
	  		conf.high = coefs + tdist * se
	  	)
	}

	# Return findings
	return(result)

}
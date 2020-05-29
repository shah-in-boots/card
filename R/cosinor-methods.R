# Generic Methods {{{ ====

#' @description Generic print method
#' @param x Model of class `cosinor`
#' @param ... arguments to pass on
#' @noRd
#' @export
print.cosinor <- function(x, ...) {

	cat("Call: \n")
	print(x$call)

	# Coefficients
	cat("\n")
	cat("Coefficients: \n")
	names(x$coefficients) <- x$coef_names
	print(x$coefficients)

}

#' @description Generic summary method
#' @param object Model of class `cosinor`
#' @param ... arguments to pass on
#' @noRd
#' @export
summary.cosinor <- function(object, ...) {

	# Summary
	cat(paste0(object$type, " Cosinor Model \n"))
	cat(strrep("-", 42))
	cat("\n")

	# Call
	cat("Call: \n")
	print(object$call)

	# Residuals
	cat("\n")
	cat("Residuals: \n")
	print(summary(object$residuals))

	# Coefficients
	cat("\n")
	cat("Coefficients: \n")
	names(object$coefficients) <- object$coef_names
	print(object$coefficients)

}

#' @description Generic plot method
#' @param x `cosinor` object
#' @param ... For extensibility
#' @noRd
#' @export
plot.cosinor <- function(x, ...) {

	# Model data
	model <- as.data.frame(x$model)
	model$yhat <- x$fitted.values
	model$res <- x$residuals

	# Plotting function
	plot(model$t, model$yhat)
}

# }}}

# Statistical Methods {{{ ====

#' @description Generic confidence interval method
#' @param object model of class `cosinor`
#' @param level the confidence level required
#' @param parm specification of which parameters (not currently used)
#' @param ... arguments to pass on
#' @noRd
#' @export
confint.cosinor <- function(object, parm, level = 0.95, ...) {

	### Taylor Series Approach

	# Confidence level
	alpha <- 1 - level

  # Matrix data
	y <- object$model[,"y"]
	t <- object$model[,"t"]
	x <- object$model[,"x"]
	z <- object$model[,"z"]
	xmat <- object$xmat
	yhat <- object$fitted.values
	mesor <- object$coefficients[object$coef_names == "mesor"]
	amp <- object$coefficients[object$coef_names == "amp"]
	phi <- object$coefficients[object$coef_names == "phi"]
	beta <- object$coefficients[object$coef_names == "beta"]
	gamma <- object$coefficients[object$coef_names == "gamma"]

	# Parameters / degrees of freedom
	n <- length(y)

	if(object$type == "Population") {

		### Population confidence intervals

		# Message
		message("Confidence intervals for amplitude and acrophase are not currently returned for population-mean cosinor.")

		# Parameters
		k <- nrow(xmat)

	  # Sigma / variances
	  sMesor <- sqrt(sum((xmat[, "mesor"] - mesor)^2) / (k-1))
	  sBeta <- sqrt(sum((xmat[, "beta"] - beta)^2) / (k-1))
	  sGamma <- sqrt(sum((xmat[, "gamma"] - beta)^2) / (k-1))

	  # Standard error
	  mesorSE <- tdist * sMesor / sqrt(k)

	  # Confidence intervals
	  tdist <- stats::qt(1 - alpha/2, df = k - 1)

	  confints <- matrix(
	  	rbind(
			  c(mesor - tdist * mesorSE, mesor + tdist * mesorSE),
			  c(NA, NA),
			  c(NA, NA)
	  	),
	  	ncol = 2, nrow = 3,
	  	dimnames = list(
	  		c("mesor", "amp", "phi"),
	  		c(paste0(100*(alpha/2),"%"), paste0(100*(1-alpha/2), "%"))
	  	)
	  )

	  # Returned
	  return(confints)

	} else {

	  ### Individual Cosinor Taylor Series

		# Nummber of parameters
		k <- 3

	  # Matrix to get standard errors and confidence intervals
	  s <- solve(xmat)

		# Residual sum of squared errors
	  RSS <- sum((y - yhat)^2)
	  sigma <- sqrt(RSS / (n - k))

	  # Standard error for MESOR
	  mesorSE <- unname(sigma * sqrt(s[1,1]))

	  # Standard error for amplitude
	  ampSE <-
	  	unname(sigma * sqrt(
	  		s[2,2]*cos(phi)^2 -
	  			2*s[2,3]*sin(phi)*cos(phi) +
	  			s[3,3]*sin(phi)^2
	  	))

	  # Standard error for phi
	  phiSE <-
	  	unname(1/amp * sigma * sqrt(
	  		s[2,2]*sin(phi)^2 -
	  			2*s[2,3]*sin(phi)*cos(phi) +
	  			s[3,3]*cos(phi)^2
	  	))

	  # Confidence intervals
	  tdist <- stats::qt(1 - alpha/2, df = n - k)

	  confints <- matrix(
	  	rbind(
			  c(mesor - tdist * mesorSE, mesor + tdist * mesorSE),
			  c(amp - tdist * ampSE, amp + tdist * ampSE),
			  c(phi - tdist * phiSE, phi + tdist * phiSE)
	  	),
	  	ncol = 2, nrow = 3,
	  	dimnames = list(
	  		c("mesor", "amp", "phi"),
	  		c(paste0(100*(alpha/2),"%"), paste0(100*(1-alpha/2), "%"))
	  	)
	  )

	  # Returned
	  return(confints)

	  ## }}}

	}


	### Ellipse Approach

	# Sum((x - xbar)^2(b - bhat)^2) + 2(sum((x - xbar)(z - zbar)(b - bhat)(g - ghat)) + sum((z - zbar)^2(z - zhat)^2) <= 2*sigma^2*fdist
	# xbar = sum(x)/n
	# zbar = sum(z)/n

	# RHS of ellipse equation (with fdist)
  fdist <- stats::qf(1 - alpha, df1 = 2, df2 = n - 3)
  RSS <- sum((y - yhat)^2)
  sigma <- sqrt(RSS / (n - 3))

  # LHS variables
  xbar <- sum(x)/n
  zbar <- sum(z)/n
  ghat <- seq(from = gamma - (amp * 2), to = gamma + (amp * 2), by = amp / 1000)
  gseq <- ghat
  bhat <- 0

  # First term
  sum((x - xbar)^2 * (beta - bhat)^2) +

  # Middle term
  2 * sum((x - xbar) * (z - zbar) * (beta - bhat) * (gamma - ghat)) +

  # Last term
  sum((z - zbar)^2 * (gamma - ghat)^2)

  # Confidence interval for MESOR
  sigma <- sqrt(RSS / (n - 3))
  tdist <- stats::qt(1 - alpha / 2, df = n - 3)
  mesorConf <- tdist * sigma * sqrt(1/n)
  mesorUpper <- mesor + mesorConf
  mesorLower <- mesor - mesorConf

}

# }}}
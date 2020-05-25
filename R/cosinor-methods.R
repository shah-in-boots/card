# Generic Methods {{{ ====

#' @description Generic print method
#' @param x Model of class `cosinor`
#' @param ... arguments to pass on
#' @noRd
#' @export
print.cosinor <- function(x, ...) {

	cat("Call: \n")
	print(x$call)
	cat("\n")
	cat("Coefficients: \n")
	print(x$coefficients)

}

#' @description Generic summary method
#' @param object Model of class `cosinor`
#' @param ... arguments to pass on
#' @noRd
#' @export
summary.cosinor <- function(object, ...) {

	# Generic method
	summary(object)

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
	object$coefficients



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

	# Confidence level
	alpha <- 1 - level

  # Matrix data
	y <- object$model[,"y"]
	t <- object$model[,"t"]
	x <- object$model[,"x"]
	z <- object$model[,"z"]
	yhat <- object$fitted.values
	n <- length(y)
	mesor <- object$coefficients["mesor"]
	beta <- object$coefficients["beta"]
	gamma <- object$coefficients["gamma"]
	amp <- object$coefficients["amp"]
	phi <- object$coefficients["phi"]

	# Residual sum of squared errors
  RSS <- sum((y - yhat)^2)

  # Confidence interval for MESOR
  sigma <- sqrt(RSS / (n - 3))
  tdist <- stats::qt(1 - alpha / 2, df = n - 3)
  mesorConf <- tdist * sigma * sqrt(1/n)
  mesorUpper <- mesor + mesorConf
  mesorLower <- mesor - mesorConf

  # Correction of sum of squares for each parameter
  xc <- 1 / n * sum((x - mean(x))^2)
  zc <- 1 / n * sum((z - mean(z))^2)
  tc <- 1 / n * sum((x - mean(x)) * (z - mean(z)))

  # Find beta and gamma CI region
  fdist <- stats::qf(1 - alpha / 2, df1 = 2, df2 = n - 3)

  # Quadratic/ellipse formula setup
  a <- xc
  b <- 2 * tc
  c <- zc
  d <- -2 * xc * beta - 2 * tc * gamma
  e <- -2 * tc * beta - 2 * zc * gamma
  f <-
  	xc * beta^2 +
  	2 * tc * beta * gamma +
  	zc * gamma^2 -
  	(2 / n) * sigma^2 * fdist
  gmax <- -(2 * a * e - d * b) / (4 * a * c - b^2)

  # Identify parameters for ellipses
  gseq <- seq(from = gmax - amp * 2, to = gmax + amp * 2, by = amp / 1000)
  bs1 <- (-(b * gseq + d) + sqrt(
    as.complex((b * gseq + d)^2 - 4 * a * (c * gseq^2 + e * gseq + f))
  )) / (2 * a)
  bs2 <- (-(b * gseq + d) - sqrt(
    as.complex((b * gseq + d)^2 - 4 * a * (c * gseq^2 + e * gseq + f))
  )) / (2 * a)

  # Isolate the elliptical region (non imaginary)
  index <- which(Re(bs1) != Re(bs2))
  gseq <- gseq[index]
  bs1 <- Re(bs1[index])
  bs2 <- Re(bs2[index])

  # Determine if ellipse regions overlap the pole (if overlap, cannot get CI)
  if (
    (diff(range(gseq)) >= max(gseq)) &
      ((diff(range(bs1)) >= max(bs1)) | (diff(range(bs2)) >= max(bs2)))
  ) {
    print("Confidence regions overlap the poles. Confidence intervals for amplitude and acrophase cannot be determined.")
  } else {
    # CI for Amplitude
    ampUpper <- max(c(sqrt(bs1^2 + gseq^2), sqrt(bs2^2 + gseq^2)))
    ampLower <- min(c(sqrt(bs1^2 + gseq^2), sqrt(bs2^2 + gseq^2)))

    # CI for Acrophase
    theta <- c(atan(abs(gseq / bs1)), atan(abs(gseq / bs2)))
    sa <- sign(c(bs1, bs2))
    sb <- sign(c(gseq, gseq)) * 3
    sc <- sa + sb
    tmp <- sc
    phiConf <- vector(mode = "double", length = length(theta))

    # Place theta in correct quadrant for phi
    for (i in 1:length(sc)) {
      if (sc[i] == 4 | sc[i] == 3) {
        phiConf[i] <- -theta[i]
        sc[i] <- 1
      } else if (sc[i] == 2 || sc[i] == -1) {
        phiConf[i] <- -pi + theta[i]
        sc[i] <- 2
      } else if (sc[i] == -4 || sc[i] == -3) {
        phiConf[i] <- -pi - theta[i]
        sc[i] <- 3
      } else if (sc[i] == -2 || sc[i] == -1) {
        phiConf[i] <- -2 * pi + theta[i]
        sc[i] <- 4
      }
    }

    # Get max and min values for phi / acrophase
    if (max(sc) - min(sc) == 3) {
      phiUpper <- min(phiConf[sc == 1])
      phiLower <- max(phiConf[sc == 4])
    } else {
      phiUpper <- max(phiConf)
      phiLower <- min(phiConf)
    }
  }

  # Output
  confints <- matrix(
  	c(mesorLower, ampLower, phiLower, mesorUpper, ampUpper, phiUpper),
  	nrow = 3, ncol = 2,
  	dimnames = list(c("mesor", "amp", "phi"),
  									c(paste0(100*(alpha/2),"%"), paste0(100*(1-alpha/2), "%")))
  )

  # Print output
  print(confints)

  # Returned
  list(
  	# Confidence intervals
  	confint = confints,

	  # Area of ellipse for circadian rhythmicity
  	area = cbind(gseq, bs1, bs2)

  )


}

# }}}
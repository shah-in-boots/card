# Zero Amplitude Test {{{ ====

#' @title Zero Amplitude Test
#'
#' @description Zero amplitude test assesses how well the circadian pattern fits the data, essentially detecting the present of a rhythm to the data.
#'
#' @param object model of class `cosinor`
#'
#' @param level confidence level
#'
#' @export
cosinor_zero_amplitude <- function(object, level = 0.95, ...) {

	### Confidence level of fstatistic

	# Model objects
	y <- object$model[, "y"]
	yhat <- object$fitted.values
	ybar <- mean(y)

	# Confidence level
	alpha <- 1 - level

	# Degrees of freedom
	n <- length(y)
	k <- 3 # Number of parameters
	fdist <- stats::qf(1 - alpha/2, df1 = k - 1, df2 = n - k)

	### Total sum of squares = model sum of squares + residual sum of squares
		# TSS = sum(y - ybar)^2
		# MSS = sum(yhat - ybar)^2
		# RSS = sum(y - yhat)^2
	TSS <- sum((y - ybar)^2)
	MSS <- sum((yhat - ybar)^2)
	RSS <- sum((y - yhat)^2)

	# Statistical significance by F test
	fstat <- (MSS / 2) / (RSS / (n - 3))

	# Output
	cat("Zero Amplitude Test: \n\n")
	cat(paste0("F-statistic: ", round(fstat, 3)))
	cat("\n")
	cat(paste0("F-level at ", level*100, "% confidence level: ", round(fdist, 3)))
	cat("\n")
	if(fstat > fdist) {
		cat("Rhythm detected by F-test.\n\n")
	} else {
		cat("Rhythm not detected by F-test.\n\n")
	}

	# Return
	list(
		fstat = fstat,
		fdist = fdist
	)

}

# }}}

# Goodness of Fit {{{ ====

#' @title Goodness of Fit of Cosinor
#'
#' @description Goodness of fit of a cosinor from data that has multiple
#'   collections at different timepoints or from multiple cycles. The RSS is
#'   partitioned into pure error (SSPE) and lack of fit (SSLOF). An F-test
#'   compares the SSPE and SSLOF to detect appropriateness of model.
#'
#'   \deqn{SSLOF = RSS - SSPE}
#'
#'   \deqn{SSPE = \sum_{i} \sum_{l} ( Y_{il} - \overline{Y}_{i} )^2}
#'
#'   The fitted values for each time point are:
#'   \deqn{\overline{Y}_{i} = \frac{ \sum_{l} Y_{il} }{ n_{i}}}
#'
#' @param object requires cosinor model generated with [card::cosinor] to
#'   calculate statistics.
#'
#' @param level confidence level desired
#'
#' @param ... additional parameters may be needed for extensibility
#'
#' @return f-statistic as result of goodness of fit
#'
#' @export
cosinor_goodness_of_fit <- function(object, level = 0.95, ...) {

  # Model data
	y <- object$model[,"y"]
	t <- object$model[,"t"]
	x <- object$model[,"x"]
	z <- object$model[,"z"]
	yhat <- object$fitted.values
	n <- length(y)
	names(object$coefficients) <- object$coef_names
	mesor <- object$coefficients["mesor"]
	amp <- object$coefficients["amp"]
	phi <- object$coefficients["phi"]

  # Goodness of fit
  # lack of fit = sumsq(res) - sumsq(observed - local avg)
  # SSLOF = RSS - SSPE
    # SSLOF = sum of squares lack of fit
    # RSS = residual sum of squares
    # SSPE = pure error sum of squares

  # SSPE = sumi(suml( (yil - yibar)^2 ))
    # yibar = suml(yil)/ni
    # ni = number of data collected at time t

  yil <- stats::aggregate(y, by = list(t), sum)
  names(yil) <- c("t", "yil")
  ni <- stats::aggregate(y, by = list(t), length)
  names(ni) <- c("t", "ni")
  ybar <- merge(yil, ni, by = "t")
  ybar$ybar <- ybar$yil / ybar$ni # Fitted average at each hour

  # SSPE = sum(observed value at t - local average at t)^2
  df <- data.frame(y, t)
  SSPE <- vector()
  for(l in seq_along(ybar$t)) {
    yl <- df$y[df$t == ybar$t[l]]
    ybarl <- ybar$ybar[ybar$t == ybar$t[l]]
    SSPE[l] <- sum((yl - ybarl)^2)
  }
  SSPE <- sum(SSPE)

  # Lack of fit
  SSLOF <- RSS - SSPE

  # Appropriateness of model...
  # F = (SSLOF/(m-1-2p)) / (SSPE/(N-m))
    # m = number of time points
    # p = number of cosine components
  m <- length(unique(t))
  p <- 1 # Single cosinor... may need to adjust to count terms [TODO]

  fstat <- (SSLOF/(m - 1 - 2*p)) / (SSPE/(n - m))
  fdist <- stats::qf(1 - alpha/2, df1 = m - 1 - 2*p, n - m)

  # Return
  list(
    fstat = fstat,
    fdist = fdist
  )

}

# }}}

# Area of Ellipse {{{ ====

#' @title Area of Ellipse
#'
#' @description Formulas for creating the area of the ellipse to identify confidence intervals, directionality, and graphing purposes.
#'
#' @param object Model of class `cosinor`
#'
#' @param level Confidence level requested
#'
#' @param ... Not currently used, but required for extensibility.
#'
#' @export
cosinor_area <- function(object, level = 0.95, ...) {

	# Model objects needed
	y <- object$model[, "y"]
	x <- object$model[, "x"]
	z <- object$model[, "z"]
	t <- object$model[, "t"]
	beta <- object$coefficients[object$coef_names == "beta"]
	gamma <- object$coefficients[object$coef_names == "gamma"]
	yhat <- object$fitted.values
	alpha <- 1 - level

  # Stats
  RSS <- sum((y - yhat)^2)
  sigma <- sqrt(RSS / (n - 3))
  k <- 3 # number of parameters
	n <- length(y)

  # Correction of sum of squares for each parameter
  xc <- 1 / n * sum((x - mean(x))^2)
  zc <- 1 / n * sum((z - mean(z))^2)
  tc <- 1 / n * sum((x - mean(x)) * (z - mean(z)))

  # Find beta and gamma CI region
  fdist <- stats::qf(1 - (alpha / 2), df1 = k - 1, df2 = n - k)

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

  # Return
  list(
  	area = cbind(gseq, bs1, bs2)
  )

}
# }}}

# Graphical Assessment of Amplitude and Acrophse {{{ ====

#' @title Graphical Assessment of Amplitude and Acrophase
#'
#' @description This is a ggplot-styled graphical representation of the ellipse
#'   region generated by the cosinor analysis. It requires the same data used by
#'   cosinor model to be fit with the model [card::cosinor]. This includes
#'   the amplitude, acrophase,
#'
#' @param object Requires a cosinor model to extract the correct statistics to
#'   generate the plot.
#'
#' @param level Confidence level for ellipse
#'
#' @param ... Additional parameters may be needed for extensibility
#'
#' @return ggplot object that has has multiple components
#'
#' @examples
#' data("twins")
#' m <- cosinor(rDYX ~ hour, twins)
#' ggcosinorfit(m)
#' @export
ggcosinorfit <- function(object, level = 0.95, ...) {

	# Area
	area <- cosinor_area(object, level = level)$area
  gseq <- area[, "gseq"]
  bs1 <- area[, "bs1"]
  bs2 <- area[, "bs2"]

  # Model parameters
	mesor <- object$coefficients[object$coef_names == "mesor"]
	amp <- object$coefficients[object$coef_names == "amp"]
	phi <- object$coefficients[object$coef_names == "phi"]
	beta <- object$coefficients[object$coef_names == "beta"]
	gamma <- object$coefficients[object$coef_names == "gamma"]

  # Necessary values for the plot
  theta_clock <- seq(0, 2 * pi, length.out = 24^2)
  clock <- cbind(2 * amp * cos(theta_clock), 2 * amp * sin(theta_clock))
  rad <- seq(0, 2 * pi - pi / 4, by = pi / 4)
  rad_clock <- cbind(2.2 * amp * cos(rad), 2.2 * amp * sin(rad))

  # GGplot the values
  ggplot() +
    # Ellipse
    geom_line(aes(x = gseq, y = bs1), col = "goldenrod", size = 1) +
    geom_line(aes(x = gseq, y = bs2), col = "goldenrod", size = 1) +
    # Line from origin to ellipse
    geom_line(aes(
      x = c(0, gamma),
      y = c(0, beta)
    ),
    lty = 1,
    size = 1,
    col = "black"
    ) +
    # Line from ellipse to circumference
    geom_line(aes(
      x = c(gamma, -2 * amp * sin(phi)),
      y = c(beta, 2 * amp * cos(phi)),
      group = 0
    ),
    size = 1,
    col = "black",
    lty = 3
    ) +
    # Axes
    geom_line(aes(x = c(0, 0), y = c(-2 * amp, 2 * amp)), lty = 5, col = "grey") +
    geom_line(aes(y = c(0, 0), x = c(-2 * amp, 2 * amp)), lty = 5, col = "grey") +
    # "Clock" shape to help show degrees on unit circle
    geom_path(aes(x = clock[, 1], y = clock[, 2]), col = "cornflowerblue") +
    annotate(
      geom = "text",
      x = rad_clock[, 1],
      y = rad_clock[, 2],
      label = paste(rad * 180 / pi, "\u00B0")
    ) +
    # Labels
    xlab(expression(paste(gamma))) +
    ylab(expression(paste(beta))) +
    xlim(-2.5 * amp, 2.5 * amp) +
    ylim(-2.5 * amp, 2.5 * amp) +
    theme_minimal()
}

# }}}

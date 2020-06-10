# Diagnostic Testing {{{ ====

#' @title Zero Amplitude Test
#' @description Zero amplitude test assesses how well the circadian pattern fits
#'   the data, essentially detecting the present of a rhythm to the data.
#' @param object model of class `cosinor`
#' @param level confidence level
#' @return Returns a list of test statistics, as well prints out a report of
#'   analysis.
#' @export
cosinor_zero_amplitude <- function(object, level = 0.95) {

	if(object$type == "Population") {
		message("Zero amplitude test may not be accurate for population-mean cosinor method.")
	}

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
	fdist <- stats::qf(1 - alpha, df1 = k - 1, df2 = n - k)

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

#' @title Goodness of Fit of Cosinor
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
#' @param level confidence level desired
#' @param ... additional parameters may be needed for extensibility
#' @return f-statistic as result of goodness of fit
#' @export
cosinor_goodness_of_fit <- function(object, level = 0.95, ...) {

	if(object$type == "Population") {
		message("Goodness of fit may not be accurate for population-mean cosinor method.")
	}

	# Confidence level
	alpha <- 1 - level

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

	# RSS
	RSS <- sum((y - yhat)^2)

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
  fdist <- stats::qf(1 - alpha, df1 = m - 1 - 2*p, n - m)

  # Return
  list(
    fstat = fstat,
    fdist = fdist
  )

}

# }}}

# Graphical Assessment {{{ ====

#' @title Area of Ellipse
#' @description Formulas for creating the area of the ellipse to identify
#'   confidence intervals, direction, and graphing purposes.
#' @param object Model of class `cosinor`
#' @param level Confidence level requested
#' @param ... Not currently used, but required for extensibility.
#' @return Area of potential cosinor for graphical analysis as matrix stored in
#'   a list.
#' @export
cosinor_area <- function(object, level = 0.95, ...) {

	if(object$type == "Population") {
		message("Area calculations may not be accurate for population-mean cosinor method.")
	}

	# Model objects needed
	y <- object$model[, "y"]
	x <- object$model[, "x"]
	z <- object$model[, "z"]
	t <- object$model[, "t"]
	yhat <- object$fitted.values
	mesor <- object$coefficients[object$coef_names == "mesor"]
	amp <- object$coefficients[object$coef_names == "amp"]
	phi <- object$coefficients[object$coef_names == "phi"]
	beta <- object$coefficients[object$coef_names == "beta"]
	gamma <- object$coefficients[object$coef_names == "gamma"]

  # Stats
	alpha <- 1 - level
	n <- length(y)
  k <- 3 # number of parameters
  RSS <- sum((y - yhat)^2)
  sigma <- sqrt(RSS / (n - k))
  fdist <- stats::qf(1 - alpha, df1 = k - 1, df2 = n - k)

  # Correction of sum of squares for each parameter
  xc <- 1 / n * sum((x - mean(x))^2)
  zc <- 1 / n * sum((z - mean(z))^2)
  tc <- 1 / n * sum((x - mean(x)) * (z - mean(z)))

  # Find beta and gamma CI region

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

#' @title Graphical Assessment of Amplitude and Acrophase
#' @description This is a ggplot-styled graphical representation of the ellipse
#'   region generated by the cosinor analysis. It requires the same data used by
#'   cosinor model to be fit with the model [card::cosinor]. This includes
#'   the amplitude, acrophase,
#' @param object Requires a cosinor model to extract the correct statistics to
#'   generate the plot.
#' @param level Confidence level for ellipse
#' @param ... Additional parameters may be needed for extensibility
#' @examples
#' data("twins")
#' m <- cosinor(rDYX ~ hour, twins, tau = 24)
#' ggellipse(m)
#' @return Object of class `ggplot` to help identify confidence intervals
#' @import ggplot2
#' @export
ggellipse <- function(object, level = 0.95, ...) {

	if(object$type == "Population") {
		message("Ellipse may not be accurate for population-mean cosinor method.")
	}

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
  	geom_line(aes(x = c(0, gamma),
  								y = c(0, beta)),
  						lty = 1,
  						size = 1,
  						col = "black") +
  	# Line from ellipse to circumference
  	geom_line(aes(
  		x = c(gamma, -2 * amp * sin(phi)),
  		y = c(beta, 2 * amp * cos(phi)),
  		group = 0
  	),
  	size = 1,
  	col = "black",
  	lty = 3) +
  	# Axes
  	geom_line(aes(x = c(0, 0), y = c(-2 * amp, 2 * amp)),
  						lty = 5, col = "grey") +
  	geom_line(aes(y = c(0, 0), x = c(-2 * amp, 2 * amp)),
  						lty = 5, col = "grey") +
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

#' @title ggplot of cosinor model
#' @description ggplot of single cosinor model, options include to ability to add diagnostic/fit parameters such as residuals.
#' @param object Model of class `cosinor`.
#' @param residuals If residuals should be added. Colors can be overwritten with additional arguments (e.g. "+ scale_color_grey()")
#' @param ... For extensibility
#' @return Object of class `ggplot` that can be layered
#' @import ggplot2
#' @export
ggcosinor <- function(object, residuals = TRUE, ...) {

	# Decide what type of plot
	type <- object$type

	# Model
	model <- tidyr::tibble(as.data.frame((object$model)))
	model$yhat <- object$fitted.values
	model$res <- object$residuals
	period <- object$tau

	# Coefficients
	coefs <- object$coefficients
	mesor <- coefs[object$coef_names == "mesor"]
	amp <- coefs[object$coef_names == "amp"]
	phi <- coefs[object$coef_names == "phi"]

	# Summary per hour
	aug <-
		model %>%
		dplyr::group_by(t) %>%
		dplyr::summarise_all(mean, na.rm = TRUE)

	### Conditional graphing positions

	# Acrophase in hours
	acro <- abs((phi * period) / (2 * pi)) # location of peak

	# Period label
	perly <- mesor - 13/12*amp
	perlx <- period/2

	# Obtain approximate points of intersection of MESOR and curve
	f <- stats::approxfun(x = aug$t, y = aug$yhat, rule = 2)
	xs <- seq(min(aug$t), max(aug$t), length.out = 100)
	ys <- f(xs)
	pos <- xs[which(abs(ys - mesor) < 0.01)]

	if(length(pos) == 1) {
		peak <- dplyr::case_when(
			pos < acro ~ "early",
			pos > acro ~ "late"
		)
	} else if(length(pos) >= 2) {
		peak <- dplyr::case_when(
			pos[1] < acro & pos[length(pos)] > acro ~ "mid"
		)
	}

	# Identify where to position acrophase/amplitude and its label
	switch(
		peak,
		early = {
			# Amplitude
			ampx <- pos
			amplx <- (pos + period/2)/2
			amply <- (2*mesor + amp)/2
			# Acrophase
			acrox <- min(aug$t)
			acroy <- aug$yhat[aug$t == min(aug$t)]
			acrox <- min(aug$t)
			acroy <- max(aug$yhat)
			acrolx <- (acrox + acro) / 2
			acroly <- mesor + 11/12 * amp
			# Mesor
			mesx <- (acro + pos)/2
			mesy <- mesor - 1/12*amp
		},
		mid = {
			# Amplitude
			ampx <- acro
			amplx <- period/2
			amply <- (2*mesor + amp)/2
			# Acrophase
			acrox <- min(aug$t)
			acroy <- max(aug$yhat)
			acrolx <- (acrox + acro) / 2
			acroly <- mesor + 11/12 * amp
			# Mesor
			mesx <- dplyr::case_when(
				acro < period/2 ~ (acro + pos[1])/2,
				acro > period/2 ~ (acro + pos[length(pos)])/2
			)
			mesy <- mesor - 1/12*amp
		},
		late = {
			# Amplitude
			ampx <- pos
			amplx <- (pos + period/2)/2
			amply <- (2*mesor + amp)/2
			# Acrophase
			acrox <- min(aug$t)
			acroy <- aug$yhat[aug$t == min(aug$t)]
			acrox <- min(aug$t)
			acroy <- max(aug$yhat)
			acrolx <- (acrox + acro) / 2
			acroly <- mesor + 11/12 * amp
			# Mesor
			mesx <- (acro + pos)/2
			mesy <- mesor - 1/12*amp
		}
	)

	# Parts of ggplot
	g <- ggplot(aug) +
		# Fit
		stat_smooth(
			aes(x = t, y = yhat), method = "gam",
			color = "black", size = 1.1
		) +
		geom_hline(yintercept = mesor, color = "gray") +
		geom_vline(xintercept = ampx, color = "gray") +
		theme_minimal() +
		theme(
			legend.position = "none",
			panel.grid.major = element_blank(),
			panel.grid.minor = element_blank()
		) +
		xlim(min(aug$t), period)

	glabs <- list(
		# Mesor
		geom_label(
			aes(x = mesx, y = mesy, label = paste0("MESOR = ", round(mesor, 2))),
			fill = "white"
		),
		# Amplitude
		geom_segment(
			aes(x = ampx, xend = ampx, y = mesor, yend = mesor + amp),
			linetype = "dashed", lineend = "butt", linejoin = "mitre",
		),
		geom_label(
			aes(x = amplx, y = amply, label = paste0("A = ", round(amp, 2))),
			fill = "white"
		),
		# Acrophase
		geom_segment(
			aes(x = acro, xend = acrox, y = acroy, yend = acroy),
			linetype = "dashed", lineend = "butt", linejoin = "mitre",
		),
		geom_label(
			aes(x = acrolx, y = acroly, label = paste0("Phi = ", round(acro, 2))),
			fill = "white",
		),
		# Period
		geom_segment(
			aes(x = min(t), xend = period, y = min(yhat), yend = min(yhat)),
			linetype = "dashed", lineend = "butt", linejoin = "mitre",
		),
		geom_label(
			aes(x = perlx, y = perly, label = paste0("Period = ", period)),
			fill = "white"
		)
	)

	if(residuals) {
		gg <-
			g +
			geom_segment(aes(x = t, xend = t, y = y, yend = yhat), alpha = 0.3) +
			geom_point(aes(x = t, y = y, colour = abs(res), size = abs(res))) +
			scale_color_viridis_c(option = "magma") +
			glabs
	} else {
		gg <-
			g +
			glabs
	}

	# Return
	return(gg)

}

#' @title ggplot of multiple cosinor models
#' @description ggplot of multiple cosinor objects for comparison purposes. Appearance of plot will be greatly enhanced if objects share a similar y-axis scale.
#' @param objects List of `cosinor` objects
#' @param ... For extensibility
#' @return Object of class `ggplot` that can be layered
#' @import ggplot2
#' @export
ggmulticosinor <- function(objects, ...) {

	# Are there multiple objects? Returns TRUE if vector (thus list of models)
	if(!is.vector(objects)) {
		stop("Requires list of cosinor objects.", call. = FALSE)
	}

	# We need multiple dataframes to plot this effectively
	augdf <- list()
	coefs <- list()
	for(i in seq_along(objects)) {
		print(i)
		object <- objects[[i]]

		model <- tidyr::tibble(as.data.frame((object$model)))
		model$yhat <- object$fitted.values
		model$res <- object$residuals

		# Coefficients
		coef <- object$coefficients
		mesor <- coef[object$coef_names == "mesor"]
		amp <- coef[object$coef_names == "amp"]
		phi <- coef[object$coef_names == "phi"]

		# Phi in hours
		acro <- abs((phi * 24) / (2 * pi))

		# Save output
		coefs[[i]] <- list(
			mesor = mesor,
			amp = amp,
			phi = phi,
			acro = acro
		)

		# Summary per hour
		aug <-
			model %>%
			dplyr::group_by(t) %>%
			dplyr::summarise_all(mean, na.rm = TRUE)

		augdf[[i]] <- aug
	}

	# Combine into 1 table with grouping variable as .id
	aug <- dplyr::tibble(data.table::rbindlist(augdf, idcol = TRUE))
	aug$.id <- factor(aug$.id)
	vars <- dplyr::tibble(data.table::rbindlist(coefs, idcol = TRUE))
	vars$.id <- factor(vars$.id)

	# Overall plot
	g <- ggplot(aug) +
		stat_smooth(
			aes(x = t, y = yhat, group = .id),
			method = "gam", size = 1.1, colour = "black"
		) +
		theme_minimal() +
		theme(
			legend.position = "none",
			panel.grid.major = element_blank(),
			panel.grid.minor = element_blank()
		) +
		coord_cartesian(xlim = c(min(aug$t), max(aug$t)))

	# Annotations
	glabs <- list(
		geom_hline(aes(yintercept = mesor, colour = .id), vars),
		geom_vline(aes(xintercept = acro, colour = .id), vars),
		geom_label(
			aes(
				x = 1/3*acro, y = mesor + 1/3*amp,
				label = paste0("Group = ", .id,
											 "\nMesor = ", round(mesor, 2),
											 "\nAmp = ", round(amp, 2),
											 "\nPhi = ", round(phi, 2))
				),
			data = vars
		)
	)

	# Residuals
	gres <- list(
		geom_point(
			aes(x = t, y = y, group = .id, colour = .id)
		),
		geom_segment(
			aes(x = t, xend = t, y = y, yend = yhat, group = .id, colour = .id),
			alpha = 0.3
		),
		scale_color_viridis_d(option = "plasma", end = 0.8)
	)

	# Return
	gg <- g + gres + glabs
	return(gg)

}

#' @title ggplot of population cosinor model
#' @description ggplot of a population cosinor model
#' @param object A `cosinor` object of the population type, called when the population is specified in [card::cosinor()]
#' @param ... For extensibility
#' @return Returns object of class `ggplot` that can be layered
#' @import ggplot2
#' @export
ggpopcosinor <- function(object, ...) {

	# Confirm if type is population
	if(object$type != "Population") {
		stop("This object is not a `cosinor` model of type `Population`.", call. = FALSE)
	}

	model <- tidyr::tibble(as.data.frame((object$model)))
	model$yhat <- object$fitted.values
	model$res <- object$residuals

  # Remove patients with only 1 observation (will cause a det = 0 error)
  counts <- by(model, model[, "population"], nrow)
  lowCounts <- as.numeric(names(counts[counts <= 5]))
  model <- subset(model, !(population %in% lowCounts))

	# Subset for testing
	#model <- subset(model, pop %in% c(1:100))

	# Coefficients
	coefs <- object$coefficients
	mesor <- coefs[object$coef_names == "mesor"]
	amp <- coefs[object$coef_names == "amp"]
	phi <- coefs[object$coef_names == "phi"]

	# Phi in hours
	acro <- abs((phi * 24) / (2 * pi))

	# Predicted values
	model$pred <- mesor + amp*cos((2*pi*model$t / object$tau) + phi)

	# Summary per hour
	aug <-
		model %>%
		dplyr::group_by(t) %>%
		dplyr::summarise_all(mean, na.rm = TRUE)

	# General ggplot
	g <- ggplot(model) +
		geom_line(
			aes(x = t, y = yhat, group = population, colour = "Individual"),
			size = 0.5, alpha = 0.5
		) +
		# Average
		stat_smooth(
			aes(x = t, y = yhat, colour = "Mean"), data = aug, size = 1.5
		) +
		# Predicted
		stat_smooth(
			aes(x = t, y = pred, colour = "Predicted"), size = 1.5
		) +
		scale_colour_manual(
			name = "",
			values = c(
				"Individual" = "darkslategrey",
				"Mean" = "cornflowerblue",
				"Predicted" = "indianred"
				)
		) +
		theme_minimal() +
		theme(
			legend.position = "bottom",
			legend.background = element_rect(color = NA),
			panel.grid.major = element_blank(),
			panel.grid.minor = element_blank()
		)

	# Mean line

	# Predicted

	# Return
	gg <- g
	return(g)

}

# }}}

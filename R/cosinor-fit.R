# Cosinor Implementation ----

## Single Component Cosinor Implementation

#' @description Model fitting algorithm for cosinor. Results in output that define the new S3 class, as seen by the [hardhat::new_model], which generates the `new_cosinor()` function.
#' @noRd
cosinor_impl <- function(predictors, outcomes, tau) {

  ### Parameters for normal equations

  # Formal equation
  # y(t) = M + A*cos(2*pi*t/tau + phi)
    # A = Amplitude
    # phi = acrophase (measure of hte time of overall high values in cycle)
    # M = MESOR
  # y(t) = M + beta*x + gamma*z + error(t)
    # beta = A*cos(phi)
    # gamma = -A*sin(phi)
    # x = cos(2*pi*t/tau)
    # z = sin(2*pi*t/tau)

  # Where N is number of observations iterated through by i

  # RSS = sum[y - (M + beta*x + gamma*z)]^2

  # Normal equations (where M, beta, gamma are the coefficients to solve for)
  # sum(y) = M*n + beta*sum(x) + gamma*sum(z)
  # sum(y*x) = M*sum(x) + beta*sum(x^2) + gamma*sum(x*z)
  # sum(y*z) = M*sum(z) + beta*sum(x*z) + gamma*sum(z^2)

  # Multiple components... is an extension of single component
  # y(t) = M + sum_j[ A_j*cos(2*pi*t/tau_j + phi_j)
  # y(t) = M + sum_j[beta_j * x_j + gamma_j * z_j] + error(t)

  # Number of parameters will be the number of taus
    # e.g. single component = 3 components, where 3 = 2p + 1 (p = 1 component)
  p <- length(tau)

  # Single parameters
  y <- outcomes
  t <- predictors
  n  <- length(t)

  # Normal equation for 3 components
    # Normal equations (where M, beta, gamma are the coefficients to solve for)
    # sum(y) = M*n + beta*sum(x) + gamma*sum(z)
    # sum(y*x) = M*sum(x) + beta*sum(x^2) + gamma*sum(x*z)
    # sum(y*z) = M*sum(z) + beta*sum(x*z) + gamma*sum(z^2)
    # d = Su (for single component, 3 equations with 3 unknowns)

  # For multiple components, the matrix must be expanded

  # Need to create number of x values to match number of taus
  # x1, x2, z1, z2 in this case
  for (i in 1:p) {
    assign(paste0("x", i), cos((2 * pi * t) / tau[i]))
    assign(paste0("z", i), sin((2 * pi * t) / tau[i]))
  }

  # Creating a new dataframe with all variables
  model <- data.frame(y, t, mget(paste0("x", 1:p)), mget(paste0("z", 1:p)))

  # The formula, where the intercept will be the MESOR (not included)
  f <- stats::formula(
    paste0("y ~ ", paste0("x", 1:p, " + ", "z", 1:p, collapse = " + "))
  )

  # Can create a model frame here using two approaches
  # Base R and with hardhat
  m <- stats::model.frame(f, model)
  xmat <- stats::model.matrix(f, m)
  ymat <- as.matrix(y)

  ### Solving for coefficients

  # Solve for coefficients, including amplitude and acrophase
  coefs <- solve(t(xmat) %*% xmat) %*% t(xmat) %*% ymat
  mesor <- coefs[1]

  for (i in 1:p) {

    # Beta and gamma terms
    assign(paste0("beta", i), unname(coefs[paste0("x", i),]))
    assign(paste0("gamma", i), unname(coefs[paste0("z", i),]))

    # Amplitude
    assign(paste0("amp", i), sqrt(get(paste0("beta", i))^2 + get(paste0("gamma", i))^2))

    # Phi / acrophase
    sb <- sign(get(paste0("beta", i)))
    sg <- sign(get(paste0("gamma", i)))
    theta <- atan(abs(get(paste0("gamma", i)) / get(paste0("beta", i))))

    if ((sb == 1 | sb == 0) & sg == 1) {
      phi <- -theta
    } else if (sb == -1 & (sg == 1 | sg == 0)) {
      phi <- theta - pi
    } else if ((sb == -1 | sb == 0) & sg == -1) {
      phi <- -theta - pi
    } else if (sb == 1 & (sg == -1 | sg == 0)) {
      phi <- theta - (2 * pi)
    }

    assign(paste0("phi", i), phi)
  }

  coefs <- unlist(c(mesor = mesor, mget(paste0("amp", 1:p)), mget(paste0("phi", 1:p)), mget(paste0("beta", 1:p)), mget(paste0("gamma", 1:p))))

  # Predicted / output
	  # y(t) = M + b1 * x1 + g1 * z1 + b2 * x2 + g2 * z2
	  # y(t) = M + amp1 * cos(2*pi*t/tau1 + phi1) + amp2 * cos(2*pi*t/tau2 + phi2)

  pars <- list()
  for (i in 1:p) {
		pars[[i]] <- get(paste0("amp", i)) * cos(2*pi*t / tau[i] + get(paste0("phi", i)))
  }
  df <- data.frame(mesor = mesor, matrix(unlist(pars), ncol = length(pars), byrow = FALSE))
  yhat <- rowSums(df)

  ### Model Output

  # Model coefficients
  coef_names <- names(coefs)
  coefs <- unname(coefs)

  # Fit and residuals
  fitted.values <- yhat
  residuals <- y - yhat

  # List to return
  list(

    # Raw coefficients
    coefficients = coefs,
    coef_names = coef_names,

    # Fitted and residual values
    fitted.values = fitted.values,
    residuals = residuals,

    # Overall model of cosinor
    model = model,

    # Matrices used
    xmat = xmat

  )

}

## Population Mean Cosinor Implementation

#' @description Model fitting algorithm for population-mean cosinor. Uses the
#'   `cosinor_impl()` algorithm to derive individual parameters.
#' @noRd
cosinor_pop_impl <- function(predictors, outcomes, tau, population) {

  ### Population cosinor parameter setup

  # Period
  p <- length(tau) # Number of parameters ... single cosinor ~ 2p + 1 = 3

  # Create data frame for split/apply approach
  df <- na.omit(data.frame(predictors, outcomes, population))

  # Remove patients with only p observations (will cause a det ~ 0 error)
  counts <- by(df, df[, "population"], nrow)
  lowCounts <- as.numeric(names(counts[counts <= 2*p + 1]))
  df <- subset(df, !(population %in% lowCounts))

  # Message about population count removal
  if (length(lowCounts) != 0) {
    message(length(lowCounts), " subjects were removed due to having insufficient observations.")
  }

  # Population parameters
  k <- length(unique(df$population)) # Number of individuals
  y <- df$outcomes
  t <- df$predictors
  n <- length(t)
  population <- df$population

  # Need to create number of x values to match number of taus
  # x1, x2, z1, z2 in this case
  for (i in 1:p) {
    assign(paste0("x", i), cos((2 * pi * t) / tau[i]))
    assign(paste0("z", i), sin((2 * pi * t) / tau[i]))
  }

  # Creating a new dataframe with all variables
  model <- data.frame(y, t, mget(paste0("x", 1:p)), mget(paste0("z", 1:p)), population)

  # Create matrix that we can apply cosinor to subgroups
  kCosinors <- with(
    df,
    by(df, population, function(x) {
      cosinor_impl(x$predictors, x$outcomes, tau)
    })
  )

  ### Coefficients

  # Fits of individual cosinors
  kfits <- sapply(kCosinors, stats::fitted)
  fits <- data.frame(
    population = rep(names(kfits), sapply(kfits, length)),
    yhat = unlist(kfits)
  )

  # Matrix of coefficients
  tbl <- sapply(kCosinors, stats::coef, USE.NAMES = TRUE)
  coef_names <- c("mesor", paste0("amp", 1:p), paste0("phi", 1:p), paste0("beta", 1:p), paste0("gamma", 1:p))
  rownames(tbl) <- coef_names
  xmat <- t(tbl)

  # Get mean for each parameter (mesor, beta, gamma), ignoring averaged amp/phi
  coefs <- colMeans(xmat, na.rm = TRUE)

  for (i in 1:p) {

    # Beta and gamma terms
    assign(paste0("beta", i), unname(coefs[paste0("beta", i)]))
    assign(paste0("gamma", i), unname(coefs[paste0("gamma", i)]))

    # Amplitude
    assign(paste0("amp", i), sqrt(get(paste0("beta", i))^2 + get(paste0("gamma", i))^2))

    # Phi / acrophase
    sb <- sign(get(paste0("beta", i)))
    sg <- sign(get(paste0("gamma", i)))
    theta <- atan(abs(get(paste0("gamma", i)) / get(paste0("beta", i))))

    if ((sb == 1 | sb == 0) & sg == 1) {
      phi <- -theta
    } else if (sb == -1 & (sg == 1 | sg == 0)) {
      phi <- theta - pi
    } else if ((sb == -1 | sb == 0) & sg == -1) {
      phi <- -theta - pi
    } else if (sb == 1 & (sg == -1 | sg == 0)) {
      phi <- theta - (2 * pi)
    }

    assign(paste0("phi", i), phi)

    # Final coefs
    coefs[paste0("amp", i)] <- get(paste0("amp", i))
    coefs[paste0("phi", i)] <- get(paste0("phi", i))

  }

  ### Model output

  # Fitted values
  # y(t) = M + A*cos(2*pi*t/tau + phi)

  # Individual fits

  # Overall model
  yhat <- fits$yhat

  # List of values to return (must be same as cosinor_impl)
  list(

    # Raw coefficients
    coefficients = unname(coefs),
    coef_names = coef_names,

    # Fitted and residual values
    fitted.values = yhat,
    residuals = y - yhat,

    # Overall population cosinor data set, including subject names
    model = model,

    # Matrices used (for population cosinor, is the coefficient matrix)
    xmat = xmat

  )

}

# Statistical Methods ----

## Confidence Intervals

#' @description Generic confidence interval method
#' @param object model of class `cosinor`
#' @param level the confidence level required
#' @param parm specification of which parameters (not currently used)
#' @param ... arguments to pass on
#' @noRd
#' @export
confint.cosinor <- function(object, parm, level = 0.95, ...) {

	# Confidence level
	a <- 1 - level

	# Parameters
	y <- object$model[,"y"]
	t <- object$model[,"t"]
	n <- length(t)
	p <- length(object$tau)

  for (i in 1:p) {
    assign(paste0("x", i), object$model[, paste0("x", i)])
    assign(paste0("z", i), object$model[, paste0("z", i)])
  }

	xmat <- object$xmat
	yhat <- object$fitted.values
	coefs <- object$coefficients
	names(coefs) <- object$coef_names
  for (i in 1:length(coefs)) {
    assign(names(coefs)[i], unname(coefs[i]))
	}

	switch(
	  object$type,
	  Population = {
  		# Message
  		message("Confidence intervals for amplitude and acrophase for population-mean cosinor use the methods described by Fernando et al 2004, which may not be applicable to multiple-components.")

  		# Freedom by number of individuals
  		k <- nrow(object$xmat)

  		# Standard errors
  	  SE_mesor <- sd(xmat[, "mesor"]) / sqrt(k)
  		for(i in 1:p) {
  		  assign(paste0("SE_amp", i), (sd(xmat[, paste0("amp", i)]) / sqrt(k)))
  		  assign(paste0("SE_phi", i), (sd(xmat[, paste0("phi", i)]) / sqrt(k)))
  		  assign(paste0("SE_beta", i), (sd(xmat[, paste0("beta", i)]) / sqrt(k)))
  		  assign(paste0("SE_gamma", i), (sd(xmat[, paste0("gamma", i)]) / sqrt(k)))
  		}

  	  # Save SE
  	  se <- list()
      for(i in 1:p) {
        se[[i]] <- c(
          paste0("SE_amp", i),
          paste0("SE_phi", i),
          paste0("SE_beta", i),
          paste0("SE_gamma", i)
        )
      }
  	  se <- c(SE_mesor, unlist(mget(unlist(se))))
  	  names(se)[1] <- "SE_mesor"
  	  names(se) <- gsub("SE_", "", names(se))

  	  # Confidence intervals
  	  tdist <- stats::qt(1 - a/2, df = n - k)
  	  confints <- list()
  	  for(i in 1:p) {
  	    confints[[i]] <-
  	      c(
  	        # Amp
    	      get(paste0("amp", i)) - tdist * get(paste0("SE_amp", i)),
    	      get(paste0("amp", i)) + tdist * get(paste0("SE_amp", i)),
    	      # Phi
    	      get(paste0("phi", i)) - tdist * get(paste0("SE_phi", i)),
    	      get(paste0("phi", i)) + tdist * get(paste0("SE_phi", i))
  	    )
  	  }

      df <- rbind(
        c(mesor - tdist*SE_mesor, mesor + tdist*SE_mesor),
        matrix(unlist(confints), ncol = 2, byrow = TRUE)
      )
      rnames <- list()
      for(i in 1:p) {
        rnames[[i]] <- c(paste0("amp", i), paste0("phi", i))
      }
      rownames(df) <- c("mesor", unlist(rnames))
      colnames(df) <- c(paste0(100*(a/2),"%"), paste0(100*(1-a/2), "%"))

  	  # Returned
      estimates <- list(
        ci = df,
        se = se
      )
      return(estimates)

	  },
	  Individual = {

  		# Nummber of parameters
  		k <- 2*p + 1

  	  # Matrix to get standard errors and confidence intervals
    	xmat <- t(object$xmat) %*% object$xmat
  	  s <- solve(xmat)

  		# Residual sum of squared errors
  	  RSS <- sum((y - yhat)^2)
  	  sigma <- sqrt(RSS / (n - k))

  	  # Standard error for MESOR
  	  SE_mesor <- unname(sigma * sqrt(s[1,1]))

  	  # Standard error for Amplitude and Phi
  	  for(i in 1:p) {

  	    # Amplitudes
  	    amp <- sigma * sqrt(
  	      s[paste0("x",i), paste0("x", i)] * cos(get(paste0("phi", i)))^2 -
  	      2 * s[paste0("x",i), paste0("z", i)] * sin(get(paste0("phi", i))) * cos(get(paste0("phi", i))) +
  	      s[paste0("z",i), paste0("z", i)] * sin(get(paste0("phi", i)))^2
  	    )

  	    assign(paste0("SE_amp", i), amp)

  	    # Acrophase
  	    phi <- sigma * sqrt(
  	      s[paste0("x",i), paste0("x", i)] * sin(get(paste0("phi", i)))^2 -
  	      2 * s[paste0("x",i), paste0("z", i)] * sin(get(paste0("phi", i))) * cos(get(paste0("phi", i))) +
  	      s[paste0("z",i), paste0("z", i)] * cos(get(paste0("phi", i)))^2
  	    ) / get(paste0("amp", i))

  	    assign(paste0("SE_phi", i), phi)
  	  }

  	  # Save SE
  	  se <- list()
      for(i in 1:p) {
        se[[i]] <- c(paste0("SE_amp", i), paste0("SE_phi", i))
      }
  	  se <- c(SE_mesor, unlist(mget(unlist(se))))
  	  names(se)[1] <- "SE_mesor"
  	  names(se) <- gsub("SE_", "", names(se))

  	  # Confidence intervals
  	  tdist <- stats::qt(1 - a/2, df = n - k)

  	  confints <- list()
  	  for(i in 1:p) {
  	    confints[[i]] <-
  	      c(
  	        # Amp
    	      get(paste0("amp", i)) - tdist * get(paste0("SE_amp", i)),
    	      get(paste0("amp", i)) + tdist * get(paste0("SE_amp", i)),
    	      # Phi
    	      get(paste0("phi", i)) - tdist * get(paste0("SE_phi", i)),
    	      get(paste0("phi", i)) + tdist * get(paste0("SE_phi", i))
  	    )

  	  }

      df <- rbind(
        c(mesor - tdist*SE_mesor, mesor + tdist*SE_mesor),
        matrix(unlist(confints), ncol = 2, byrow = TRUE)
      )
      rnames <- list()
      for(i in 1:p) {
        rnames[[i]] <- c(paste0("amp", i), paste0("phi", i))
      }
      rownames(df) <- c("mesor", unlist(rnames))
      colnames(df) <- c(paste0(100*(a/2),"%"), paste0(100*(1-a/2), "%"))

  	  # Returned
      estimates <- list(
        ci = df,
        se = se
      )
      return(estimates)

	  }
	)

}

## Zero Amplitude Test

#' @title Zero Amplitude Test
#' @description Zero amplitude test assesses how well the circadian pattern fits
#'   the data, essentially detecting the present of a rhythm to the data.
#' @param object model of class `cosinor`
#' @param level confidence level
#' @return Returns a list of test statistics, as well prints out a report of
#'   analysis.
#' @export
cosinor_zero_amplitude <- function(object, level = 0.95) {

	if (object$type == "Population") {
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

	# Return
	list(
		fstat = fstat,
		fdist = fdist
	)

}

## Goodness of Fit

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

	if (object$type == "Population") {
		message("Goodness of fit may not be accurate for population-mean cosinor method.")
	}

	# Confidence level
	a <- 1 - level

	# Parameters
	y <- object$model[,"y"]
	t <- object$model[,"t"]
	n <- length(t)
	p <- length(object$tau)

  for (i in 1:p) {
    assign(paste0("x", i), object$model[, paste0("x", i)])
    assign(paste0("z", i), object$model[, paste0("z", i)])
  }

	xmat <- object$xmat
	yhat <- object$fitted.values
	coefs <- object$coefficients
	names(coefs) <- object$coef_names
  for (i in 1:length(coefs)) {
    assign(names(coefs)[i], unname(coefs[i]))
	}

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
  for (l in seq_along(ybar$t)) {
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
  p <- length(object$tau) # Single cosinor... may need to adjust to count terms
  fstat <- (SSLOF/(m - 1 - 2*p)) / (SSPE/(n - m))
  fdist <- stats::qf(1 - a, df1 = m - 1 - 2*p, n - m)

  # Return
  list(
    fstat = fstat,
    fdist = fdist
  )

}

## Confidence Area of Ellipse

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

	if (object$type == "Population") {
		message("Area calculations may not be accurate for population-mean cosinor method.")
	}
  if (length(object$tau) > 1) {
    message("Area calculations currently use only the principal amplitude and acrophase in multiple-component cosinor method.")
  }

	# Confidence level
	a <- 1 - level

	# Parameters
	y <- object$model[,"y"]
	t <- object$model[,"t"]
	n <- length(t)
	p <- length(object$tau)

  for (i in 1:p) {
    assign(paste0("x", i), object$model[, paste0("x", i)])
    assign(paste0("z", i), object$model[, paste0("z", i)])
  }

	xmat <- object$xmat
	yhat <- object$fitted.values
	coefs <- object$coefficients
	names(coefs) <- object$coef_names
  for (i in 1:length(coefs)) {
    assign(names(coefs)[i], unname(coefs[i]))
	}

  # Stats
  k <- 2*p + 1 # number of parameters
  RSS <- sum((y - yhat)^2)
  sigma <- sqrt(RSS / (n - k))
  fdist <- stats::qf(1 - a, df1 = k - 1, df2 = n - k)

  # Correction of sum of squares for each parameter
  xc <- 1 / n * sum((x1 - mean(x1))^2)
  zc <- 1 / n * sum((z1 - mean(z1))^2)
  tc <- 1 / n * sum((x1 - mean(x1)) * (z1 - mean(z1)))

  # Find beta and gamma CI region

  # Quadratic/ellipse formula setup
  a <- xc
  b <- 2 * tc
  c <- zc
  d <- -2 * xc * beta1 - 2 * tc * gamma1
  e <- -2 * tc * beta1 - 2 * zc * gamma1
  f <-
  	xc * beta1^2 +
  	2 * tc * beta1 * gamma1 +
  	zc * gamma1^2 -
  	(2 / n) * sigma^2 * fdist
  gmax <- -(2 * a * e - d * b) / (4 * a * c - b^2)

  # Identify parameters for ellipses
  gseq <- seq(from = gmax - amp1 * 2, to = gmax + amp1 * 2, by = amp1 / 1000)
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
    message("Confidence regions overlap the poles. Confidence intervals for amplitude and acrophase cannot be determined.")
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

## Multiple Component Cosinor Features

#' @title Multiple Component Cosinor Features
#' @description Extract the special/global features of a multiple component cosinor. In a multiple component model, there are specific parameters that are not within the model itself, but must be extracted from the model fit. When extracted, can be used to improve the plot of a multiple component cosinor. However, this is only possible if the cosinor is harmonic (see `details`). For single-component models, the orthophase is the same as the acrophase and the global amplitude
#'
#'   * Global Amplitude (Ag) = the overall amplitude is defined as half the difference between the peak and trough values
#'
#'   * Orthophase (Po) = the lag until the peak time
#'
#'   * Bathyphase (Pb) =  the lag until the trough time
#'
#' @details These calculations can only occur if the periods of the cosinor are harmonic - as in, the longest period is a integer multiple of the smallest period (known as the fundamental frequency). Otherwise, these statistics are not accurate or interpretable.
#' @param object Model of class `cosinor` with multiple periods
#' @param population If the object is a population cosinor, should the features be calculated for the individual cosinors or for the population-cosinors. Default is TRUE. This has no effect on "Individual" cosinor objects.
#'
#'    * If TRUE, then will calculate features for entire population.
#'
#'    * If FALSE, then will calculate features for every individual cosinor in the population.
#' @param ... For extensibility
#' @return When returning the cosinor features for a single model, will return an object of class `list`. When returning the cosinor features for every individual in a population cosinor, will return an object of class `tibble`.
#' @examples
#' data(twins)
#' model <- cosinor(rDYX ~ hour, twins, c(24, 8), "patid")
#' results <- cosinor_features(model, population = FALSE)
#' head(results)
#' @export
cosinor_features <- function(object, population = TRUE, ...) {

	# Is multiple component and harmonic?
	tau <- object$tau
	p <- length(tau)
	harmonic <- ifelse(length(tau) > 1 & max(tau) %% min(tau) == 0, TRUE, FALSE)
	if (harmonic) {
		message("This is a harmonic multiple-component cosinor object. The orthophase, bathyphase, and global amplitude were calculated.")
	}

	# Function to repeat internally
	features <- function(z) {

			f <- stats::splinefun(
				x = z$t, y = z$.fitted, method = "natural"
			)
			n <- nrow(z)
			xs <- seq(min(z$t), max(z$t), length.out = n)
			ys <- f(xs)
			fit <- tibble::tibble(x = xs, y = ys)

			# Return
			res <- list(
				harmonic = harmonic,
				peak = max(fit$y),
				trough = min(fit$y),
				ampGlobal = (max(fit$y) - min(fit$y))/2,
				orthophase = fit$x[which.max(fit$y)],
				bathyphase = fit$x[which.min(fit$y)]
			)

			return(res)
	}

	# Get features for each type
	if(object$type == "Individual") {
		# Object model
		aug <- augment(object)

		results <- features(aug)

	} else if(object$type == "Population" & population) {
		# Object
		aug <- augment(object)

		# Overall fit based on coefficients
			# y = M + amp * cos(2*pi*t / period + phi)
		names(object$coefficients) <- object$coef_names
		coefs <- object$coefficients

	  pars <- list()
	  for(i in 1:p) {
			pars[[i]] <-
				coefs[paste0("amp", i)] *
				cos(2*pi*aug$t / tau[i] + coefs[paste0("phi", i)])
	  }

	  df <- if(p == 1) {
		  data.frame(cbind(mesor = coefs["mesor"], pars = unlist(pars)))
	  } else if(p > 1) {
	  	data.frame(
	  		mesor = coefs["mesor"],
	  		matrix(unlist(pars), ncol = length(pars), byrow = TRUE)
	  	)
	  }
	  yhat <- rowSums(df)
	  aug$.fitted <- yhat

	  results <- features(aug)

	} else if(!(object$type == "Population" & population)) {

		# Object
		aug <- augment(object)

		# Fits are based on individuals, already made from original pop-cosinor
		results <-
			aug |>
			tidyr::nest(models = -population) |>
			dplyr::mutate(purrr::map_df(models, features)) |>
			dplyr::select(-models)

	}

	# Return
	return(results)

}
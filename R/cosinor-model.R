# Single Cosinor Implementation {{{ ====

#' @description Model fitting algorithm for cosinor. Results in output that define the new S3 class, as seen by the [hardhat::new_model], which generates the `new_cosinor` function.
#' @noRd
cosinor_impl <- function(predictors, outcomes, tau) {

  ## Parameters for normal equations {{{ ====

  # Formal equation
  # y(t) = M + A*cos(2*pi*t/period + phi)
  # y(t) = M + beta*x + gamma*z + error(t)
  # beta = A*cos(phi)
  # gamma = -A*sin(phi)
  # x = cos(2*pi*t/period)
  # z = sin(2*pi*t/period)
  # M = MESOR
  # A = Amplitude
  # phi = acrophase (measure of hte time of overall high values in cycle)

  # RSS = sum[y - (M + beta*x + gamma*z)]^2

  # Normal equations (where M, beta, gamma are the coefficients to solve for)
  # sum(y) = M*n + beta*sum(x) + gamma*sum(z)
  # sum(y*x) = M*sum(x) + beta*sum(x^2) + gamma*sum(x*z)
  # sum(y*z) = M*sum(z) + beta*sum(x*z) + gamma*sum(z^2)

  # Parameters using the predictors (time) and outcomes (y)
  y <- outcomes
  t <- predictors
  n <- length(t)
  period <- tau
  x <- cos((2 * pi * t) / period)
  z <- sin((2 * pi * t) / period)

  # Return / save the model
  model <- cbind(y, t, x, z)

  # Matrices
  ymat <- as.matrix(cbind(y = c(sum(y), sum(y * x), sum(y * z))))
  mcol <- c(n, sum(x), sum(z)) # Mesor column
  bcol <- c(sum(x), sum(x^2), sum(x * z)) # Beta column
  gcol <- c(sum(z), sum(x * z), sum(z^2)) # Gamma column
  xmat <- as.matrix(cbind(mesor = mcol, beta = bcol, gamma = gcol))

  # }}}

  ## Solve System of Equations {{{ ====

  coefs <- solve(t(xmat) %*% xmat, tol = 1e-21) %*% (t(xmat) %*% ymat)
  mesor <- coefs[1] # mesor
  beta <- coefs[2] # beta
  gamma <- coefs[3] # gamma

  # Amplitude
  amp <- sqrt(beta^2 + gamma^2)

  # Acrophase (phi) must be in correct quadrant
  sb <- sign(beta)
  sg <- sign(gamma)
  theta <- atan(abs(gamma / beta))

  if ((sb == 1 | sb == 0) & sg == 1) {
    phi <- -theta
  } else if (sb == -1 & (sg == 1 | sg == 0)) {
    phi <- theta - pi
  } else if ((sb == -1 | sb == 0) & sg == -1) {
    phi <- -theta - pi
  } else if (sb == 1 & (sg == -1 | sg == 0)) {
    phi <- theta - (2 * pi)
  }

  # Prediction
  yhat <- mesor + beta * x + gamma * z

  # }}}

  ## Model Output {{{ ====

  # Model coefficients
  coefs <- unname(c(mesor, amp, phi, beta, gamma))
  coef_names <- c("mesor", "amp", "phi", "beta", "gamma")

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

  # }}}
}

# }}}

# Population Mean Cosinor Implementation {{{ ====

#' @description Model fitting algorithm for population-mean cosinor. Uses the
#'   `cosinor_impl()` algorithm to derive individual parameters.
#' @noRd
cosinor_pop_impl <- function(predictors, outcomes, tau, population) {

  ## Population cosinor parameter setup {{{ ====

  # Period
  period <- tau

  # Create data frame for split/apply approach
  df <- data.frame(predictors, outcomes, population)

  # Remove patients with only 2 observations (will cause a det ~ 0 error)
  counts <- by(df, df[, "population"], nrow)
  lowCounts <- as.numeric(names(counts[counts <= 2]))
  df <- subset(df, !(population %in% lowCounts))

  # Message about population count removal
  if(length(lowCounts) != 0) {
    message(length(lowCounts), " subjects were removed due to having insufficient observations.")
  }

  # Population parameters
  k <- length(unique(df$population)) # Number of individuals
  p <- 1 # Number of parameters such that 2p + 1 = 3 for single cosinor
  y <- df$outcomes
  t <- df$predictors
  n <- length(t)
  population <- df$population
  x <- cos((2 * pi * t) / period)
  z <- sin((2 * pi * t) / period)

  # Return / save the model
  model <- cbind(y, t, x, z, population)

  # Create matrix that we can apply cosinor to subgroups
  kCosinors <- with(
    df,
    by(df, population, function(x) {
      cosinor_impl(x$predictors, x$outcomes, period)
    })
  )

  ## }}}

  ## Coefficients {{{ ====

  # Fits of individual cosinors
  tmp <- sapply(kCosinors, stats::fitted)
  fits <- data.frame(
    population = rep(names(tmp), sapply(tmp, length)),
    yhat = unlist(tmp)
  )

  # Matrix of coefficients
  tbl <- sapply(kCosinors, stats::coef, USE.NAMES = TRUE)
  coef_names <- c("mesor", "amp", "phi", "beta", "gamma")
  rownames(tbl) <- coef_names
  xmat <- t(tbl)

  # Get mean for each parameter (mesor, beta, gamma), ignoring averaged amp/phi
  coefs <- apply(xmat, MARGIN = 2, function(x) {
    sum(x) / k
  })

  mesor <- unname(coefs["mesor"])
  beta <- unname(coefs["beta"])
  gamma <- unname(coefs["gamma"])

  # Get amplitude
  amp <- sqrt(beta^2 + gamma^2)

  # Acrophase (phi) must be in correct quadrant
  sb <- sign(beta)
  sg <- sign(gamma)
  theta <- atan(abs(gamma / beta))

  if ((sb == 1 | sb == 0) & sg == 1) {
    phi <- -theta
  } else if (sb == -1 & (sg == 1 | sg == 0)) {
    phi <- theta - pi
  } else if ((sb == -1 | sb == 0) & sg == -1) {
    phi <- -theta - pi
  } else if (sb == 1 & (sg == -1 | sg == 0)) {
    phi <- theta - (2 * pi)
  }

  # Final coefficients
  coefs["amp"] <- amp
  coefs["phi"] <- phi

  ## }}}

  ## Model diagnostics / fit {{{ ====

  # Sigma / variances
  sMesor <- sqrt(sum((xmat[, "mesor"] - mesor)^2) / (k-1))
  sBeta <- sqrt(sum((xmat[, "beta"] - beta)^2) / (k-1))
  sGamma <- sqrt(sum((xmat[, "gamma"] - beta)^2) / (k-1))

  # Confidence intervals
  a <- 0.05
  tdist <- stats::qt(1 - a/2, df = k - 1)
  mesorSE <- tdist * sMesor / sqrt(k)

  # Zero amplitude test
  sBetaGamma <- sqrt(sum((xmat[,"beta"] - beta) * (xmat[,"gamma"] - gamma)))
  r <- sBetaGamma / (sBeta * sGamma)
  fdist <- stats::qf(1 - a, df1 = 2, df2 = k - 2)
  fstat <- abs(((k * (k - 2)) / (2 * (k - 1)))) *
    abs((1 / (1 - r^2))) *
    abs(((beta^2 / sBeta^2) - ((2 * r * beta * gamma) / (sBeta * sGamma)) + (gamma^2 / sGamma^2)))

  ## }}}

  ## Model output {{{ ====

  # Fitted values
  # y(t) = M + A*cos(2*pi*t/period + phi)

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
  ## }}}


}

# }}}
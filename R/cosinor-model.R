# Cosinor Implementation {{{ ====

#' @description Model fitting algorithm for cosinor. Results in output that define the new S3 class, as seen by the [hardhat::new_model], which generates the `new_cosinor` function.
#' @noRd
cosinor_impl <- function(predictors, outcomes, tau) {

  ## Parameters for normal equations {{{ ====

  # Validate predictors
  hardhat::validate_predictors_are_numeric(predictors)

  # Period of 24 hours in a day
  alpha <- 0.05

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
  y <- outcomes[[1]]
  t <- predictors[[1]]
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

  coefs <- solve(t(xmat) %*% xmat) %*% (t(xmat) %*% ymat)
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

  # Elliptical method for confidence intervals {{{ ====

  # Stats
  RSS <- sum((y - yhat)^2)
  sigma <- sqrt(RSS / (n - 3))

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

  # }}}

  ## Model Output {{{ ====

  # Model coefficients
  coefs <- unname(c(mesor, amp, phi, beta, gamma))
  coef_names <- c("mesor", "amp", "phi", "beta", "gamma")

  # Fit and residuals
  fitted.values <- yhat
  residuals <- y - yhat

  # Area of ellipse for circadian rhythmicity
  area <- cbind(gseq, bs1, bs2)

  # Make call
  call <- call("cosinor", paste0(names(outcomes), " ~ ", names(predictors)))

  # List to return
  list(

    # Raw coefficients
    coefficients = coefs,
    coef_names = coef_names,

    # Fitted and residual values
    fitted.values = fitted.values,
    residuals = residuals,

    # Function call
    call = call,

    # Overall model of cosinor
    model = model,

    # Matrices used
    xmat = xmat,
    ymat = ymat,

    # Description of ellipse that generates CI
    area = area
  )

  # }}}
}

# }}}
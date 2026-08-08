# Cosinor Implementation ----

## Single Component Cosinor Implementation

#' @description Model fitting algorithm for cosinor. Results in output that
#'   define the new S3 class, as seen by the [hardhat::new_model], which
#'   generates the `new_cosinor()` function.
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
  # 	e.g. single component = 3 components, where 3 = 2p + 1 (p = 1 component)
  p <- length(tau)

  # Create null variables
  mesor <- NULL
  for (i in 1:p) {
    assign(paste0("x", i), NULL)
    assign(paste0("z", i), NULL)
    assign(paste("amp", i), NULL)
    assign(paste("phi", i), NULL)
    assign(paste("beta", i), NULL)
    assign(paste("gamma", i), NULL)
  }

  # Single parameters
  y <- outcomes
  t <- predictors
  n <- length(t)

  # Normal equation for 3 components
  # 	Normal equations (where M, beta, gamma are the coefficients to solve for)
  # 	sum(y) = M*n + beta*sum(x) + gamma*sum(z)
  # 	sum(y*x) = M*sum(x) + beta*sum(x^2) + gamma*sum(x*z)
  # 	sum(y*z) = M*sum(z) + beta*sum(x*z) + gamma*sum(z^2)
  # 	d = Su (for single component, 3 equations with 3 unknowns)

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

  # Refused before `solve()` reaches it, so the message names the shortfall
  # rather than reporting an exactly singular system
  if (nrow(xmat) < 2 * p + 1) {
    stop(
      "A ",
      p,
      "-component cosinor needs at least ",
      2 * p + 1,
      " observations to estimate its ",
      2 * p + 1,
      " parameters; only ",
      nrow(xmat),
      " were given.",
      call. = FALSE
    )
  }

  ### Solving for coefficients

  # Solve for coefficients, including amplitude and acrophase
  coefs <- solve(t(xmat) %*% xmat) %*% t(xmat) %*% ymat
  mesor <- coefs[1]

  for (i in 1:p) {
    # Beta and gamma terms
    assign(paste0("beta", i), unname(coefs[paste0("x", i), ]))
    assign(paste0("gamma", i), unname(coefs[paste0("z", i), ]))

    # Amplitude
    assign(
      paste0("amp", i),
      sqrt(get(paste0("beta", i))^2 + get(paste0("gamma", i))^2)
    )

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

  coefs <- unlist(c(
    mesor = mesor,
    mget(paste0("amp", 1:p)),
    mget(paste0("phi", 1:p)),
    mget(paste0("beta", 1:p)),
    mget(paste0("gamma", 1:p))
  ))

  # Predicted / output
  # y(t) = M + b1 * x1 + g1 * z1 + b2 * x2 + g2 * z2
  # y(t) = M + amp1 * cos(2*pi*t/tau1 + phi1) + amp2 * cos(2*pi*t/tau2 + phi2)

  pars <- list()
  for (i in 1:p) {
    pars[[i]] <- get(paste0("amp", i)) *
      cos(2 * pi * t / tau[i] + get(paste0("phi", i)))
  }
  df <- data.frame(
    mesor = mesor,
    matrix(unlist(pars), ncol = length(pars), byrow = FALSE)
  )
  yhat <- rowSums(df)

  ### Model Output

  # Model coefficients
  coef_names <- names(coefs)
  coefs <- unname(coefs)

  # Fit and residuals
  fitted.values <- yhat
  residuals <- y - yhat

  ### Covariance parts

  # Computed here, where `n` and `xmat` are both in scope and consistent, rather
  # than recomputed by each statistical method. `stats::model.frame()` drops
  # incomplete rows, so `nrow(xmat)` is the count the covariance is built on and
  # `nrow(model)` is not.
  nobs <- nrow(xmat)
  dfResidual <- nobs - (2 * p + 1)
  RSS <- sum(residuals[seq_len(nobs)]^2)

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
    xmat = xmat,

    # Parts every downstream statistic needs
    parts = list(
      XtXinv = solve(t(xmat) %*% xmat),
      RSS = RSS,
      sigma = sqrt(RSS / dfResidual),
      nobs = nobs,
      df.residual = dfResidual,
      kappa = cosinor_condition(xmat)
    )
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

  # Create null variables based on number of parameters
  mesor <- NULL
  for (i in 1:p) {
    assign(paste0("x", i), NULL)
    assign(paste0("z", i), NULL)
    assign(paste("amp", i), NULL)
    assign(paste("phi", i), NULL)
    assign(paste("beta", i), NULL)
    assign(paste("gamma", i), NULL)
  }

  # Create data frame for split/apply approach
  df <- stats::na.omit(data.frame(predictors, outcomes, population))

  # Remove patients with only p observations (will cause a det ~ 0 error).
  # Compared as character throughout: coercing the names with `as.numeric()`
  # turned every non-numeric subject identifier into NA, so the filter dropped
  # nobody and the per-subject `solve()` then died on a singular matrix.
  counts <- by(df, df[, "population"], nrow)
  lowCounts <- names(counts)[counts <= 2 * p + 1]
  df <- subset(df, !(as.character(population) %in% lowCounts))

  # Message about population count removal
  if (length(lowCounts) != 0) {
    message(
      length(lowCounts),
      " subjects were removed due to having insufficient observations."
    )
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
  model <- data.frame(
    y,
    t,
    mget(paste0("x", 1:p)),
    mget(paste0("z", 1:p)),
    population
  )

  # Create matrix that we can apply cosinor to subgroups. A poorly conditioned
  # design would warn once per subject here, so it is collected and reported
  # once by the bridge instead.
  df$.row <- seq_len(nrow(df))
  kCosinors <- suppressWarnings(with(
    df,
    by(df, population, function(.x) {
      cosinor_impl(.x$predictors, .x$outcomes, tau)
    })
  ))

  ### Coefficients

  # Fits of individual cosinors
  # Must be a data frame to have column names
  kfits <- sapply(kCosinors, stats::fitted, USE.NAMES = TRUE)
  if (inherits(kfits, "matrix")) {
    kfits <- as.data.frame(kfits)
  }

  # `by()` walks the groups in sorted order, so concatenating the per-subject
  # fits gives a vector ordered by subject while `y` and `model` stay in input
  # order. Scattering through the row index each group carried keeps the fitted
  # values and residuals aligned with the rows they belong to; before this, both
  # were correct only when the input happened to be sorted by subject.
  rowOrder <- unlist(
    by(df, df$population, function(.x) .x$.row),
    use.names = FALSE
  )
  fittedByRow <- numeric(nrow(df))
  fittedByRow[rowOrder] <- unlist(kfits, use.names = FALSE)
  df$.row <- NULL

  # Coefficient table
  tbl <- sapply(kCosinors, stats::coef, USE.NAMES = TRUE)
  coef_names <- c(
    "mesor",
    paste0("amp", 1:p),
    paste0("phi", 1:p),
    paste0("beta", 1:p),
    paste0("gamma", 1:p)
  )
  rownames(tbl) <- coef_names
  xmat <- t(tbl)

  # Get mean for each parameter (mesor, beta, gamma)
  # Will need to recalculate the valvues for amplitude & acrophase
  # Creates a vector of values we can overwrite
  coefs <- colMeans(xmat, na.rm = TRUE)

  for (i in 1:p) {
    # Get the beta and gamma parameter names from the table
    # Calculate the mean of each
    beta <- mean(xmat[, paste0("beta", i)], na.rm = TRUE)
    gamma <- mean(xmat[, paste0("gamma", i)], na.rm = TRUE)

    # Calculate population amplitude
    # Uses trigonometric approach
    amp <- sqrt(beta^2 + gamma^2)

    # Acrophase = phi, calculated with the arctangent
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

    # Final coefs
    # Assign terms for final coefs
    coefs[paste0("amp", i)] <- amp
    coefs[paste0("phi", i)] <- phi
  }

  ### Model output

  # Fitted values
  # y(t) = M + A*cos(2*pi*t/tau + phi)

  # Individual fits

  # Overall model
  yhat <- fittedByRow

  ### Covariance parts

  # The population estimator is a mean over subjects, so the subject is the
  # random unit and the covariance is the between-subject one divided by k. The
  # amplitude and acrophase columns of `xmat` are functions of beta and gamma
  # and carry no additional information, so the free parameters are the same
  # 2p + 1 as for an individual fit.
  linearNames <- cosinor_par_names(p, type = "linear")
  popNames <- c("mesor", paste0(rep(c("beta", "gamma"), p), rep(1:p, each = 2)))

  V <- stats::cov(xmat[, popNames, drop = FALSE], use = "complete.obs") / k
  dimnames(V) <- list(linearNames, linearNames)

  # One condition number per subject would be k warnings, so they are summarised
  # instead. The median is what gets tested: a single sparsely sampled subject
  # among hundreds has a badly conditioned design of its own but contributes
  # 1/k of the mean, which is not a reason to distrust the population estimate.
  # The count of affected subjects is carried alongside, since that is the
  # number a user can act on.
  kappas <- vapply(kCosinors, function(.x) .x$parts$kappa, numeric(1))
  threshold <- getOption("card.cosinor.kappa", 30)

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
    xmat = xmat,

    # Parts every downstream statistic needs
    parts = list(
      V = V,
      nobs = n,
      nsubjects = k,
      nsubjects_dropped = length(lowCounts),
      df.residual = k - 1,
      kappa = stats::median(kappas, na.rm = TRUE),
      kappa_max = max(kappas, na.rm = TRUE),
      nsubjects_ill_conditioned = sum(kappas > threshold, na.rm = TRUE)
    )
  )
}

# Statistical Methods ----

## Zero Amplitude Test

#' @title Zero Amplitude Test
#'
#' @description Tests the null hypothesis that every component of a [cosinor]
#'   model has zero amplitude, which is the test for whether the data carry a
#'   rhythm at all.
#'
#' @details The null is \eqn{H_0: \beta_j = \gamma_j = 0} for all `j`, so a
#'   `p`-component model contributes `2p` parameters to the numerator:
#'
#'   \deqn{F = \frac{MSS / 2p}{RSS / (N - 2p - 1)}}
#'
#'   compared against \eqn{F_{1-\alpha}(2p, N - 2p - 1)}.
#'
#'   Earlier versions of this function fixed the numerator at 2 degrees of
#'   freedom and the denominator at `N - 3` whatever `tau` was given, so a
#'   two-component model on the `twins` data reported `F = 707` on `(2, 16383)`
#'   where the test is `F = 354` on `(4, 16381)`. The statistic was inflated in
#'   proportion to the number of components.
#'
#'   For a population-mean cosinor the same expression is evaluated against the
#'   between-subject covariance on \eqn{k - 1} degrees of freedom, giving the
#'   Hotelling \eqn{T^{2}} test of Bingham et al. (1982).
#'
#' @param object model of class `cosinor`
#'
#' @param level confidence level used for the reported critical value
#'
#' @return A list with the `fstat` observed, the `fdist` critical value at
#'   `level`, the numerator and denominator degrees of freedom `df1` and `df2`,
#'   and the `p.value`.
#'
#' @examples
#' data(twins)
#' model <- cosinor(rDYX ~ hour, twins, tau = c(24, 12))
#' cosinor_zero_amplitude(model)
#'
#' @references Cornelissen G. Cosinor-based rhythmometry. *Theoretical Biology
#'   and Medical Modelling* 2014;11:16. \doi{10.1186/1742-4682-11-16}
#'
#' @seealso [anova.cosinor()] for the same test applied one component at a time
#'
#' @export
cosinor_zero_amplitude <- function(object, level = 0.95) {
  p <- length(object$tau)

  # H0: beta_j = gamma_j = 0 for every component, so every non-mesor parameter
  # enters the numerator. For least squares this Wald form is algebraically
  # identical to comparing residual sums of squares against the mesor-only fit.
  parm <- setdiff(cosinor_par_names(p, "linear"), "mesor")
  test <- cosinor_wald(object, parm)

  list(
    fstat = test$statistic,
    fdist = stats::qf(level, df1 = test$df1, df2 = test$df2),
    df1 = test$df1,
    df2 = test$df2,
    p.value = test$p.value
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
  # Refused rather than warned about: the partition of the residual sum of
  # squares into lack of fit and pure error assumes one fit against replicate
  # observations at shared time points, which a pooled per-subject fit does not
  # give. The F statistic would still print, and would still be wrong.
  if (object$type == "Population") {
    stop(
      "`cosinor_goodness_of_fit()` is not defined for a population-mean ",
      "cosinor, whose residuals come from one fit per subject rather than from ",
      "a single model. Fit an individual cosinor to test lack of fit.",
      call. = FALSE
    )
  }

  # Confidence level
  a <- 1 - level

  # Parameters
  y <- object$model[, "y"]
  t <- object$model[, "t"]
  n <- length(t)
  p <- length(object$tau)

  # Create null variables
  mesor <- NULL
  for (i in 1:p) {
    assign(paste0("x", i), NULL)
    assign(paste0("z", i), NULL)
    assign(paste("amp", i), NULL)
    assign(paste("phi", i), NULL)
    assign(paste("beta", i), NULL)
    assign(paste("gamma", i), NULL)
  }

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
  fstat <- (SSLOF / (m - 1 - 2 * p)) / (SSPE / (n - m))
  fdist <- stats::qf(1 - a, df1 = m - 1 - 2 * p, n - m)

  # Return
  list(
    fstat = fstat,
    fdist = fdist
  )
}

## Confidence Area of Ellipse

#' @title Confidence Ellipse For A Cosinor Component
#'
#' @description The joint confidence region for a component's `(beta, gamma)`
#'   pair, together with the conservative amplitude and acrophase limits derived
#'   from it.
#'
#' @details Under Gaussian errors \eqn{(\hat\beta_j, \hat\gamma_j)} is exactly
#'   bivariate normal, so the joint region is an ellipse rather than a rectangle
#'   of marginal intervals. Conservative limits follow from sweeping its boundary:
#'   the smallest and largest distance from the pole give the amplitude, the
#'   tangent radii give the acrophase (Bingham et al. 1982; Cornelissen 2014).
#'
#'   These respect the parameter space where a symmetric Wald interval does not.
#'   When the region covers the pole the rhythm is not distinguishable from zero
#'   amplitude, the amplitude lower limit is zero and the acrophase is not
#'   identifiable at all - reported as `NA` rather than as an interval, because
#'   the honest answer is the whole circle.
#'
#'   Earlier versions computed these limits and then returned only the plotting
#'   coordinates, so none of it reached the caller.
#'
#' @param object Model of class `cosinor`
#'
#' @param level Confidence level requested
#'
#' @param component Which component to describe, for a multiple-component model.
#'   Defaults to the first.
#'
#' @param ... Not currently used, but required for extensibility.
#'
#' @return A list with `area`, the boundary coordinates used for plotting;
#'   `limits`, a matrix of amplitude and acrophase confidence limits;
#'   `covers_pole`, whether the region includes the origin; and `component`.
#'
#' @examples
#' data(twins)
#' model <- cosinor(rDYX ~ hour, twins, tau = c(24, 12))
#' cosinor_area(model, component = 2)$limits
#'
#' @references Bingham C, Arbogast B, Cornelissen G, Lee J, Halberg F.
#'   Inferential statistical methods for estimating and comparing cosinor
#'   parameters. *Chronobiologia* 1982;9(4):397-439.
#'
#'   Cornelissen G. Cosinor-based rhythmometry. *Theoretical Biology and Medical
#'   Modelling* 2014;11:16. \doi{10.1186/1742-4682-11-16}
#'
#' @seealso [confint.cosinor()] with `method = "ellipse"`, [ggellipse()]
#'
#' @export
cosinor_area <- function(object, level = 0.95, component = 1, ...) {
  if (object$type == "Population") {
    stop(
      "`cosinor_area()` is not defined for a population-mean cosinor. The ",
      "ellipse is built from the residual covariance of a single fit, which a ",
      "pooled per-subject model does not have.",
      call. = FALSE
    )
  }

  p <- length(object$tau)
  if (
    !is.numeric(component) ||
      length(component) != 1 ||
      !component %in% seq_len(p)
  ) {
    stop(
      "`component` must be a single number between 1 and ",
      p,
      "; it was given as ",
      paste(component, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  parts <- cosinor_vcov_parts(object)
  co <- cosinor_coefficients(object, type = "linear")
  nms <- paste0(c("beta", "gamma"), component)
  centre <- co[nms]
  V <- parts$V[nms, nms, drop = FALSE]

  # The region is (bhat - b)' V^-1 (bhat - b) <= 2 * F(2, df). Boundary points
  # are centre + sqrt(crit) * R' u for R'R = V and |u| = 1, which traces the
  # ellipse exactly rather than solving the conic for one axis at a time.
  crit <- 2 * stats::qf(level, df1 = 2, df2 = parts$df.residual)
  angles <- seq(0, 2 * pi, length.out = 2048)
  R <- chol(V)
  boundary <- sqrt(crit) * cbind(cos(angles), sin(angles)) %*% R
  boundary[, 1] <- boundary[, 1] + centre[[1]]
  boundary[, 2] <- boundary[, 2] + centre[[2]]

  # Evaluating the same quadratic form at the pole is the component's own
  # zero-amplitude test, so the geometry and the F test cannot disagree
  coversPole <- drop(t(centre) %*% solve(V) %*% centre) <= crit

  # `ggellipse()` draws the region as two curves of beta against a shared
  # gamma, so the closed boundary is cut at its extremes in gamma into the two
  # branches and both are interpolated onto one grid
  gam <- boundary[, 2]
  bet <- boundary[, 1]
  idx <- seq_along(gam)
  lo <- which.min(gam)
  hi <- which.max(gam)
  first <- if (lo <= hi) idx[lo:hi] else c(idx[lo:length(idx)], idx[1:hi])
  second <- setdiff(idx, first)

  gseq <- seq(min(gam), max(gam), length.out = 512)
  bs1 <- stats::approx(gam[first], bet[first], xout = gseq, rule = 2)$y
  bs2 <- stats::approx(gam[second], bet[second], xout = gseq, rule = 2)$y

  limits <- suppressWarnings(
    stats::confint(
      object,
      parm = paste0(c("amp", "phi"), component),
      level = level,
      method = "ellipse"
    )
  )

  if (coversPole) {
    warning(
      "The confidence region for component ",
      component,
      " covers the pole, so its acrophase is not identifiable and its limits ",
      "are returned as NA. The rhythm is not distinguishable from zero ",
      "amplitude at this level.",
      call. = FALSE
    )
  }

  list(
    area = cbind(gseq = gseq, bs1 = bs1, bs2 = bs2),
    limits = limits,
    covers_pole = coversPole,
    component = component
  )
}

## Multiple Component Cosinor Features

#' @title Multiple Component Cosinor Features
#'
#' @description Extract the special/global features of a multiple component
#'   cosinor. In a multiple component model, there are specific parameters that
#'   are not within the model itself, but must be extracted from the model fit.
#'   When extracted, can be used to improve the plot of a multiple component
#'   cosinor. However, this is only possible if the cosinor is harmonic (see
#'   `details`). For single-component models, the orthophase is the same as the
#'   acrophase and the global amplitude
#'
#'   * Global Amplitude (Ag) = the overall amplitude is defined as half the difference between the peak and trough values
#'
#'   * Orthophase (Po) = the lag until the peak time
#'
#'   * Bathyphase (Pb) =  the lag until the trough time
#'
#' @details These calculations can only occur if the periods of the cosinor are
#'   harmonic - as in, the longest period is a integer multiple of the smallest
#'   period (known as the fundamental frequency). Otherwise, these statistics
#'   are not accurate or interpretable.
#'
#' @param object Model of class `cosinor` with multiple periods
#'
#' @param population If the object is a population cosinor, should the features
#'   be calculated for the individual cosinors or for the population-cosinors.
#'   Default is TRUE. This has no effect on "Individual" cosinor objects.
#'
#'    * If TRUE, then will calculate features for entire population.
#'
#'    * If FALSE, then will calculate features for every individual cosinor in the population.
#'
#' @param ... For extensibility
#'
#' @return When returning the cosinor features for a single model, will return
#'   an object of class `list`. When returning the cosinor features for every
#'   individual in a population cosinor, will return an object of class
#'   `tibble`.
#'
#' @examples
#' data(twins)
#' model <- cosinor(rDYX ~ hour, twins, c(24, 8), "patid")
#' results <- cosinor_features(model, population = FALSE)
#' head(results)
#'
#' @export
cosinor_features <- function(object, population = TRUE, ...) {
  # Object components
  tau <- object$tau
  p <- length(tau)

  # Create null variables
  mesor <- NULL
  for (i in 1:p) {
    assign(paste0("x", i), NULL)
    assign(paste0("z", i), NULL)
    assign(paste("amp", i), NULL)
    assign(paste("phi", i), NULL)
    assign(paste("beta", i), NULL)
    assign(paste("gamma", i), NULL)
  }
  models <- NULL

  # Is multiple component and harmonic? Every period must divide the
  # fundamental, not just the shortest one: comparing only the extremes called
  # `c(24, 5, 12)` non-harmonic on account of the 5 while missing that it also
  # misjudges sets where the extremes happen to divide but an inner period does
  # not. Compared on a tolerance because 24/3 is not exactly representable.
  fundamental <- max(tau)
  ratios <- fundamental / tau
  harmonic <- length(tau) > 1 &&
    all(abs(ratios - round(ratios)) < sqrt(.Machine$double.eps))
  if (harmonic) {
    message(
      "This is a harmonic multiple-component cosinor object. The orthophase, bathyphase, and global amplitude were calculated."
    )
  }

  # Function to repeat internally
  features <- function(z) {
    f <- stats::splinefun(
      x = z$t,
      y = z$.fitted,
      method = "natural"
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
      ampGlobal = (max(fit$y) - min(fit$y)) / 2,
      orthophase = fit$x[which.max(fit$y)],
      bathyphase = fit$x[which.min(fit$y)]
    )

    return(res)
  }

  # Get features for each type
  if (object$type == "Individual") {
    # Object model
    aug <- augment(object)

    results <- features(aug)
  } else if (object$type == "Population" & population) {
    # Object
    aug <- augment(object)

    # Overall fit based on coefficients
    # y = M + amp * cos(2*pi*t / period + phi)
    names(object$coefficients) <- object$coef_names
    coefs <- object$coefficients

    pars <- list()
    for (i in 1:p) {
      pars[[i]] <-
        coefs[paste0("amp", i)] *
        cos(2 * pi * aug$t / tau[i] + coefs[paste0("phi", i)])
    }

    df <- if (p == 1) {
      data.frame(cbind(mesor = coefs["mesor"], pars = unlist(pars)))
    } else if (p > 1) {
      # byrow = FALSE, matching `cosinor_impl()` and `predict_cosinor_numeric()`.
      # Filling by row interleaved the components across observations, so the
      # peak and trough of a multiple-component population fit were taken from a
      # curve that was not the fitted one.
      data.frame(
        mesor = coefs["mesor"],
        matrix(unlist(pars), ncol = length(pars), byrow = FALSE)
      )
    }
    yhat <- rowSums(df)
    aug$.fitted <- yhat

    results <- features(aug)
  } else if (!(object$type == "Population" & population)) {
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

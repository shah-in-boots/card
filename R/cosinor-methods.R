# Cosinor Extractor Methods ----

# This file holds every quantity derived from a fitted cosinor: the parameter
# covariance, the delta-method Jacobian, the Wald tests, and the confidence
# intervals built on them.
#
# It exists because those quantities used to be rebuilt independently inside
# `confint()`, `cosinor_zero_amplitude()`, `cosinor_goodness_of_fit()` and
# `cosinor_area()`. Five copies of `solve(t(xmat) %*% xmat)` meant five places a
# formula could be wrong, and one of them was: the acrophase standard error
# carried the wrong sign on its cross term for as long as the package has
# existed.
#
# TWO PARAMETERISATIONS
#
#   linear   M, beta_1, gamma_1, ..., beta_p, gamma_p     what OLS solves for
#   cosinor  M, A_1,    phi_1,   ..., A_p,    phi_p       what is interpreted
#
#   beta_j  =  A_j cos(phi_j)          A_j   = sqrt(beta_j^2 + gamma_j^2)
#   gamma_j = -A_j sin(phi_j)          phi_j = atan2(-gamma_j, beta_j)
#
# TWO COVARIANCE SOURCES
#
#   Individual  sigma^2 (X'X)^-1                  residual is the random unit
#   Population  cov(per-subject coefs) / k        subject is the random unit
#
# Both are (2p+1) x (2p+1) over the linear parameters, so everything downstream
# is shared; the only branch is which covariance and which reference
# distribution (t on n-2p-1 against t on k-1).
#
# THE CROSS-TERM SIGN
#
#   dA/dbeta   =  cos(phi)          dA/dgamma   = -sin(phi)
#   dphi/dbeta = -sin(phi)/A        dphi/dgamma = -cos(phi)/A
#
# The cross term takes the sign of the product of the two gradient components,
# so it is NEGATIVE for the amplitude and POSITIVE for the acrophase:
#
#   Var(A)   = s_bb cos^2(phi) - 2 s_bg sin(phi)cos(phi) + s_gg sin^2(phi)
#   Var(phi) = [s_bb sin^2(phi) + 2 s_bg sin(phi)cos(phi) + s_gg cos^2(phi)]/A^2
#
# This matches Cornelissen (2014). Deriving the Jacobian once, in
# `cosinor_jacobian()`, is what stops the two from being written out separately
# and drifting apart again.

# The generics these methods extend have to be visible in the namespace for S3
# registration to resolve them, even though every call site writes `stats::`.
#' @importFrom stats anova coef confint df.residual logLik nobs sigma vcov
NULL

## Internal Helpers ----

#' @title Cosinor Parameter Names
#' @description Parameter names for a `p`-component cosinor, in the interleaved
#'   order the design matrix uses - the mesor, then each component's pair in
#'   turn. This is deliberately not the order `coef_names` stores on the object,
#'   which groups by parameter type rather than by component.
#' @param p number of components
#' @param type either `"linear"` for the regression coefficients or `"cosinor"`
#'   for the amplitude and acrophase parameterisation
#' @return a character vector of length `2 * p + 1`
#' @noRd
cosinor_par_names <- function(p, type = c("linear", "cosinor")) {
  type <- match.arg(type)
  pair <- if (type == "linear") c("beta", "gamma") else c("amp", "phi")
  c("mesor", paste0(rep(pair, p), rep(seq_len(p), each = 2)))
}

#' @title Acrophase From Regression Coefficients
#' @description Converts a `(beta, gamma)` pair to the acrophase on the
#'   `[-2*pi, 0]` convention the package reports.
#' @details Replaces a four-branch quadrant chain that had no branch for
#'   `beta == gamma == 0`; inside a loop over components that case silently
#'   inherited the previous component's acrophase. `atan2()` handles every
#'   quadrant and returns `-2*pi` at the origin, which is at least deterministic.
#' @param beta,gamma numeric vectors of regression coefficients
#' @return a numeric vector of acrophases in `[-2*pi, 0]`
#' @noRd
cosinor_acrophase <- function(beta, gamma) {
  a <- atan2(-gamma, beta)
  ifelse(a >= 0, a - 2 * pi, a)
}

#' @title Identifiability Of Cosinor Periods
#'
#' @description How [cosinor()] decides whether the periods it was given can be
#'   told apart by the data, and why it does not use the criterion the
#'   chronobiology literature would suggest.
#'
#' @details A multiple-component model can only separate two periods if the
#'   design carries enough information to distinguish them. The textbook
#'   criterion is spectral resolution: two frequencies are separable when
#'   \eqn{|1/\tau_i - 1/\tau_j| > 1/T} for an observation span \eqn{T}
#'   (Cornelissen 2014).
#'
#'   That criterion cannot be applied here. Time indices are routinely folded -
#'   the bundled [twins] data records the clock hour, so its span is 23 hours
#'   however many days were pooled into it - and a span-based check rejects
#'   `tau = c(24, 12)` on that data, which is both the documented example and a
#'   perfectly well-conditioned fit.
#'
#'   The design matrix answers the question the span cannot. `cosinor()`
#'   therefore reports the condition number \eqn{\kappa(X)} and warns when it
#'   exceeds 30, a threshold that separates every well-posed case from the
#'   degenerate ones by more than an order of magnitude:
#'
#'   | `tau` | \eqn{\kappa(X)} on `twins` |
#'   | --- | --- |
#'   | `24` | 1.4 |
#'   | `c(24, 12)` | 1.5 |
#'   | `c(24, 8)` | 1.5 |
#'   | `c(24, 12, 8)` | 1.5 |
#'   | `c(24, 23.5)` | 129 |
#'
#'   The warning is not an error, because closely spaced periods are sometimes
#'   fitted deliberately - a free-running rhythm against a 24-hour zeitgeber, for
#'   instance. It is worth heeding: `tau = c(24, 23.5)` on `twins` returns
#'   amplitudes of 7.4 and 7.1 against a single-component amplitude of 0.30,
#'   because two near-collinear components can grow without bound as long as they
#'   cancel. The condition number is stored on the object and printed by
#'   [summary()] for that reason, rather than only being warned about once.
#'
#'   The threshold can be moved with `options(card.cosinor.kappa = ...)`.
#'
#' @references Cornelissen G. Cosinor-based rhythmometry. *Theoretical Biology
#'   and Medical Modelling* 2014;11:16. \doi{10.1186/1742-4682-11-16}
#'
#' @seealso [cosinor()], [cosinor_methods]
#'
#' @name cosinor_identifiability
NULL

#' @title Validate Cosinor Periods
#' @description Checks `tau` where it is written, before any fitting happens.
#' @details Duplicated periods make \eqn{X'X} exactly singular; without this
#'   check the failure surfaces as a LAPACK message that names neither the
#'   argument nor the values responsible.
#' @param tau vector of periods as supplied by the user
#' @return nothing; called for its error
#' @noRd
validate_cosinor_tau <- function(tau) {
  if (!is.numeric(tau) || length(tau) < 1) {
    stop(
      "`tau` must be a numeric vector of one or more periods; it was given as ",
      "an object of class '",
      class(tau)[1],
      "' of length ",
      length(tau),
      ".",
      call. = FALSE
    )
  }
  if (any(!is.finite(tau))) {
    stop(
      "`tau` must be finite; it was given as ",
      paste(tau, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  if (any(tau <= 0)) {
    stop(
      "`tau` must be strictly positive; it was given as ",
      paste(tau, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  if (anyDuplicated(tau) > 0) {
    stop(
      "`tau` must hold distinct periods; it was given as ",
      paste(tau, collapse = ", "),
      ". Repeating a period makes the design matrix exactly singular.",
      call. = FALSE
    )
  }
  invisible(NULL)
}

#' @title Warn On A Poorly Conditioned Design
#' @description Warns once when the periods given cannot be separated by the
#'   data, naming the observed condition number.
#' @param kappa condition number of the design matrix
#' @param tau vector of periods
#' @param n_subjects number of subjects affected, for population models
#' @return nothing; called for its warning
#' @noRd
warn_cosinor_condition <- function(kappa, tau, n_subjects = NULL) {
  threshold <- getOption("card.cosinor.kappa", 30)
  if (is.na(kappa) || kappa <= threshold || length(tau) < 2) {
    return(invisible(NULL))
  }

  where <- if (is.null(n_subjects)) {
    "."
  } else {
    paste0(" for the median subject, and above ", threshold, " for ", n_subjects,
      " subjects.")
  }

  warning(
    "`tau` was given as ",
    paste(tau, collapse = ", "),
    ", whose components this data cannot separate: the design condition number ",
    "is ",
    format(kappa, digits = 4),
    where,
    " Amplitudes from a fit like this can be wrong by an order of magnitude ",
    "while looking ordinary. See `?cosinor_identifiability`.",
    call. = FALSE,
    immediate. = TRUE
  )
  invisible(NULL)
}

#' @title Design Matrix Condition Number
#' @description Condition number of the cosinor design matrix, used to detect
#'   periods that the data cannot separate.
#' @details Reported in preference to a resolution criterion based on the
#'   observation span. See [cosinor_identifiability] for why.
#' @param xmat a design matrix
#' @return a single numeric, or `NA_real_` if the matrix is degenerate
#' @noRd
cosinor_condition <- function(xmat) {
  out <- try(kappa(xmat, exact = TRUE), silent = TRUE)
  if (inherits(out, "try-error")) NA_real_ else out
}

#' @title Stored Covariance Parts
#' @description Returns the parts every cosinor statistic is built from,
#'   preferring the values stored at fit time and recomputing them from `xmat`
#'   when the object predates them.
#' @details Both paths take `n` from `nrow(xmat)`, so a model fitted by an older
#'   version of the package cannot disagree with a freshly fitted one.
#' @param object model of class `cosinor`
#' @return a list with `V` (covariance over the linear parameters), `sigma`,
#'   `nobs`, `df.residual` and `kappa`
#' @noRd
cosinor_vcov_parts <- function(object) {
  p <- length(object$tau)
  linearNames <- cosinor_par_names(p, type = "linear")
  parts <- object$parts

  if (object$type == "Population") {
    if (is.null(parts)) {
      k <- nrow(object$xmat)
      popNames <- c(
        "mesor",
        paste0(rep(c("beta", "gamma"), p), rep(seq_len(p), each = 2))
      )
      V <- stats::cov(
        object$xmat[, popNames, drop = FALSE],
        use = "complete.obs"
      ) /
        k
      dimnames(V) <- list(linearNames, linearNames)
      parts <- list(
        V = V,
        nobs = nrow(object$model),
        nsubjects = k,
        df.residual = k - 1,
        kappa = NA_real_
      )
    }
    return(list(
      V = parts$V,
      sigma = NA_real_,
      nobs = parts$nobs,
      nsubjects = parts$nsubjects,
      df.residual = parts$df.residual,
      kappa = parts$kappa
    ))
  }

  if (is.null(parts)) {
    nobs <- nrow(object$xmat)
    dfResidual <- nobs - (2 * p + 1)
    RSS <- sum(object$residuals[seq_len(nobs)]^2)
    parts <- list(
      XtXinv = solve(t(object$xmat) %*% object$xmat),
      RSS = RSS,
      sigma = sqrt(RSS / dfResidual),
      nobs = nobs,
      df.residual = dfResidual,
      kappa = cosinor_condition(object$xmat)
    )
  }

  V <- parts$sigma^2 * parts$XtXinv
  dimnames(V) <- list(linearNames, linearNames)

  list(
    V = V,
    sigma = parts$sigma,
    nobs = parts$nobs,
    nsubjects = NA_integer_,
    df.residual = parts$df.residual,
    kappa = parts$kappa
  )
}

#' @title Delta Method Jacobian
#' @description Jacobian of the map from the linear parameters to the amplitude
#'   and acrophase parameterisation, evaluated at the estimates.
#' @details Block diagonal: a one for the mesor, then a 2x2 block per component
#'
#'   \deqn{ \begin{pmatrix} \cos\phi & -\sin\phi \\ -\sin\phi / A & -\cos\phi / A
#'   \end{pmatrix} }
#'
#'   Deriving both rows here is what keeps the amplitude and acrophase cross
#'   terms from being written out separately and acquiring different signs.
#' @param object model of class `cosinor`
#' @return a `(2p+1) x (2p+1)` numeric matrix
#' @noRd
cosinor_jacobian <- function(object) {
  p <- length(object$tau)
  coefs <- stats::setNames(object$coefficients, object$coef_names)

  J <- diag(1, nrow = 2 * p + 1)
  dimnames(J) <- list(
    cosinor_par_names(p, "cosinor"),
    cosinor_par_names(p, "linear")
  )

  for (i in seq_len(p)) {
    phi <- coefs[[paste0("phi", i)]]
    amp <- coefs[[paste0("amp", i)]]
    at <- 2 * i

    J[at, at] <- cos(phi)
    J[at, at + 1] <- -sin(phi)
    J[at + 1, at] <- -sin(phi) / amp
    J[at + 1, at + 1] <- -cos(phi) / amp
  }

  J
}

#' @title Wald Test On A Subset Of Parameters
#' @description Tests `H0: theta = 0` for a subset of the linear parameters,
#'   using the covariance the object already carries.
#' @details For ordinary least squares this is algebraically identical to
#'   refitting without those columns and comparing residual sums of squares, so
#'   no refit is needed. Because the population covariance is the between-subject
#'   one divided by `k`, the same expression yields the Hotelling
#'   \eqn{T^{2}} population tests of Bingham et al. (1982).
#' @param object model of class `cosinor`
#' @param parm character vector of linear parameter names to test jointly
#' @return a list with `statistic`, `df1`, `df2` and `p.value`
#' @noRd
cosinor_wald <- function(object, parm) {
  parts <- cosinor_vcov_parts(object)
  coefs <- cosinor_coefficients(object, type = "linear")

  b <- coefs[parm]
  V <- parts$V[parm, parm, drop = FALSE]

  df1 <- length(parm)
  df2 <- parts$df.residual
  statistic <- drop(t(b) %*% solve(V) %*% b) / df1

  list(
    statistic = statistic,
    df1 = df1,
    df2 = df2,
    p.value = stats::pf(statistic, df1 = df1, df2 = df2, lower.tail = FALSE)
  )
}

#' @title Coefficients In A Given Parameterisation
#' @description Internal accessor returning a named coefficient vector in the
#'   interleaved order used by `cosinor_par_names()`.
#' @param object model of class `cosinor`
#' @param type either `"linear"` or `"cosinor"`
#' @return a named numeric vector of length `2 * p + 1`
#' @noRd
cosinor_coefficients <- function(object, type = c("cosinor", "linear")) {
  type <- match.arg(type)
  p <- length(object$tau)
  coefs <- stats::setNames(object$coefficients, object$coef_names)
  nms <- cosinor_par_names(p, type)
  stats::setNames(unname(coefs[nms]), nms)
}

## Extractor Methods ----

#' @title Extract Cosinor Model Components
#'
#' @description Standard extractor methods for a fitted [cosinor] model. Every
#'   statistic the package reports is built from [vcov.cosinor()], so these are
#'   the functions to reach for when writing a new one.
#'
#' @details The individual cosinor is ordinary least squares on the design
#'   matrix `[1, x_1, z_1, ..., x_p, z_p]`, so `coef()`, `vcov()`, `sigma()`,
#'   `df.residual()` and `logLik()` agree exactly with the equivalent [stats::lm]
#'   fit. That equivalence is asserted in the package's tests and is the
#'   cheapest available check on the fitting code.
#'
#'   # Parameterisations
#'
#'   `type = "linear"` returns the regression coefficients
#'   \eqn{M, \beta_j, \gamma_j} that least squares solves for; `type =
#'   "cosinor"` returns the mesor, amplitudes and acrophases
#'   \eqn{M, A_j, \phi_j} that are interpreted. The second is a nonlinear
#'   function of the first, and its covariance is obtained by the delta method.
#'
#'   # Population models
#'
#'   For a population-mean cosinor the random unit is the subject, so `vcov()`
#'   returns the between-subject covariance of the per-subject coefficients
#'   divided by the number of subjects, on \eqn{k - 1} degrees of freedom.
#'   `sigma()` and `logLik()` are not defined - a population fit never maximised
#'   a single likelihood, so returning an `AIC` for it would be a plausible
#'   wrong number rather than an answer.
#'
#' @param object model of class `cosinor`
#'
#' @param type parameterisation to return. `"cosinor"` gives the mesor,
#'   amplitudes and acrophases; `"linear"` gives the regression coefficients.
#'   `coef()` additionally accepts `"all"`, which returns every stored
#'   coefficient in the object's own order.
#'
#' @param ... not currently used, but required for extensibility
#'
#' @return `coef()` a named numeric vector; `vcov()` a named
#'   `(2p+1) x (2p+1)` matrix; `sigma()` a single numeric; `nobs()` and
#'   `df.residual()` a single integer; `logLik()` an object of class `logLik`.
#'
#' @references Cornelissen G. Cosinor-based rhythmometry. *Theoretical Biology
#'   and Medical Modelling* 2014;11:16. \doi{10.1186/1742-4682-11-16}
#'
#' @seealso [cosinor()], [confint.cosinor()], [anova.cosinor()]
#'
#' @name cosinor_methods
NULL

#' @rdname cosinor_methods
#' @export
coef.cosinor <- function(object, type = c("cosinor", "linear", "all"), ...) {
  type <- match.arg(type)
  if (type == "all") {
    return(stats::setNames(object$coefficients, object$coef_names))
  }
  cosinor_coefficients(object, type = type)
}

#' @rdname cosinor_methods
#' @export
vcov.cosinor <- function(object, type = c("linear", "cosinor"), ...) {
  type <- match.arg(type)
  V <- cosinor_vcov_parts(object)$V

  if (type == "linear") {
    return(V)
  }

  J <- cosinor_jacobian(object)
  out <- J %*% V %*% t(J)
  nms <- cosinor_par_names(length(object$tau), "cosinor")
  dimnames(out) <- list(nms, nms)
  out
}

#' @rdname cosinor_methods
#' @export
sigma.cosinor <- function(object, ...) {
  if (object$type == "Population") {
    stop(
      "`sigma()` is not defined for a population-mean cosinor, which pools ",
      "per-subject fits rather than estimating a single residual scale. Use ",
      "`vcov()` for the between-subject covariance.",
      call. = FALSE
    )
  }
  cosinor_vcov_parts(object)$sigma
}

#' @rdname cosinor_methods
#' @export
nobs.cosinor <- function(object, ...) {
  cosinor_vcov_parts(object)$nobs
}

#' @rdname cosinor_methods
#' @export
df.residual.cosinor <- function(object, ...) {
  cosinor_vcov_parts(object)$df.residual
}

#' @rdname cosinor_methods
#' @export
logLik.cosinor <- function(object, ...) {
  if (object$type == "Population") {
    stop(
      "`logLik()` is not defined for a population-mean cosinor, which pools ",
      "per-subject fits rather than maximising a single likelihood. An `AIC()` ",
      "derived from one would not mean what it appears to mean.",
      call. = FALSE
    )
  }

  parts <- cosinor_vcov_parts(object)
  n <- parts$nobs
  p <- length(object$tau)
  RSS <- parts$sigma^2 * parts$df.residual

  val <- -0.5 * n * (log(2 * pi) + 1 - log(n) + log(RSS))
  attr(val, "nobs") <- n
  attr(val, "df") <- 2 * p + 2
  class(val) <- "logLik"
  val
}

## Confidence Intervals ----

#' @title Confidence Intervals For Cosinor Parameters
#'
#' @description Confidence intervals for the mesor, amplitudes and acrophases of
#'   a fitted [cosinor] model, by the delta method or from the joint confidence
#'   ellipse.
#'
#' @details
#'
#'   # The delta method, and where it fails
#'
#'   The amplitude and acrophase are a nonlinear function of the regression
#'   coefficients, so `method = "delta"` propagates [vcov.cosinor()] through the
#'   Jacobian and forms a symmetric Wald interval. That is the conventional
#'   approach and it is accurate when the rhythm is well determined.
#'
#'   It degrades where the amplitude is small relative to its own standard
#'   error, which is exactly the situation of a weak higher harmonic. The
#'   amplitude is not differentiable at the origin and the acrophase gradient
#'   diverges as \eqn{1/A}, so a symmetric interval can extend below zero for a
#'   quantity that cannot be negative, or span more than a full circle. Such
#'   bounds are reported as given rather than silently truncated - a lower bound
#'   below zero is a signal that the delta method has stopped applying, and
#'   clamping it to zero would hide that.
#'
#'   # The confidence ellipse
#'
#'   Under Gaussian errors \eqn{(\hat\beta_j, \hat\gamma_j)} is exactly bivariate
#'   normal, so the exact joint region is an ellipse
#'
#'   \deqn{ (\hat\theta - \theta)' V_j^{-1} (\hat\theta - \theta) \le 2
#'   F_{1-\alpha}(2, \nu) }
#'
#'   and `method = "ellipse"` returns the conservative limits obtained by
#'   sweeping its boundary - the smallest and largest distance from the pole for
#'   the amplitude, and the tangent radii for the acrophase (Bingham et al. 1982;
#'   Cornelissen 2014). These respect the parameter space by construction.
#'
#'   When the ellipse covers the pole the no-rhythm null is not rejected and the
#'   acrophase is not identifiable at all. The amplitude then has a lower bound
#'   of zero and the acrophase bounds are returned as `NA` with a warning,
#'   because the honest answer is the whole circle rather than any interval.
#'
#' @param object model of class `cosinor`
#'
#' @param parm parameters to return intervals for, given as names or as
#'   positions within [coef()]. Defaults to all of them. An unrecognised name is
#'   an error rather than an `NA` row, so a misspelling cannot be mistaken for a
#'   genuinely unestimable parameter.
#'
#' @param level the confidence level required
#'
#' @param type parameterisation to return intervals in, `"cosinor"` for the
#'   mesor, amplitudes and acrophases or `"linear"` for the regression
#'   coefficients
#'
#' @param method `"delta"` for symmetric Wald intervals, or `"ellipse"` for the
#'   conservative limits derived from the joint confidence region. The ellipse is
#'   available only for individual models in the `"cosinor"` parameterisation.
#'
#' @param ... not currently used, but required for extensibility
#'
#' @return A matrix with one row per requested parameter and two columns giving
#'   the lower and upper bounds.
#'
#' @examples
#' data(twins)
#' model <- cosinor(rDYX ~ hour, twins, tau = c(24, 12))
#' confint(model)
#' confint(model, parm = "amp1", method = "ellipse")
#'
#' @references Bingham C, Arbogast B, Cornelissen G, Lee J, Halberg F.
#'   Inferential statistical methods for estimating and comparing cosinor
#'   parameters. *Chronobiologia* 1982;9(4):397-439.
#'
#'   Cornelissen G. Cosinor-based rhythmometry. *Theoretical Biology and Medical
#'   Modelling* 2014;11:16. \doi{10.1186/1742-4682-11-16}
#'
#' @seealso [cosinor_methods], [cosinor_area()], [anova.cosinor()]
#'
#' @export
confint.cosinor <- function(
  object,
  parm,
  level = 0.95,
  type = c("cosinor", "linear"),
  method = c("delta", "ellipse"),
  ...
) {
  type <- match.arg(type)
  method <- match.arg(method)

  if (!is.numeric(level) || length(level) != 1 || level <= 0 || level >= 1) {
    stop("`level` must be a single number strictly between 0 and 1.",
      call. = FALSE
    )
  }

  coefs <- cosinor_coefficients(object, type = type)
  pnames <- names(coefs)

  if (missing(parm)) {
    parm <- pnames
  } else if (is.numeric(parm)) {
    parm <- pnames[parm]
  }
  unknown <- setdiff(parm, pnames)
  if (length(unknown) > 0) {
    stop(
      "`parm` was given as ",
      paste(unknown, collapse = ", "),
      ", which this model does not carry; it holds ",
      paste(pnames, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  a <- 1 - level
  cnames <- paste(format(100 * c(a / 2, 1 - a / 2), trim = TRUE), "%")

  if (method == "ellipse") {
    return(cosinor_confint_ellipse(object, parm, level, type, cnames))
  }

  parts <- cosinor_vcov_parts(object)
  se <- sqrt(diag(vcov(object, type = type)))
  tdist <- stats::qt(1 - a / 2, df = parts$df.residual)

  ci <- cbind(
    coefs[parm] - tdist * se[parm],
    coefs[parm] + tdist * se[parm]
  )
  dimnames(ci) <- list(parm, cnames)
  ci
}

#' @title Conservative Intervals From The Confidence Ellipse
#' @description Sweeps the joint confidence region for each component's
#'   `(beta, gamma)` pair and returns the resulting amplitude and acrophase
#'   limits.
#' @param object model of class `cosinor`
#' @param parm character vector of parameter names, already validated
#' @param level the confidence level required
#' @param type parameterisation requested
#' @param cnames column names for the returned matrix
#' @return a matrix of confidence limits
#' @noRd
cosinor_confint_ellipse <- function(object, parm, level, type, cnames) {
  if (type != "cosinor") {
    stop(
      "`method = \"ellipse\"` applies to the amplitude and acrophase ",
      "parameterisation; use `type = \"cosinor\"`, or `method = \"delta\"` for ",
      "intervals on the regression coefficients.",
      call. = FALSE
    )
  }
  if (object$type == "Population") {
    stop(
      "`method = \"ellipse\"` is not available for a population-mean cosinor. ",
      "Use `method = \"delta\"`.",
      call. = FALSE
    )
  }

  parts <- cosinor_vcov_parts(object)
  co <- cosinor_coefficients(object, type = "linear")
  p <- length(object$tau)

  # The region is (bhat - b)' V^-1 (bhat - b) <= 2 * F(2, df). Boundary points
  # are centre + sqrt(crit) * R' u for R'R = V and |u| = 1, which is exact
  # rather than a grid search over the conic.
  crit <- 2 * stats::qf(level, df1 = 2, df2 = parts$df.residual)
  angles <- seq(0, 2 * pi, length.out = 2048)

  out <- matrix(NA_real_, nrow = 0, ncol = 2)
  covered <- character()

  for (i in seq_len(p)) {
    nms <- paste0(c("beta", "gamma"), i)
    centre <- co[nms]
    V <- parts$V[nms, nms, drop = FALSE]

    # Evaluating the same quadratic form at the pole is the component's own
    # zero-amplitude test, so the geometry and the F test cannot disagree
    coversPole <- drop(t(centre) %*% solve(V) %*% centre) <= crit

    R <- chol(V)
    boundary <- sqrt(crit) * cbind(cos(angles), sin(angles)) %*% R
    boundary[, 1] <- boundary[, 1] + centre[[1]]
    boundary[, 2] <- boundary[, 2] + centre[[2]]

    radius <- sqrt(boundary[, 1]^2 + boundary[, 2]^2)
    ampLimits <- if (coversPole) c(0, max(radius)) else range(radius)

    if (coversPole) {
      covered <- c(covered, paste0("amp", i))
      phiLimits <- c(NA_real_, NA_real_)
    } else {
      # Unwrap the boundary angles about the estimate before taking the range,
      # so a region straddling the branch cut is not reported as the whole circle
      ang <- atan2(-boundary[, 2], boundary[, 1])
      centreAng <- atan2(-centre[[2]], centre[[1]])
      d <- ((ang - centreAng + pi) %% (2 * pi)) - pi
      phiLimits <- cosinor_acrophase(centre[[1]], centre[[2]]) +
        c(min(d), max(d))
    }

    out <- rbind(out, ampLimits, phiLimits)
  }

  mesorSE <- sqrt(vcov(object, type = "cosinor")["mesor", "mesor"])
  tdist <- stats::qt(1 - (1 - level) / 2, df = parts$df.residual)
  out <- rbind(
    co[["mesor"]] + c(-1, 1) * tdist * mesorSE,
    out
  )
  rownames(out) <- cosinor_par_names(p, "cosinor")
  colnames(out) <- cnames

  if (length(covered) > 0) {
    warning(
      "The confidence region covers the pole for ",
      paste(covered, collapse = ", "),
      ", so the acrophase is not identifiable and its limits are returned as ",
      "NA. The rhythm is not distinguishable from zero amplitude at this level.",
      call. = FALSE
    )
  }

  out[parm, , drop = FALSE]
}

## Model Tests ----

#' @title Per-Component Tests For A Cosinor
#'
#' @description Tests each component of a [cosinor] model for a non-zero
#'   amplitude, or compares nested models fitted to the same data.
#'
#' @details With a single model, one row is returned per component testing
#'   \eqn{H_0: \beta_j = \gamma_j = 0} - whether that component carries a rhythm
#'   at all, on 2 degrees of freedom. This is the question a multiple-component
#'   model raises and the package previously had no way to answer: a user fitting
#'   `tau = c(24, 12)` could read off a 12-hour amplitude but not ask whether it
#'   was distinguishable from noise.
#'
#'   The test is a Wald statistic on the relevant block of [vcov.cosinor()],
#'   which for least squares is algebraically identical to refitting without
#'   those two columns and comparing residual sums of squares. No refit happens.
#'
#'   With several models it compares them in the order given, requiring that they
#'   were fitted to the same observations.
#'
#' @param object,... models of class `cosinor`
#'
#' @param test currently only `"F"`
#'
#' @return A `data.frame` of class `anova` with one row per component, or per
#'   model comparison.
#'
#' @examples
#' data(twins)
#' model <- cosinor(rDYX ~ hour, twins, tau = c(24, 12, 8))
#' anova(model)
#'
#' @seealso [cosinor_zero_amplitude()] for the joint test over all components
#'
#' @export
anova.cosinor <- function(object, ..., test = "F") {
  others <- list(...)

  if (length(others) > 0) {
    return(anova_cosinor_models(c(list(object), others)))
  }

  p <- length(object$tau)
  rows <- lapply(seq_len(p), function(i) {
    cosinor_wald(object, paste0(c("beta", "gamma"), i))
  })

  out <- data.frame(
    tau = object$tau,
    Df = vapply(rows, function(.x) .x$df1, numeric(1)),
    "Res.Df" = vapply(rows, function(.x) .x$df2, numeric(1)),
    "F value" = vapply(rows, function(.x) .x$statistic, numeric(1)),
    "Pr(>F)" = vapply(rows, function(.x) .x$p.value, numeric(1)),
    check.names = FALSE
  )
  rownames(out) <- paste0("component ", seq_len(p))

  structure(
    out,
    heading = c(
      "Cosinor Component Tests",
      paste0("Model: ", object$call),
      "Each row tests H0: beta = gamma = 0 for that component"
    ),
    class = c("anova", "data.frame")
  )
}

#' @title Compare Nested Cosinor Models
#' @description Sequential F tests across a list of `cosinor` models fitted to
#'   the same observations.
#' @param models a list of models of class `cosinor`
#' @return a `data.frame` of class `anova`
#' @noRd
anova_cosinor_models <- function(models) {
  if (!all(vapply(models, inherits, logical(1), "cosinor"))) {
    stop("All objects compared must be of class `cosinor`.", call. = FALSE)
  }
  if (any(vapply(models, function(.x) .x$type, character(1)) == "Population")) {
    stop(
      "Nested comparison is not defined for a population-mean cosinor, which ",
      "has no single residual sum of squares to partition.",
      call. = FALSE
    )
  }

  ns <- vapply(models, nobs, numeric(1))
  if (length(unique(ns)) != 1) {
    stop(
      "Models must be fitted to the same observations; they carry ",
      paste(ns, collapse = ", "),
      " rows respectively.",
      call. = FALSE
    )
  }

  RSS <- vapply(models, function(.x) {
    parts <- cosinor_vcov_parts(.x)
    parts$sigma^2 * parts$df.residual
  }, numeric(1))
  dfr <- vapply(models, df.residual, numeric(1))

  out <- data.frame(
    "Res.Df" = dfr,
    RSS = RSS,
    Df = c(NA, -diff(dfr)),
    "Sum of Sq" = c(NA, -diff(RSS)),
    check.names = FALSE
  )
  # Each comparison is scaled by the larger model's residual variance
  out[["F"]] <- c(
    NA,
    (out[["Sum of Sq"]][-1] / out[["Df"]][-1]) / (RSS[-1] / dfr[-1])
  )
  out[["Pr(>F)"]] <- stats::pf(
    out[["F"]],
    df1 = out[["Df"]],
    df2 = dfr,
    lower.tail = FALSE
  )
  rownames(out) <- vapply(
    models,
    function(.x) paste0("tau = ", paste(.x$tau, collapse = ", ")),
    character(1)
  )

  structure(
    out,
    heading = "Cosinor Model Comparison",
    class = c("anova", "data.frame")
  )
}

## Glance ----

#' @importFrom generics glance
#' @export
generics::glance

#' @title Glance At A Cosinor Model
#'
#' @description One row summarising a fitted [cosinor] model.
#'
#' @details `logLik`, `AIC` and `BIC` are `NA` for a population-mean cosinor,
#'   which pools per-subject fits rather than maximising a single likelihood.
#'
#'   `kappa` is the design condition number, reported here because a model whose
#'   periods the data cannot separate is otherwise indistinguishable from a good
#'   one in this table. See [cosinor_identifiability].
#'
#' @param x a `cosinor` object created by [card::cosinor()]
#' @param ... For extensibility
#'
#' @return a `tibble` with one row
#'
#' @examples
#' data(twins)
#' glance(cosinor(rDYX ~ hour, twins, tau = c(24, 12)))
#'
#' @export
glance.cosinor <- function(x, ...) {
  parts <- cosinor_vcov_parts(x)
  test <- cosinor_zero_amplitude(x)
  isPopulation <- x$type == "Population"

  y <- x$model[["y"]]
  ok <- stats::complete.cases(y, x$fitted.values)
  rsq <- 1 -
    sum((y[ok] - x$fitted.values[ok])^2) / sum((y[ok] - mean(y[ok]))^2)

  tibble::tibble(
    type = x$type,
    n_components = length(x$tau),
    r.squared = rsq,
    sigma = if (isPopulation) NA_real_ else parts$sigma,
    statistic = test$fstat,
    df1 = test$df1,
    df2 = test$df2,
    p.value = test$p.value,
    logLik = if (isPopulation) NA_real_ else as.numeric(stats::logLik(x)),
    AIC = if (isPopulation) NA_real_ else stats::AIC(x),
    BIC = if (isPopulation) NA_real_ else stats::BIC(x),
    nobs = parts$nobs,
    nsubjects = parts$nsubjects,
    kappa = parts$kappa
  )
}

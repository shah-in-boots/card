# Cosinor Harmonic Order ----

# Choosing how many components to fit is a model selection problem, and the
# cosinor literature mostly leaves it to judgement. This file provides the two
# standard answers - a sequential F test over nested harmonic models, and the
# information criteria - over a family that is nested by construction.
#
# Candidates are harmonics of the fundamental, tau_1 / 1 ... tau_1 / n. That
# restriction is deliberate:
#
#   1. Harmonic families are genuinely nested, so the F test is valid. An
#      arbitrary set of periods is not nested in any other, and comparing them
#      by F would be meaningless.
#   2. They stay well conditioned. Harmonics of a common fundamental are close
#      to orthogonal on any reasonable design, where two arbitrary periods can
#      be inseparable - see `cosinor_identifiability`.
#
# The condition number is reported per row for the same reason: an improvement
# in AIC at kappa = 400 is not evidence of anything.

#' @title Choose The Number Of Cosinor Components
#'
#' @description Fits a nested family of harmonic cosinor models and reports the
#'   evidence for each additional component, so the harmonic order can be chosen
#'   rather than assumed.
#'
#' @details Candidate models take periods \eqn{\tau_1, \tau_1/2, \ldots,
#'   \tau_1/n} where \eqn{\tau_1} is the longest period of `object`, so each
#'   model is nested in the next and the sequential F test is valid. The `tau`
#'   of `object` beyond its first element is ignored; only the fundamental is
#'   used.
#'
#'   Higher harmonics describe the *shape* of a single rhythm rather than
#'   separate rhythms. A model chosen this way is a truncated Fourier series, and
#'   the quantities to interpret from it are the composite ones from
#'   [cosinor_features()] - the global amplitude, orthophase and bathyphase -
#'   rather than the individual harmonic amplitudes.
#'
#'   # A caution on the intervals afterwards
#'
#'   Confidence intervals computed from the selected model, and reported as
#'   though the order had been fixed in advance, are anticonservative: they do
#'   not account for the selection having looked at the same data. This is a
#'   known gap in the cosinor literature rather than something this function
#'   solves. Where the order matters to a conclusion, fix it from prior knowledge
#'   instead.
#'
#' @param object Model of class `cosinor`, supplying both the data and the
#'   fundamental period
#'
#' @param max_order Largest number of harmonics to consider
#'
#' @param criterion Which criterion selects the returned order. `"bic"` and
#'   `"aic"` take the minimum; `"ftest"` takes the largest order whose addition
#'   is significant at `level`.
#'
#' @param level Confidence level for `criterion = "ftest"`
#'
#' @param ... Not currently used, but required for extensibility
#'
#' @return A `tibble` with one row per candidate order and columns `order`,
#'   `tau`, `npar`, `sigma`, `r.squared`, `logLik`, `AIC`, `BIC`, `statistic`,
#'   `df1`, `df2`, `p.value`, `kappa` and `selected`. The test columns compare
#'   each order against the one below it and are `NA` in the first row.
#'
#' @examples
#' data(twins)
#' model <- cosinor(rDYX ~ hour, twins, tau = 24)
#' cosinor_order(model, max_order = 3)
#'
#' @seealso [anova.cosinor()], [cosinor_features()], [cosinor_identifiability]
#'
#' @export
cosinor_order <- function(
  object,
  max_order = 4L,
  criterion = c("bic", "aic", "ftest"),
  level = 0.95,
  ...
) {
  criterion <- match.arg(criterion)

  if (!inherits(object, "cosinor")) {
    stop("`object` must be a model of class `cosinor`.", call. = FALSE)
  }
  if (object$type == "Population") {
    stop(
      "`cosinor_order()` is not defined for a population-mean cosinor, which ",
      "has no single likelihood or residual sum of squares to compare across ",
      "orders.",
      call. = FALSE
    )
  }
  if (
    !is.numeric(max_order) || length(max_order) != 1 || max_order < 1
  ) {
    stop(
      "`max_order` must be a single number of 1 or greater; it was given as ",
      paste(max_order, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  fundamental <- max(object$tau)
  d <- data.frame(t = object$model[["t"]], y = object$model[["y"]])

  fits <- lapply(seq_len(max_order), function(n) {
    suppressWarnings(cosinor(y ~ t, d, tau = fundamental / seq_len(n)))
  })

  RSS <- vapply(fits, function(.x) {
    parts <- cosinor_vcov_parts(.x)
    parts$sigma^2 * parts$df.residual
  }, numeric(1))
  dfr <- vapply(fits, stats::df.residual, numeric(1))
  TSS <- sum((d$y - mean(d$y))^2)

  # Each order against the one below it, scaled by the larger model's residual
  # variance, which is the usual nested F comparison
  statistic <- c(
    NA_real_,
    ((-diff(RSS)) / (-diff(dfr))) / (RSS[-1] / dfr[-1])
  )
  p.value <- stats::pf(
    statistic,
    df1 = c(NA_real_, -diff(dfr)),
    df2 = dfr,
    lower.tail = FALSE
  )

  out <- tibble::tibble(
    order = seq_len(max_order),
    tau = vapply(
      fits,
      function(.x) paste(format(.x$tau, digits = 4), collapse = ", "),
      character(1)
    ),
    npar = 2L * seq_len(max_order) + 1L,
    sigma = vapply(fits, stats::sigma, numeric(1)),
    r.squared = 1 - RSS / TSS,
    logLik = vapply(fits, function(.x) as.numeric(stats::logLik(.x)), numeric(1)),
    AIC = vapply(fits, stats::AIC, numeric(1)),
    BIC = vapply(fits, stats::BIC, numeric(1)),
    statistic = statistic,
    df1 = c(NA_real_, -diff(dfr)),
    df2 = dfr,
    p.value = p.value,
    kappa = vapply(
      fits,
      function(.x) cosinor_vcov_parts(.x)$kappa,
      numeric(1)
    )
  )

  # `selected` is a visible column rather than an attribute so that it survives
  # printing and subsetting
  out$selected <- switch(
    criterion,
    bic = out$BIC == min(out$BIC),
    aic = out$AIC == min(out$AIC),
    ftest = {
      significant <- !is.na(out$p.value) & out$p.value < (1 - level)
      chosen <- if (any(significant)) max(which(significant)) else 1L
      seq_len(max_order) == chosen
    }
  )

  out
}

# Fixtures ----

# A deliberately unbalanced design. The acrophase standard error differs from
# the correct one only through Cov(beta, gamma), which is zero whenever time is
# sampled evenly across whole cycles - so a balanced fixture cannot see the
# error, which is why the `twins` data has never exposed it.
unbalanced_cosinor_data <- function(n = 200, tau = 24, seed = 42) {
  set.seed(seed)
  t <- stats::runif(n, 0, 18)
  y <- 5 +
    2 * cos(2 * pi * t / tau) -
    1.2 * sin(2 * pi * t / tau) +
    stats::rnorm(n, sd = 1)
  data.frame(t = t, y = y)
}

# The individual cosinor is ordinary least squares on the same design matrix, so
# `lm()` is an exact oracle for every quantity the extractor methods return.
lm_equivalent <- function(object) {
  j <- seq_along(object$tau)
  f <- stats::formula(
    paste0("y ~ ", paste0("x", j, " + z", j, collapse = " + "))
  )
  stats::lm(f, data = object$model)
}

# Standard error of a scalar function of (beta, gamma) by numerical
# differentiation. Pins the delta-method algebra without adding a dependency.
finite_difference_se <- function(fun, beta, gamma, V, h = 1e-6) {
  g <- c(
    (fun(beta + h, gamma) - fun(beta - h, gamma)) / (2 * h),
    (fun(beta, gamma + h) - fun(beta, gamma - h)) / (2 * h)
  )
  sqrt(drop(t(g) %*% V %*% g))
}

# Extractor Oracles ----

test_that("the individual cosinor reproduces the equivalent lm fit", {
  d <- unbalanced_cosinor_data()
  object <- cosinor(y ~ t, d, tau = 24)
  fit <- lm_equivalent(object)

  # The linear parameterisation is what OLS actually solves for
  expect_equal(
    unname(coef(object, type = "linear")),
    unname(stats::coef(fit)),
    tolerance = 1e-10
  )
  expect_equal(
    unname(vcov(object, type = "linear")),
    unname(stats::vcov(fit)),
    tolerance = 1e-10
  )
  expect_equal(sigma(object), stats::sigma(fit), tolerance = 1e-10)
  expect_equal(df.residual(object), stats::df.residual(fit))
  expect_equal(nobs(object), stats::nobs(fit))
  expect_equal(
    as.numeric(logLik(object)),
    as.numeric(stats::logLik(fit)),
    tolerance = 1e-8
  )
  expect_equal(AIC(object), stats::AIC(fit), tolerance = 1e-8)
})

test_that("a multiple component cosinor reproduces the equivalent lm fit", {
  d <- unbalanced_cosinor_data(n = 400)
  object <- cosinor(y ~ t, d, tau = c(24, 12))
  fit <- lm_equivalent(object)

  expect_equal(
    unname(coef(object, type = "linear")),
    unname(stats::coef(fit)),
    tolerance = 1e-10
  )
  expect_equal(df.residual(object), stats::df.residual(fit))
})

test_that("VALIDATION the fit recovers a known mesor, amplitude and acrophase", {
  # beta = 2, gamma = -1.2 by construction, so A = sqrt(2^2 + 1.2^2)
  d <- unbalanced_cosinor_data(n = 4000)
  object <- cosinor(y ~ t, d, tau = 24)
  co <- coef(object)

  expect_equal(unname(co[["mesor"]]), 5, tolerance = 0.1)
  expect_equal(unname(co[["amp1"]]), sqrt(2^2 + 1.2^2), tolerance = 0.1)
  # phi = atan2(-gamma, beta) = atan2(1.2, 2), a positive angle folded onto
  # the package's [-2*pi, 0] convention
  expect_equal(
    unname(co[["phi1"]]),
    atan2(1.2, 2) - 2 * pi,
    tolerance = 0.05
  )
})

# Delta Method ----

test_that("VALIDATION amplitude and acrophase standard errors match a finite-difference delta method", {
  d <- unbalanced_cosinor_data()
  object <- cosinor(y ~ t, d, tau = 24)

  co <- coef(object, type = "linear")
  V <- vcov(object, type = "linear")[c("beta1", "gamma1"), c("beta1", "gamma1")]
  se <- sqrt(diag(vcov(object, type = "cosinor")))

  amplitude <- function(b, g) sqrt(b^2 + g^2)
  acrophase <- function(b, g) atan2(-g, b)

  expect_equal(
    unname(se[["amp1"]]),
    finite_difference_se(amplitude, co[["beta1"]], co[["gamma1"]], V),
    tolerance = 1e-5
  )
  expect_equal(
    unname(se[["phi1"]]),
    finite_difference_se(acrophase, co[["beta1"]], co[["gamma1"]], V),
    tolerance = 1e-5
  )
})

test_that("the acrophase standard error uses a positive cross term", {
  # Cornelissen (2014) gives -2*s23 for the amplitude and +2*s23 for the
  # acrophase. Asserting on a design where Cov(beta, gamma) is materially
  # non-zero, since the two formulas agree when it is zero.
  d <- unbalanced_cosinor_data()
  object <- cosinor(y ~ t, d, tau = 24)

  V <- vcov(object, type = "linear")
  expect_gt(abs(V["beta1", "gamma1"]) / sqrt(V["beta1", "beta1"] * V["gamma1", "gamma1"]), 0.2)

  s <- V / sigma(object)^2
  phi <- coef(object)[["phi1"]]
  amp <- coef(object)[["amp1"]]
  expected <- sigma(object) *
    sqrt(
      s["beta1", "beta1"] * sin(phi)^2 +
        2 * s["beta1", "gamma1"] * sin(phi) * cos(phi) +
        s["gamma1", "gamma1"] * cos(phi)^2
    ) /
    amp

  expect_equal(
    sqrt(diag(vcov(object, type = "cosinor")))[["phi1"]],
    expected,
    tolerance = 1e-10
  )
})

# confint Contract ----

test_that("confint returns a matrix with one row per parameter", {
  d <- unbalanced_cosinor_data()
  object <- cosinor(y ~ t, d, tau = c(24, 12))
  ci <- confint(object)

  expect_true(is.matrix(ci))
  expect_equal(nrow(ci), 5L)
  expect_equal(ncol(ci), 2L)
  expect_equal(rownames(ci), c("mesor", "amp1", "phi1", "amp2", "phi2"))
  # Lower bound below upper bound, for every parameter
  expect_true(all(ci[, 1] < ci[, 2]))
})

test_that("confint honours parm by name and by position", {
  d <- unbalanced_cosinor_data()
  object <- cosinor(y ~ t, d, tau = 24)

  expect_equal(rownames(confint(object, parm = "amp1")), "amp1")
  expect_equal(rownames(confint(object, parm = c("mesor", "phi1"))), c("mesor", "phi1"))
  expect_equal(rownames(confint(object, parm = 2)), "amp1")
})

test_that("confint refuses an unknown parm", {
  # Refusing beats `stats`' silent NA row: a misspelled parameter that returns
  # NA reads as a genuinely unestimable one.
  d <- unbalanced_cosinor_data()
  object <- cosinor(y ~ t, d, tau = 24)

  expect_error(confint(object, parm = "amplitude"), "amplitude")
})

test_that("confint level widens the interval", {
  d <- unbalanced_cosinor_data()
  object <- cosinor(y ~ t, d, tau = 24)

  narrow <- confint(object, level = 0.80)
  wide <- confint(object, level = 0.99)
  expect_true(all(diff(t(wide)) > diff(t(narrow))))
})

# vcov ----

test_that("vcov is symmetric and positive definite for both types", {
  d <- unbalanced_cosinor_data()
  scos <- cosinor(y ~ t, d, tau = 24)

  for (type in c("linear", "cosinor")) {
    V <- vcov(scos, type = type)
    expect_equal(V, t(V), tolerance = 1e-12)
    expect_true(all(eigen(V, only.values = TRUE)$values > 0))
  }
})

test_that("vcov carries the parameter names for its type", {
  d <- unbalanced_cosinor_data()
  object <- cosinor(y ~ t, d, tau = 24)

  expect_equal(
    colnames(vcov(object, type = "linear")),
    c("mesor", "beta1", "gamma1")
  )
  expect_equal(
    colnames(vcov(object, type = "cosinor")),
    c("mesor", "amp1", "phi1")
  )
})

# Ellipse intervals ----

test_that("the ellipse interval brackets the estimate and respects the parameter space", {
  d <- unbalanced_cosinor_data()
  object <- cosinor(y ~ t, d, tau = 24)

  ci <- confint(object, method = "ellipse")
  co <- coef(object)

  expect_true(is.matrix(ci))
  expect_equal(rownames(ci), c("mesor", "amp1", "phi1"))
  expect_true(all(ci[, 1] < co[rownames(ci)]))
  expect_true(all(ci[, 2] > co[rownames(ci)]))
  # An amplitude cannot be negative, and the ellipse cannot produce one
  expect_gte(ci["amp1", 1], 0)
})

test_that("VALIDATION the ellipse refuses an acrophase when the region covers the pole", {
  # Pure noise, so the rhythm is not distinguishable from zero amplitude. The
  # delta method still returns a finite acrophase interval here; the ellipse
  # reports that the acrophase is not identifiable at all, which is the honest
  # answer for a rhythm whose confidence region contains the origin.
  set.seed(3)
  d <- data.frame(t = runif(40, 0, 24), y = rnorm(40, mean = 5))
  object <- cosinor(y ~ t, d, tau = 24)

  expect_warning(ci <- confint(object, method = "ellipse"), "covers the pole")
  expect_equal(ci["amp1", 1], 0)
  expect_true(all(is.na(ci["phi1", ])))

  # The geometry and the F test must agree about whether a rhythm is present
  expect_gt(cosinor_zero_amplitude(object)$p.value, 0.05)
})

test_that("the ellipse interval is wider than the delta interval", {
  # It is a joint region projected onto each parameter, so it is conservative
  d <- unbalanced_cosinor_data()
  object <- cosinor(y ~ t, d, tau = 24)

  delta <- confint(object, parm = "amp1")
  ellipse <- confint(object, parm = "amp1", method = "ellipse")
  expect_gt(diff(ellipse[1, ]), diff(delta[1, ]))
})

test_that("the ellipse method refuses parameterisations it does not apply to", {
  d <- unbalanced_cosinor_data()
  object <- cosinor(y ~ t, d, tau = 24)
  expect_error(
    confint(object, type = "linear", method = "ellipse"),
    "cosinor"
  )

  pop <- suppressMessages(
    cosinor(rDYX ~ hour, twins, tau = 24, population = "patid")
  )
  expect_error(confint(pop, method = "ellipse"), "population")
})

# anova, glance and order ----

test_that("anova tests each component on 2 degrees of freedom", {
  data(twins)
  object <- cosinor(rDYX ~ hour, twins, tau = c(24, 12, 8))
  tbl <- anova(object)

  expect_s3_class(tbl, "anova")
  expect_equal(nrow(tbl), 3L)
  expect_true(all(tbl[["Df"]] == 2))
  # A component test must agree with refitting without it
  reference <- stats::anova(
    stats::lm(y ~ x1 + z1 + x2 + z2, data = object$model),
    stats::lm(y ~ x1 + z1 + x2 + z2 + x3 + z3, data = object$model)
  )
  expect_equal(tbl[["F value"]][3], reference[["F"]][2], tolerance = 1e-8)
})

test_that("glance reports one row and no likelihood for a population model", {
  data(twins)
  expect_equal(nrow(glance(cosinor(rDYX ~ hour, twins, tau = 24))), 1L)

  pop <- suppressMessages(
    cosinor(rDYX ~ hour, twins, tau = 24, population = "patid")
  )
  g <- glance(pop)
  expect_true(is.na(g$AIC))
  expect_equal(g$nsubjects, nrow(pop$xmat))
})

test_that("cosinor_order returns one row per order and selects exactly one", {
  data(twins)
  object <- cosinor(rDYX ~ hour, twins, tau = 24)
  tbl <- cosinor_order(object, max_order = 3)

  expect_equal(nrow(tbl), 3L)
  expect_equal(sum(tbl$selected), 1L)
  # Nested models cannot fit worse as terms are added
  expect_true(all(diff(tbl$r.squared) >= 0))
  expect_true(is.na(tbl$p.value[1]))
})

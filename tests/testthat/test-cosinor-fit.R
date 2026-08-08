# General fitting functions ----
test_that("models can be generally fit", {
  data(twins)
  scos <- cosinor(rDYX ~ hour, twins, tau = 24)
  mcos <- cosinor(rDYX ~ hour, twins, tau = c(24, 12))
  pcos <- cosinor(rDYX ~ hour, twins, tau = 24, population = "patid")

  # Appropriate classes
  expect_s3_class(scos, "cosinor")
  expect_s3_class(mcos, "cosinor")
  expect_s3_class(pcos, "cosinor")

  # Harmonic checks
  expect_gt(length(mcos$tau), 1)
  expect_equal(max(mcos$tau) %% min(mcos$tau), 0)
  expect_message(suppressWarnings(cosinor_features(mcos)), "harmonic")

  # Confidence intervals
  expect_true(is.matrix(confint(scos)))
})

test_that("population cosinors can be fit", {
  # Single population cosinor
  f <- sDYX ~ hour
  data <- twins
  population = "patid"
  m1 <- suppressMessages(
    cosinor(formula = f, data = data, tau = 24, population = population)
  )
  m2 <- suppressMessages(cosinor(
    formula = f,
    data = data,
    tau = c(24, 12, 8),
    population = population
  ))

  expect_s3_class(m1, "cosinor")
  expect_s3_class(m2, "cosinor")
  expect_equal(m1$type, "Population")
  # 2p + 1 free parameters, whatever the number of subjects
  expect_equal(dim(vcov(m1)), c(3L, 3L))
  expect_equal(dim(vcov(m2)), c(7L, 7L))
})

# Identifiability guards ----

test_that("cosinor refuses duplicated periods", {
  # Previously surfaced as a LAPACK "exactly singular" message naming nothing
  expect_error(cosinor(rDYX ~ hour, twins, tau = c(24, 24)), "distinct")
})

test_that("cosinor refuses a non-numeric or non-positive period", {
  expect_error(cosinor(rDYX ~ hour, twins, tau = "24"), "numeric")
  expect_error(cosinor(rDYX ~ hour, twins, tau = c(24, -12)), "positive")
  expect_error(cosinor(rDYX ~ hour, twins, tau = c(24, NA)), "finite")
})

test_that("cosinor refuses fewer observations than parameters", {
  d <- data.frame(hour = c(1, 5, 9), rDYX = c(2, 3, 2.5))
  expect_error(cosinor(rDYX ~ hour, d, tau = c(24, 12)), "observations")
})

test_that("cosinor warns when two periods are not separable", {
  # 24 against 23.5 returns amplitudes of ~7.4 and ~7.1 where the single
  # component amplitude is 0.30, because near-collinear components can grow
  # without bound so long as they cancel
  expect_warning(
    cosinor(rDYX ~ hour, twins, tau = c(24, 23.5)),
    "cannot separate"
  )
})

test_that("cosinor does not warn for harmonic periods on folded time indices", {
  # The guard against the over-eager guard. `twins$hour` is a clock hour, so its
  # span is 23 however many days were pooled; a resolution criterion based on
  # the observation span rejects this fit, which is the documented example and
  # is perfectly well conditioned.
  expect_no_warning(cosinor(rDYX ~ hour, twins, tau = c(24, 12)))
  expect_no_warning(cosinor(rDYX ~ hour, twins, tau = c(24, 12, 8)))
})

# Population alignment ----

test_that("population cosinor accepts character subject identifiers", {
  # Coercing the subject names with `as.numeric()` made every one of these NA,
  # so the insufficient-observations filter dropped nobody and the per-subject
  # fit then died on a singular matrix
  tw <- twins
  tw$patid <- paste0("S", tw$patid)
  m <- suppressMessages(cosinor(rDYX ~ hour, tw, tau = 24, population = "patid"))
  expect_s3_class(m, "cosinor")
})

test_that("population residuals are aligned with the input rows", {
  set.seed(1)
  shuffled <- twins[sample(nrow(twins)), ]
  m <- suppressMessages(
    cosinor(rDYX ~ hour, shuffled, tau = 24, population = "patid")
  )

  # Fitted values used to come back ordered by subject while the outcome stayed
  # in input order, which only looked right when the data arrived pre-sorted
  ok <- stats::complete.cases(m$model$y, m$fitted.values)
  expect_gt(stats::cor(m$fitted.values[ok], m$model$y[ok]), 0.5)
})

test_that("population standard errors scale as 1 over sqrt of the number of subjects", {
  m <- suppressMessages(
    cosinor(rDYX ~ hour, twins, tau = 24, population = "patid")
  )
  k <- nrow(m$xmat)

  betas <- m$xmat[, "beta1"]
  expect_equal(
    sqrt(vcov(m, type = "linear")["beta1", "beta1"]),
    sqrt(stats::var(betas) / k),
    tolerance = 1e-10
  )
})

test_that("VALIDATION the population amplitude interval excludes zero when a rhythm is present", {
  # The amplitude and acrophase variances were not divided by the number of
  # subjects while the mesor variance was, so this interval came back as
  # [-0.399, 0.975] across 741 subjects - implying no population rhythm at all
  m <- suppressMessages(
    cosinor(rDYX ~ hour, twins, tau = 24, population = "patid")
  )
  expect_gt(confint(m)["amp1", 1], 0)
})

# Zero amplitude test ----

test_that("the zero amplitude test uses 2p numerator degrees of freedom", {
  m1 <- cosinor(rDYX ~ hour, twins, tau = 24)
  m2 <- cosinor(rDYX ~ hour, twins, tau = c(24, 12))

  expect_equal(cosinor_zero_amplitude(m1)$df1, 2L)
  expect_equal(cosinor_zero_amplitude(m2)$df1, 4L)
  expect_equal(cosinor_zero_amplitude(m2)$df2, nobs(m2) - 5L)

  # Cross-checked against the equivalent nested lm comparison
  reference <- stats::anova(
    stats::lm(y ~ 1, data = m2$model),
    stats::lm(y ~ x1 + z1 + x2 + z2, data = m2$model)
  )
  expect_equal(
    cosinor_zero_amplitude(m2)$fstat,
    reference[["F"]][2],
    tolerance = 1e-8
  )
})


test_that("kfits dataframe is appropriate for population mean", {
  df <- as.data.frame(matrix(NA, 3120, 3)) # data frame for data
  names(df) <- c("time", "subject", "HR") # variable names
  t <- c(1:520) # time
  df[, 1] <- rep(1:520, 6) # six subjects
  df[, 2] <- rep(1:6, 520)[order(rep(1:6, 520))] # time for each subject
  set.seed(1) # seed for rnd

  # generates six different signals with some noise
  for (i in 1:6) {
    M <- rnorm(1, mean = 70, sd = 5)
    A <- rnorm(1, mean = 3, sd = 0.1)
    phi <- rnorm(1, mean = 60, sd = 10)
    e <- rnorm(c(1:520), mean = 0, sd = 5)
    hr.curve <- M + A * cos((2 * pi / 260) * t + phi) + e
    df[520 * (i - 1) + (1:520), 3] <- hr.curve
    #print(plot(t,hr.curve))
  }

  formula <- HR ~ time
  data <- df
  tau <- 260
  population <- "subject"
  # Was erroring in the past because of a sapply leading to a matrix
  m <- cosinor(
    formula = formula,
    data = data,
    tau = tau,
    population = population
  )
  expect_s3_class(m, "cosinor")
})

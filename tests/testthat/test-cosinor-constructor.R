# Parsnip Registration ----

test_that("cosinor_reg registers a complete parsnip model", {
  env <- parsnip::get_model_env()

  # The encoding table is the one that was missing. `parsnip:::form_form()`
  # slices it on every fit, and a NULL there fails inside vctrs rather than
  # anywhere that names the model, so the gap only ever surfaced at fit time.
  for (table in c("modes", "fit", "predict", "args", "encoding")) {
    expect_false(
      is.null(env[[paste0("cosinor_reg_", table)]]),
      label = paste0("cosinor_reg_", table)
    )
  }

  encoding <- parsnip::get_encoding("cosinor_reg")
  encoding <- encoding[encoding$engine == "card", ]
  expect_equal(nrow(encoding), 1L)
  # `cosinor()` moulds its own design matrix, so parsnip must leave the formula
  # alone and must not add an intercept the mesor already provides
  expect_false(encoding$compute_intercept)
  expect_false(encoding$remove_intercept)
})

test_that("registering the model twice does not duplicate its tables", {
  # `make_cosinor_reg()` runs every setter on each load so that an incomplete
  # registration repairs itself, which is only safe if the setters are
  # idempotent
  env <- parsnip::get_model_env()
  before <- nrow(env[["cosinor_reg_encoding"]])
  make_cosinor_reg()
  expect_equal(nrow(env[["cosinor_reg_encoding"]]), before)
})

test_that("a missing encoding is repaired on the next registration", {
  env <- parsnip::get_model_env()
  keep <- env[["cosinor_reg_encoding"]]
  on.exit(assign("cosinor_reg_encoding", keep, envir = env), add = TRUE)

  rm(list = "cosinor_reg_encoding", envir = env)
  make_cosinor_reg()
  expect_false(is.null(env[["cosinor_reg_encoding"]]))
})

# Fitting Through Parsnip ----

test_that("cosinor_reg creates appropriate parsnip model", {
  data(twins)
  cosinor_mod <-
    cosinor_reg(period = c(24, 8)) |>
    parsnip::set_engine("card") |>
    parsnip::set_mode("regression")

  cosinor_fit <-
    cosinor_mod |>
    parsnip::fit(rDYX ~ hour, data = twins)

  expect_s3_class(cosinor_mod, "cosinor_reg")
  expect_s3_class(cosinor_fit$fit, "cosinor")
})

test_that("VALIDATION fitting through parsnip agrees with calling cosinor directly", {
  data(twins)
  viaParsnip <-
    cosinor_reg(period = c(24, 12)) |>
    parsnip::set_engine("card") |>
    parsnip::fit(rDYX ~ hour, data = twins)
  direct <- cosinor(rDYX ~ hour, twins, tau = c(24, 12))

  expect_equal(coef(viaParsnip$fit), coef(direct))
  expect_equal(vcov(viaParsnip$fit), vcov(direct))
})

test_that("the period argument reaches tau", {
  data(twins)
  for (period in list(24, c(24, 12), c(24, 12, 8))) {
    fitted <-
      cosinor_reg(period = period) |>
      parsnip::set_engine("card") |>
      parsnip::fit(rDYX ~ hour, data = twins)
    expect_equal(fitted$fit$tau, period)
  }
})

test_that("prediction through parsnip matches the underlying model", {
  data(twins)
  newData <- data.frame(hour = 0:23)
  fitted <-
    cosinor_reg(period = c(24, 12)) |>
    parsnip::set_engine("card") |>
    parsnip::fit(rDYX ~ hour, data = twins)

  out <- predict(fitted, new_data = newData)
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), nrow(newData))
  expect_equal(
    out$.pred,
    predict(
      cosinor(rDYX ~ hour, twins, tau = c(24, 12)),
      new_data = newData
    )$.pred
  )
})

test_that("update replaces the period on an existing specification", {
  data(twins)
  spec <- cosinor_reg(period = 24) |> parsnip::set_engine("card")
  updated <- update(spec, period = c(24, 8))

  expect_equal(rlang::eval_tidy(updated$args$period), c(24, 8))
  expect_equal(
    parsnip::fit(updated, rDYX ~ hour, data = twins)$fit$tau,
    c(24, 8)
  )
})

test_that("cosinor_reg refuses a mode it cannot support", {
  expect_error(cosinor_reg(mode = "classification"), "regression")
})

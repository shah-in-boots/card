# Cosinor Regression ----

#' @title Fit a `cosinor`
#'
#' @description `cosinor()` fits a regression model of a time variable to a
#'   continuous outcome use trigonometric features. This approaches uses the
#'   linearization of the parameters to assess their statistics and
#'   distribution.
#'
#' @param t Represents the _ordered_ time indices that provide the positions for
#'   the cosine wave. Depending on the context:
#'
#'   - A `data frame` of a time-based predictor/index.
#'
#'   - A `matrix` of time-based predictor/index.
#'
#'   - A `recipe` specifying a set of preprocessing steps
#'     created from [recipes::recipe()].
#'
#' @param y When __t__ is a `data frame` or `matrix`, __y__ is the outcome
#' specified as:
#'
#'   - A `data frame` with 1 numeric column.
#'
#'   - A `matrix` with 1 numeric column.
#'
#'   - A numeric `vector`.
#'
#' @param data When a `recipe` or `formula` is used, __data__ is specified as:
#'
#'   - A `data frame` containing both the predictors and the outcome.
#'
#' @param formula A formula specifying the outcome terms on the left-hand side,
#' and the predictor terms on the right-hand side.
#'
#' @param tau A vector that determines the periodicity of the time index. The
#'   number of elements in the vector determine the number of components (e.g.
#'   single versus multiple cosinor).
#'
#'   - A `vector` with a single element = single-component cosinor, e.g.
#'   period = c(24)
#'   - A `vector` with multiple elements = multiple-component
#'   cosinor, e.g. period = c(24, 12)
#'
#' @param population Represents the population to be analyzed with a
#'   population-mean cosinor. Defaults to NULL, assuming individual cosinors are
#'   being generated. When a `recipe` or `formula` is used, __population__ is
#'   specified as:
#'
#'   - A `character` name of the column contained in __data__ that contains
#'   identifiers for each subject. Every row will have a subject name which
#'   should be duplicated for each time index given.
#'
#'   When a `data frame` or `matrix` is used, __population__ is specified as:
#'
#'   - A `vector` of the same length as __t__, with values representing each
#'   subject at the correct indices.
#'
#' @param ... Not currently used, but required for extensibility.
#'
#' @return A `cosinor` object.
#'
#' @examples
#' # Data setup
#' data("twins")
#'
#' # Formula interface
#' model <- cosinor(rDYX ~ hour, twins, tau = 24)
#'
#' @family cosinor
#' @export
cosinor <- function(t, ...) {
  UseMethod("cosinor")
}

# Cosinor Methods ----

## Default method

#' @export
#' @rdname cosinor
cosinor.default <- function(t, ...) {
  stop("`cosinor()` is not defined for a '", class(t)[1], "'.", call. = FALSE)
}

## XY method - data frame

#' @export
#' @rdname cosinor
cosinor.data.frame <- function(t, y, tau, population = NULL, ...) {
  processed <- hardhat::mold(t, y)
  cosinor_bridge(processed, tau, population, data = NULL, ...)
}

## XY method - matrix

#' @export
#' @rdname cosinor
cosinor.matrix <- function(t, y, tau, population = NULL, ...) {
  processed <- hardhat::mold(t, y)
  cosinor_bridge(processed, tau, population, data = NULL, ...)
}

## Formula method - stable, works

#' @export
#' @rdname cosinor
cosinor.formula <- function(formula, data, tau, population = NULL, ...) {
  processed <- hardhat::mold(formula, data)
  if (is.character(population)) {
    population <- data[[population]]
  }
  cosinor_bridge(processed, tau, population, data, ...)
}

## Recipe method - unstable

#' @export
#' @rdname cosinor
cosinor.recipe <- function(t, data, tau, population = NULL, ...) {
  processed <- hardhat::mold(t, data)
  if (is.character(population)) {
    population <- data[[population]]
  }
  cosinor_bridge(processed, tau, population, data, ...)
}

# Cosinor Construction ----

## Bridging Function

#' @description Bridging function takes user-facing call, after it is processed,
#'   and moves it to `cosinor_bridge()`, which then calls both `cosinor_impl()`, the
#'   fitting algorithm, and `new_cosinor()`, the constructor for a new type of S3
#'   class. This also bridges to population-mean cosinor implementation if
#'   needed.
#' @noRd
cosinor_bridge <- function(processed, tau, population, data, ...) {
  # Validated here, where the argument is written, so a bad period is reported
  # against `tau` rather than surfacing later as a singular matrix
  validate_cosinor_tau(tau)

  ### Create call ----
  # Formal equation
  # y(t) = M + A*cos(2*pi*t/period + phi)
  # y(t) = M + beta*x + gamma*z + error(t)
  y <- names(processed$outcomes)
  t <- names(processed$predictors)
  l <- length(tau)
  ls <- list()

  for (i in 1:l) {
    ls[[i]] <- paste0("A", i, " * cos(2*pi*", t, "/", tau[i], " + phi", i, ")")
  }
  f <- paste0(y, " ~ M + ", paste0(ls, collapse = " + "))
  call <- paste0("cosinor(formula = ", f)

  ### Model fit ----

  # Check and format predictors
  hardhat::validate_predictors_are_numeric(processed$predictors)
  predictors <- processed$predictors[[1]]

  # Check and format outcomes
  hardhat::validate_outcomes_are_univariate(processed$outcomes)
  hardhat::validate_outcomes_are_numeric(processed$outcomes)
  outcomes <- processed$outcomes[[1]]

  # If population value is NULL, then perform individual cosinor
  if (is.null(population)) {
    # Implemented function for single and multiple component cosinor
    fit <- cosinor_impl(predictors, outcomes, tau)
    type <- "Individual"
  } else if (length(population) == length(predictors)) {
    # Modified function, using `cosinor_impl()` internally
    fit <- cosinor_pop_impl(predictors, outcomes, tau, population)
    type <- "Population"
  } else {
    # Error if population cosinor cannot be run either
    stop(
      "Population-mean cosinor error: `population` does not match size of time indices",
      call. = FALSE
    )
  }

  ## New Cosinor ----

  # Raised once here rather than once per subject inside the population loop.
  # For a population fit the median subject is what is tested; see
  # `cosinor_pop_impl()` for why.
  warn_cosinor_condition(
    fit$parts$kappa,
    tau,
    n_subjects = if (type == "Population") {
      fit$parts$nsubjects_ill_conditioned
    } else {
      NULL
    }
  )

  # Constructor function receives from implemented function
  new_cosinor(
    coefficients = fit$coefficients,
    coef_names = fit$coef_names,
    fitted.values = fit$fitted.values,
    residuals = fit$residuals,
    call = call, # Made with bridge data
    tau = tau,
    model = fit$model,
    xmat = fit$xmat,
    parts = fit$parts, # Covariance parts, computed once at fit time
    type = type, # Made at bridge, labels type of cosinor object
    blueprint = processed$blueprint # Made from hardhat, not from fit
  )
}

## Constructor function

#' @description Accepts the output from the fit of the implemented model. Makes a new S3 class with the correct structure. Checks output from model for coherence.
#' @noRd
new_cosinor <- function(
  coefficients,
  coef_names,
  fitted.values,
  residuals,
  call,
  tau,
  model,
  xmat,
  parts,
  type,
  blueprint
) {
  # Can validate coefs here
  if (!is.numeric(coefficients)) {
    stop("`coefficients` should be a numeric vector.", call. = FALSE)
  }

  # Names check
  if (!is.character(coef_names)) {
    stop("`coef_names` should be a character vector.", call. = FALSE)
  }

  # Length check
  if (length(coefficients) != length(coef_names)) {
    stop("`coefficients` and `coef_names` must have same length.")
  }

  # Covariance parts check
  if (!is.list(parts)) {
    stop("`parts` should be a list of covariance components.", call. = FALSE)
  }

  # Fit outputs need to match here
  hardhat::new_model(
    coefficients = coefficients,
    coef_names = coef_names,
    fitted.values = fitted.values,
    residuals = residuals,
    call = call,
    tau = tau,
    model = model,
    xmat = xmat,
    parts = parts,
    type = type,
    blueprint = blueprint,
    class = "cosinor"
  )
}

# Cosinor Parsnip Methods ----

# Wrapper function to load parsnip model
#
# Only `set_new_model()` is guarded. Every other setter is idempotent - calling
# them twice leaves the registration tables at one row apiece - so they run on
# every load. Guarding the whole block, as this used to, meant a registration
# that was incomplete for any reason could never repair itself: `cosinor_reg`
# was already in the model environment, so a reload skipped the fix along with
# everything else. That is how the missing encoding survived several releases.
make_cosinor_reg <- function() {
  # Check to see if already loaded
  current <- parsnip::get_model_env()

  # Start making new model
  if (!any(current$models == "cosinor_reg")) {
    parsnip::set_new_model("cosinor_reg")
  }

  # Add parsnip models to another package
  parsnip::set_model_mode(model = "cosinor_reg", mode = "regression")
  parsnip::set_model_engine("cosinor_reg", mode = "regression", eng = "card")
  parsnip::set_dependency("cosinor_reg", eng = "card", pkg = "card")

  # Arguments
  parsnip::set_model_arg(
    model = "cosinor_reg",
    eng = "card",
    parsnip = "period",
    original = "tau",
    func = list(pkg = "card", fun = "cosinor"),
    has_submodel = FALSE
  )

  # Fit
  parsnip::set_fit(
    model = "cosinor_reg",
    eng = "card",
    mode = "regression",
    value = list(
      interface = "formula",
      protect = c("formula", "data"),
      func = c(pkg = "card", fun = "cosinor"),
      defaults = list()
    )
  )

  # Encoding
  #
  # Required, not optional: `parsnip:::form_form()` reads this table on every
  # fit and slices it with `vctrs::vec_slice()`. Without it `get_encoding()`
  # returns NULL rather than raising an error, so the fallback that would have
  # supplied defaults never fires and the slice fails on a NULL. Registering a
  # model without an encoding therefore produces a specification that builds and
  # prints correctly and errors only when something is fitted with it.
  #
  # `cosinor()` builds its own design matrix of cosine and sine terms through
  # `hardhat::mold()`, taking the time index as a raw numeric column, so parsnip
  # must not preprocess the formula first. The mesor is the intercept and is
  # added internally.
  parsnip::set_encoding(
    model = "cosinor_reg",
    eng = "card",
    mode = "regression",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  # Prediction
  parsnip::set_pred(
    model = "cosinor_reg",
    eng = "card",
    mode = "regression",
    type = "numeric",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args = list(
        object = quote(object$fit),
        new_data = quote(new_data),
        type = "numeric"
      )
    )
  )
}


#' @title General Interface for Cosinor Regression Models
#' @description `cosinor_reg()` is a _parsnip_ friendly method for specification of cosinor regression model before fitting.
#' @param mode A character string that describes the type of model. In this case, it only supports type of "regression".
#' @param period A non-negative number or vector of numbers that represent the expected periodicity of the data to be analyzed.
#' @examples
#' data(twins)
#'
#' # The example fits deliberately rather than stopping at the specification.
#' # A specification builds and prints correctly even when the engine is
#' # registered incompletely, so only a fit exercises the registration.
#' cosinor_reg(period = c(24, 8)) |>
#'   parsnip::set_engine("card") |>
#'   parsnip::set_mode("regression") |>
#'   parsnip::fit(rDYX ~ hour, data = twins)
#' @export
cosinor_reg <- function(mode = "regression", period = NULL) {
  # Check correct mode
  if (mode != "regression") {
    stop("`mode` should be 'regression'", call. = FALSE)
  }

  # Capture arguments
  args <- list(period = rlang::enquo(period))

  # Model specs / slots
  parsnip::new_model_spec(
    "cosinor_reg",
    args = args,
    mode = mode,
    eng_args = NULL,
    method = NULL,
    engine = NULL
  )
}

#' @param object Cosinor model specification
#' @param ... Not used for `update()`
#' @param fresh A logical for whether the arguments should be modified in place or replaced altogether
#' @method update cosinor_reg
#' @rdname cosinor_reg
#' @export
update.cosinor_reg <- function(object, period = NULL, fresh = FALSE, ...) {
  parsnip::update_dot_check(...)

  # Updated arguments
  args <- list(
    period = rlang::enquo(period)
  )

  if (fresh) {
    object$args <- args
  } else {
    null_args <- purrr::map_lgl(args, parsnip::null_value)
    if (any(null_args)) {
      args <- args[!null_args]
    }
    if (length(args) > 0) {
      object$args[names(args)] <- args
    }
  }

  # Model specs / slots
  parsnip::new_model_spec(
    "cosinor_reg",
    args = object$args,
    eng_args = object$eng_args,
    mode = object$mode,
    method = NULL,
    engine = object$engine
  )
}

#' @method print cosinor_reg
#' @rdname cosinor_reg
#' @param x Cosinor model specification
#' @param ... Extensible
#' @export
print.cosinor_reg <- function(x, ...) {
  cat("Cosinor Model Specification (", x$mode, ")\n\n", sep = "")
  parsnip::model_printer(x, ...)

  if (!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(parsnip::show_call(x))
  }

  invisible(x)
}

# Cosinor Generic S3 Methods ----

## Print Method

#' @description Generic print method
#' @param x Model of class `cosinor`
#' @param ... arguments to pass on
#' @noRd
#' @export
print.cosinor <- function(x, ...) {
  cat("Call: \n")
  cat(x$call, "\n")

  # Coefficients
  cat("\n")
  cat("Coefficients: \n")
  names(x$coefficients) <- x$coef_names
  print(x$coefficients)
}

## Summary Method

#' @description Generic summary method
#' @param object Model of class `cosinor`
#' @param ... arguments to pass on
#' @noRd
#' @export
summary.cosinor <- function(object, ...) {
  # Summary
  cat(paste0(object$type, " Cosinor Model \n"))
  cat(strrep("-", 42))

  # Call
  cat("\n")
  cat("Call: \n")
  cat(object$call, "\n")

  # Periods
  cat("\n")
  cat("Period(s): ")
  cat(paste0(object$tau, collapse = ", "), "\n")

  # Residuals
  cat("\n")
  cat("Residuals: \n")
  print(summary(object$residuals))

  # Coefficients (estimate, SE, t.value, P.value)
  cat("\n")
  cat("Coefficients: \n")
  coefs <- stats::coef(object)
  mat <- cbind(
    "Estimate" = coefs,
    "Std. Error" = sqrt(diag(stats::vcov(object, type = "cosinor")))[
      names(coefs)
    ]
  )
  print(mat)

  # Rhythm detection
  test <- cosinor_zero_amplitude(object)
  cat("\n")
  cat(sprintf(
    "Zero-amplitude test: F = %.3f on %d and %d DF, p-value = %s\n",
    test$fstat,
    test$df1,
    test$df2,
    format.pval(test$p.value, digits = 4, eps = .Machine$double.eps)
  ))

  # Conditioning is surfaced on every look at the object rather than only in a
  # warning at fit time, because periods the data cannot separate produce
  # amplitudes that are wrong by an order of magnitude while looking ordinary.
  kappa <- cosinor_vcov_parts(object)$kappa
  if (!is.na(kappa)) {
    cat(sprintf("Design condition number: %.1f", kappa))
    if (kappa > getOption("card.cosinor.kappa", 30)) {
      cat(" -- periods are poorly separated, see ?cosinor_identifiability")
    }
    cat("\n")
  }

  invisible(object)
}

#' @description Generic plot method
#' @param x `cosinor` object
#' @param ... For extensibility
#' @noRd
#' @export
plot.cosinor <- function(x, ...) {
  # Model data
  model <- as.data.frame(x$model)
  model$yhat <- x$fitted.values
  model$res <- x$residuals

  # Plotting function
  plot(model$t, model$yhat)
}

# Cosinor Tidiers ----

## Tidy Method

#' @importFrom generics tidy
#' @export
generics::tidy

#' Tidy a(n) cosinor object
#' @description Tidy summarizes information about the components of a `cosinor`
#'   model.
#'
#' @details The standard error is obtained by the delta method from the
#'   covariance of the regression coefficients, since the amplitude and acrophase
#'   are a nonlinear function of them. See [confint.cosinor()] for where that
#'   approximation stops applying.
#'
#'   The `statistic` column reports each component's F test against zero
#'   amplitude, on 2 and \eqn{N - 2p - 1} degrees of freedom. It is a
#'   per-component test rather than a per-parameter one, so the amplitude and
#'   acrophase of a given component share a statistic and a p-value: the null
#'   being tested is \eqn{\beta_j = \gamma_j = 0}, and an acrophase has no
#'   meaning under it. The mesor is tested separately by a t statistic.
#'
#' @param x A `cosinor` object created by [card::cosinor()]
#'
#' @param conf.int Logical indicating whether or not to include confidence
#'
#'   interval in tidied output
#' @param conf.level The confidence level to use if `conf.int = TRUE`. Must be
#'
#'   between 0 and 1, with default to 0.95 (the 95% confidence interval).
#' @param ... For extensibility
#' @return a `tibble` object
#'
#' @export
tidy.cosinor <- function(x, conf.int = FALSE, conf.level = 0.95, ...) {
  p <- length(x$tau)
  coefs <- stats::coef(x)
  se <- sqrt(diag(stats::vcov(x, type = "cosinor")))[names(coefs)]

  # The mesor is a single linear parameter, so a t statistic; each component is
  # a joint 2 degree of freedom test shared by its amplitude and acrophase
  mesorTest <- cosinor_wald(x, "mesor")
  statistic <- c(mesor = unname(coefs[["mesor"]] / se[["mesor"]]))
  p.value <- c(mesor = mesorTest$p.value)

  for (i in seq_len(p)) {
    test <- cosinor_wald(x, paste0(c("beta", "gamma"), i))
    statistic <- c(statistic, rep(test$statistic, 2))
    p.value <- c(p.value, rep(test$p.value, 2))
  }
  names(statistic) <- names(p.value) <- names(coefs)

  result <-
    tibble::tibble(
      term = names(coefs),
      estimate = unname(coefs),
      std.error = unname(se[names(coefs)]),
      statistic = unname(statistic[names(coefs)]),
      p.value = unname(p.value[names(coefs)])
    )

  if (conf.int) {
    ci <- stats::confint(x, level = conf.level)
    colnames(ci) <- c("conf.low", "conf.high")
    result <-
      ci |>
      dplyr::as_tibble(rownames = "term") |>
      dplyr::left_join(x = result, y = _, by = "term")
  }

  # Return findings
  result
}

## Augment Method

#' @importFrom generics augment
#' @export
generics::augment

#' @title Augment data with information from a `cosinor` object
#' @description Augment accepts a `cosinor` model object and adds information about each observation in the dataset. This includes the predicted values in the `.fitted` column and the residuals in the `.resid` column. New columns always begin with a `.` prefix to avoid overwriting columns in original dataset.
#' @param x A `cosinor` object created by [card::cosinor()]
#' @param ... For extensibility
#' @return a `tibble` object
#' @export
#' @family cosinor
augment.cosinor <- function(x, ...) {
  # Add fitted and residual values
  result <-
    dplyr::bind_cols(
      dplyr::tibble(x$model),
      dplyr::tibble(.fitted = x$fitted.values),
      dplyr::tibble(.resid = x$residuals)
    )

  # Return
  return(result)
}

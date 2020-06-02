# Cosinor Regression {{{ ====

#' @title Fit a `cosinor`
#'
#' @description `cosinor()` fits a regression model of a time variable to a
#' continuous outcome use trigonometric features.
#'
#' @param t Represents the _ordered_ time indices that provide the positions for the
#'   cosine wave. Depending on the context:
#'
#'   - A __data frame__ of a time-based predictor/index.
#'   - A __matrix__ of time-based predictor/index.
#'   - A __recipe__ specifying a set of preprocessing steps
#'     created from [recipes::recipe()].
#'
#' @param y When `t` is a __data frame__ or __matrix__, `y` is the outcome
#' specified as:
#'
#'   - A __data frame__ with 1 numeric column.
#'   - A __matrix__ with 1 numeric column.
#'   - A numeric __vector__.
#'
#' @param data When a __recipe__ or __formula__ is used, `data` is specified as:
#'
#'   - A __data frame__ containing both the predictors and the outcome.
#'
#' @param formula A formula specifying the outcome terms on the left-hand side,
#' and the predictor terms on the right-hand side.
#'
#' @param tau A vector that determines the periodicity of the time index. The
#'   number of elements in the vector determine the number of components (e.g.
#'   single versus multiple cosinor).
#'
#'   - A __vector__ with a single element = single-component cosinor, e.g.
#'   period = c(24)
#'   - A __vector__ with multiple elements = multiple-component
#'   cosinor, e.g. period = c(24, 12)
#'
#' @param population Represents the population to be analyzed with a
#'   population-mean cosinor. Defaults to NULL, assuming individual cosinors are
#'   being generated. When a __recipe__ or __formula__ is used, `population` is
#'   specified as:
#'
#'   - A __character__ name of the column contained in `data` that contains
#'   identifiers for each subject. Every row will have a subject name which
#'   should be duplicated for each time index given.
#'
#'   When a __data frame__ or __matrix__ is used, `population` is specified as:
#'
#'   - A __vector__ of the same length as `t`, with values representing each
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
#' @export
cosinor <- function(t, ...) {
  UseMethod("cosinor")
}

# Methods {{{ ====

#' @export
#' @rdname cosinor
cosinor.default <- function(t, ...) {
  stop("`cosinor()` is not defined for a '", class(t)[1], "'.", call. = FALSE)
}

# XY method - data frame

#' @export
#' @rdname cosinor
cosinor.data.frame <- function(t, y, tau, population = NULL, ...) {
  processed <- hardhat::mold(t, y)
  cosinor_bridge(processed, tau, population, ...)
}

# XY method - matrix

#' @export
#' @rdname cosinor
cosinor.matrix <- function(t, y, tau, population = NULL, ...) {
  processed <- hardhat::mold(t, y)
  cosinor_bridge(processed, tau, population, ...)
}

# Formula method - stable, works

#' @export
#' @rdname cosinor
cosinor.formula <- function(formula, data, tau, population = NULL, ...) {
  processed <- hardhat::mold(formula, data)
  if(is.character(population)) {
    population <- data[[population]]
  }
  cosinor_bridge(processed, tau, population, ...)
}

# Recipe method - unstable

#' @export
#' @rdname cosinor
cosinor.recipe <- function(t, data, tau, population = NULL, ...) {
  processed <- hardhat::mold(t, data)
  if(is.character(population)) {
    population <- data[[population]]
  }
  cosinor_bridge(processed, tau, population, ...)
}

# }}}

# Bridge {{{ ====

#' @description Bridging function takes user-facing call, after it is processed,
#'   and moves it to `cosinor_bridge`, which then calls both `cosinor_impl`, the
#'   fitting algorithm, and `new_cosinor`, the constructor for a new type of S3
#'   class. This also bridges to population-mean cosinor implementation if
#'   needed.
#' @noRd
cosinor_bridge <- function(processed, tau, population, ...) {

  # Make call
  call <- call(
    "cosinor",
    paste0(names(processed$outcomes),
           " ~ ",
           names(processed$predictors))
  )

  # Check and format predictors
  hardhat::validate_predictors_are_numeric(processed$predictors)
  predictors <- processed$predictors[[1]]

  # Check and format outcomes
  hardhat::validate_outcomes_are_univariate(processed$outcomes)
  hardhat::validate_outcomes_are_numeric(processed$outcomes)
  outcomes <- processed$outcomes[[1]]

  # If population value is NULL, then perform individual cosinor
  if(is.null(population)) {

    # Implemented function
    fit <- cosinor_impl(predictors, outcomes, tau)
    type <- "Individual"

  } else if(length(population) == length(predictors)) {

    # Modified function, usings cosinor_impl internally
    fit <- cosinor_pop_impl(predictors, outcomes, tau, population)
    type <- "Population"

  } else {

    # Error if population cosinor cannot be run either
    stop("Population-mean cosinor error: `population` does not match size of time indices", call. = FALSE)

  }

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
    type = type, # Made at bridge, labels type of cosinor object
    blueprint = processed$blueprint # Made from hardhat, not from fit
  )
}

# }}}

# Constructor function {{{ ====

#' @description Accepts the output from the fit of the implemented model. Makes a new S3 class with the correct structure.
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
  type,
  blueprint
) {

  # Can validate coefs here
  if (!is.numeric(coefficients)) {
    stop("`coefficients` should be a numeric vector.",
      call. = FALSE
    )
  }

  # Names check
  if (!is.character(coef_names)) {
    stop("`coef_names` should be a character vector.",
      call. = FALSE
    )
  }

  # Length check
  if (length(coefficients) != length(coef_names)) {
    stop("`coefficients` and `coef_names` must have same length.")
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
    type = type,
    blueprint = blueprint,
    class = "cosinor"
  )
}


# }}}
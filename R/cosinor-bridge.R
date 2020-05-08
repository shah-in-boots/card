# Cosinor Regression {{{ ====

#' Fit a `cosinor`
#'
#' `cosinor()` fits a regression model of a time variable to a continuous outcome use trigonometric features.
#'
#' @param x Depending on the context:
#'
#'   * A __data frame__ of predictors.
#'   * A __matrix__ of predictors.
#'   * A __recipe__ specifying a set of preprocessing steps
#'     created from [recipes::recipe()].
#'
#' @param y When `x` is a __data frame__ or __matrix__, `y` is the outcome
#' specified as:
#'
#'   * A __data frame__ with 1 numeric column.
#'   * A __matrix__ with 1 numeric column.
#'   * A numeric __vector__.
#'
#' @param data When a __recipe__ or __formula__ is used, `data` is specified as:
#'
#'   * A __data frame__ containing both the predictors and the outcome.
#'
#' @param formula A formula specifying the outcome terms on the left-hand side,
#' and the predictor terms on the right-hand side.
#'
#' @param ... Not currently used, but required for extensibility.
#'
#' @return
#'
#' A `cosinor` object.
#'
#' @examples
#'
#' # Data setup
#' data("twins")
#' predictors <- twins["hour"]
#' outcome <- twins["rDYX"]
#'
#' # XY interface
#' mod1 <- cosinor(predictors, outcome)
#'
#' # Formula interface
#' mod2 <- cosinor(rDYX ~ hour, twins)
#'
#' # Recipes interface
#' library(recipes)
#' rec <- recipe(rDYX ~ hour, twins)
#' rec <- step_log(rec, rDYX)
#' mod3 <- cosinor(rec, mtcars)
#'
#' @export
cosinor <- function(x, ...) {
  UseMethod("cosinor")
}

# Methods {{{ ====

#' @export
#' @rdname cosinor
cosinor.default <- function(x, ...) {
  stop("`cosinor()` is not defined for a '", class(x)[1], "'.", call. = FALSE)
}

# XY method - data frame

#' @export
#' @rdname cosinor
cosinor.data.frame <- function(x, y, ...) {
  processed <- hardhat::mold(x, y)
  cosinor_bridge(processed, ...)
}

# XY method - matrix

#' @export
#' @rdname cosinor
cosinor.matrix <- function(x, y, ...) {
  processed <- hardhat::mold(x, y)
  cosinor_bridge(processed, ...)
}

# Formula method - stable, works

#' @export
#' @rdname cosinor
cosinor.formula <- function(formula, data, ...) {
  processed <- hardhat::mold(formula, data)
  cosinor_bridge(processed, ...)
}

# Recipe method

#' @export
#' @rdname cosinor
cosinor.recipe <- function(x, data, ...) {
  processed <- hardhat::mold(x, data)
  cosinor_bridge(processed, ...)
}

# }}}

# Bridge {{{ ====

cosinor_bridge <- function(processed, ...) {

  # Make sure input is appropriate format
  hardhat::validate_outcomes_are_univariate(processed$outcomes)
  hardhat::validate_outcomes_are_numeric(processed$outcomes)
  hardhat::validate_predictors_are_numeric(processed$predictors)

  # Outcomes need correct format, predictors shaped in implementation
  predictors <- processed$predictors[[1]]
  outcomes <- processed$outcomes[[1]]

  # Implempented function requires
  fit <- cosinor_impl(predictors, outcomes)

  # Constructor function recieves from implemented function (fit$*)
  new_cosinor(
    coefs = fit$coefs,
    coef_names = fit$coef_names,
    ellipse = fit$ellipse,
    amp = fit$amp,
    phi = fit$phi,
    blueprint = processed$blueprint # Made from hardhat, not from fit
  )
}

# }}}
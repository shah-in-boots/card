# Cosinor Regression {{{ ====

#' Fit a `cosinor`
#'
#' `cosinor()` fits a regression model of a time variable to a continuous outcome use trigonometric features.
#'
#' @param t Depending on the context:
#'
#'   * A __data frame__ of time-based predictors/indices.
#'   * A __matrix__ of time-based predictors/indices.
#'   * A __recipe__ specifying a set of preprocessing steps
#'     created from [recipes::recipe()].
#'
#' @param y When `t` is a __data frame__ or __matrix__, `y` is the outcome
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
#'
#' # XY interface
#' # mod1 <- cosinor(twins["hour"], twins$rDYX)
#'
#' # Formula interface
#' mod2 <- cosinor(rDYX ~ hour, twins)
#'
#' # Recipes interface (still needs work)
#' # rec <- recipes::recipe(rDYX ~ hour, twins)
#' # rec <- recipes::step_log(rec, rDYX)
#' # mod3 <- cosinor(rec, twins)
#'
#' @export
cosinor <- function(x, ...) {
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
cosinor.data.frame <- function(t, y, ...) {
  processed <- hardhat::mold(t, y)
  cosinor_bridge(processed, ...)
}

# XY method - matrix

#' @export
#' @rdname cosinor
cosinor.matrix <- function(t, y, ...) {
  processed <- hardhat::mold(t, y)
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
cosinor.recipe <- function(t, data, ...) {
  processed <- hardhat::mold(t, data)
  cosinor_bridge(processed, ...)
}

# }}}

# Bridge {{{ ====

#' @description Bridging function takes user-facing call, after it is processed,
#'   and moves it to `cosinor_bridge`, which then calls both `cosinor_impl`, the
#'   fitting algorithm, and `new_cosinor`, the constructor for a new type of S3
#'   class.
#' @noRd
cosinor_bridge <- function(processed, ...) {

  # Make sure input is appropriate format
  hardhat::validate_outcomes_are_univariate(processed$outcomes)
  hardhat::validate_outcomes_are_numeric(processed$outcomes)

  # Outcomes and predictors converted to vectors to process in fit
  predictors <- processed$predictors
  outcomes <- processed$outcomes

  # Implempented function requires
  fit <- cosinor_impl(predictors, outcomes)

  # Constructor function recieves from implemented function
  new_cosinor(
    coefficients = fit$coefficients,
    coef_names = fit$coef_names,
    mesor = fit$mesor,
    beta = fit$beta,
    gamma = fit$gamma,
    amp = fit$amp,
    phi = fit$phi,
    fitted.values = fit$fitted.values,
    residuals = fit$residuals,
    formula = fit$formula,
    area = fit$area,
    blueprint = processed$blueprint # Made from hardhat, not from fit
  )
}

# }}}

# Constructor function {{{ ====

#' @description Accepts the output from the fit of the implemented model
#' @noRd
new_cosinor <- function(
                        coefficients,
                        coef_names,
                        mesor = mesor,
                        beta = beta,
                        gamma = gamma,
                        amp = amp,
                        phi = phi,
                        fitted.values,
                        residuals,
                        formula,
                        area,
                        blueprint) {

  # Can validate coefs here
  if (!is.numeric(coefficients)) {
    stop("`coef` should be a numeric vector.",
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
    stop("`coefs` and `coef_names` must have same length.")
  }

  # Fit outputs need to match here
  hardhat::new_model(
    coefficients = coefficients,
    coef_names = coef_names,
    mesor = mesor,
    beta = beta,
    gamma = gamma,
    amp = amp,
    phi = phi,
    fitted.values = fitted.values,
    residuals = residuals,
    formula = formula,
    area = area,
    blueprint = blueprint,
    class = "cosinor"
  )
}


# }}}
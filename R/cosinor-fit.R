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

# Formula method

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

  # Regression parameters
  predictors <- processed$predictors
  outcome <- processed$outcomes[[1]]

  # Make sure outcome is numeric
  hardhat::validate_outcomes_are_univariate(outcome)
  hardhat::validate_outcomes_are_numeric(outcome)

  # Place actual function here
  fit <- cosinor_impl(predictors, outcome)

  # Receives from implemented function (fit$*)
  new_cosinor(
    coefs = fit$coefs,
    blueprint = processed$blueprint
  )
}

# }}}

# Implementation {{{ ====

cosinor_impl <- function(predictors, outcome) {

    # Preprocessing
    hardhat::validate_predictors_are_numeric(predictors)

    # Period of 24 hours in a day
    period <- 24

    # Time variable
    tvar <- predictors[[1]]

    cos((2 * pi * tvar) / period)

    cosinor_fit <- lm.fit(predictors, outcomes)
}

twins$cosh <- cos((2*pi*twins$hour)/period)
twins$sinh <- sin((2*pi*twins$hour)/period)

m <- lm(rDYX ~ hour, data = twins)
n <- lm(rDYX ~ cosh, data = twins)
o <- lm(rDYX ~ sinh, data = twins)
p <- lm(rDYX ~ cosh + sinh, data = twins)
ggplot(twins, aes(x = hour, y = rDYX)) +
    geom_point(alpha = 0.2, colour = "grey") +
    geom_line(aes(y = m$fitted.values), colour = "black") +
    geom_line(aes(y = n$fitted.values), colour = "blue") +
    geom_line(aes(y = o$fitted.values), colour = "red") +
    geom_line(aes(y = p$fitted.values), colour = "purple") +
    theme_minimal() +
    labs(x = "Hours",
         y = "DYX",
         title = "Model building: Y = B1*cos(hr) + B2*sin(hr) + intercept",
         caption = "Black = linear, Red = sin, Blue = cos, Purple = sin+cos"
    ) +
    ylim(2.5,3.3)


ggplot(m, aes(x = hour, y = rDYX)) +
    geom_smooth(method = "lm", se = TRUE)

# }}}

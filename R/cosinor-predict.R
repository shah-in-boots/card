# Predict {{{ ====

#' Predict from a `cosinor`
#'
#' @param object A `cosinor` object.
#'
#' @param new_data A data frame or matrix of new predictors.
#'
#' @param type A single character. The type of predictions to generate.
#' Valid options are:
#'
#' - `"numeric"` for numeric predictions.
#'
#' @param ... Not used, but required for extensibility.
#'
#' @return
#'
#' A tibble of predictions. The number of rows in the tibble is guaranteed
#' to be the same as the number of rows in `new_data`.
#'
#' @examples
#' train <- mtcars[1:20, ]
#' test <- mtcars[21:32, -1]
#'
#' # Fit
#' mod <- cosinor(mpg ~ cyl + log(drat), train)
#'
#' # Predict, with preprocessing
#' predict(mod, test)
#' @export
predict.cosinor <- function(object, new_data, type = "numeric", ...) {
  forged <- hardhat::forge(new_data, object$blueprint)
  rlang::arg_match(type, valid_predict_types())
  predict_cosinor_bridge(type, object, forged$predictors)
}

# Can adjust prediction types (which are generated from the implementation)
valid_predict_types <- function() {
  c("numeric")
}

# }}}

# Bridge {{{ ====

predict_cosinor_bridge <- function(type, model, predictors) {

  # Only needed if is a matrix
  predictors <- as.matrix(predictors)

  # Get_predict below needs to have matching type
  predict_function <- get_predict_function(type)
  predictions <- predict_function(model, predictors)

  hardhat::validate_prediction_size(predictions, predictors)

  predictions
}

get_predict_function <- function(type) {

  # Make sure that the prediction classes match up to model type
  switch(
    type,
    numeric = predict_cosinor_numeric
  )
}

# }}}

# Implementation {{{ ====

# Numeric prediction
predict_cosinor_numeric <- function(model, predictors) {
  predictions <- rep(1L, times = nrow(predictors))
  hardhat::spruce_numeric(predictions)
}

# }}}

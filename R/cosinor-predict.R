# Predict ----

#' Predict from a `cosinor`
#'
#' @param object A `cosinor` object.
#'
#' @param new_data A data frame or matrix of new predictors.
#'
#' @param type A single character. The type of predictions to generate. Valid
#'   options are:
#'
#' - `"numeric"` for numeric predictions.
#'
#' @param ... Additional arguments passed to the prediction function
#'
#' @return A tibble of predictions. The number of rows in the tibble is
#' guaranteed to be the same as the number of rows in `new_data`.
#'
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

# Bridge ----

predict_cosinor_bridge <- function(type, object, predictors) {

  # Get_predict below needs to have matching type
  predict_function <- get_predict_function(type)
  predictions <- predict_function(object, predictors)

  hardhat::validate_prediction_size(predictions, predictors)

  # Return
  return(predictions)

}

get_predict_function <- function(type) {

  # Make sure that the prediction classes match up to model type
  switch(
    type,
    numeric = predict_cosinor_numeric
  )
}

# Implementation ----

# Numeric prediction
predict_cosinor_numeric <- function(object, predictors) {
  predictions <- rep(1L, times = nrow(predictors))
  hardhat::spruce_numeric(predictions)

  # Basic coefs
  coefs <- object$coefficients
  names(coefs) <- object$coef_names

  # Cosinor specific remodeling of coefs
  tau <- object$tau
  p <- length(tau)
  t <- predictors
  mesor <- coefs[1]

  # Assign to environemntal variables the values of coefficients
  for (i in 1:p) {
    assign(paste0("amp", i), unname(coefs[paste0("amp", i)]))
    assign(paste0("phi", i), unname(coefs[paste0("phi", i)]))
  }

  # Make prediction based on parameters/coefficients
  # y(t) = M + amp1 * cos(2*pi*t/tau1 + phi1) + amp2 * cos(2*pi*t/tau2 + phi2)
  pars <- list()
  for (i in 1:p) {
		pars[[i]] <- get(paste0("amp", i)) * cos(2*pi*t / tau[i] + get(paste0("phi", i)))
  }
  df <- data.frame(mesor = mesor, trig = matrix(unlist(pars), ncol = length(pars), byrow = FALSE))
  pred <- rowSums(df)

  # Reformat and return
  out <- hardhat::spruce_numeric(pred)
  return(out)
}

# Cosinor Regression ----

#' @title Fit a `cosinor`
#'
#' @description `cosinor()` fits a regression model of a time variable to a
#'   continuous outcome use trigonometric features. This approaches uses the
#'   linearization of the parameters to assess their statistics and
#'   distribution.
#'
#' @param t Represents the _ordered_ time indices that provide the positions for the
#'   cosine wave. Depending on the context:
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

  ### Create call ----
    # Formal equation
    # y(t) = M + A*cos(2*pi*t/period + phi)
    # y(t) = M + beta*x + gamma*z + error(t)
  y <- names(processed$outcomes)
  t <- names(processed$predictors)
  l <- length(tau)
  ls <- list()

  for (i in 1:l) {
    ls[[i]] <- paste0("A", i, " * cos(2*pi*", t, "/", tau[i], " + phi", i,")")
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

    # Modified function, usings cosinor_impl internally
    fit <- cosinor_pop_impl(predictors, outcomes, tau, population)
    type <- "Population"

  } else {

    # Error if population cosinor cannot be run either
    stop("Population-mean cosinor error: `population` does not match size of time indices", call. = FALSE)

  }

  ## New Cosinor ----

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

# Cosinor Parsnip Methods ----

# Wrapper function to load parsnip model
make_cosinor_reg <- function() {

	# Check to see if already loaded
	current <- parsnip::get_model_env()

	# If not loaded, then set up model
	if(!any(current$models == "cosinor_reg")) {

		# Start making new model
		parsnip::set_new_model("cosinor_reg")

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

}


#' @title General Interface for Cosinor Regression Models
#' @description `cosinor_reg()` is a _parsnip_ friendly method for specification of cosinor regression model before fitting.
#' @param mode A character string that describes the type of model. In this case, it only supports type of "regression".
#' @param period A non-negative number or vector of numbers that represent the expected periodicity of the data to be analyzed.
#' @examples
#' library(parsnip)
#' data(twins)
#' cosinor_reg(period = 24) |>
#'   set_engine("card") |>
#'   fit(rDYX ~ hour, data = twins)
#' @export
cosinor_reg <- function(mode = "regression", period = NULL) {

	# Check correct mode
	if(mode != "regression") {
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
		if (any(null_args))
			args <- args[!null_args]
		if (length(args) > 0)
			object$args[names(args)] <- args
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
	names(object$coefficients) <- object$coef_names
	coefs <- object$coefficients
  coefs <- coefs[grep("mesor|amp|phi", names(coefs))]
  se <- stats::confint(object)$se
	l <- list(coefs = coefs, se = se)

	mat <- do.call(cbind, lapply(l, function(x) {x[match(names(l[[1]]), names(x))]}))
	colnames(mat) <- c("Estimate", "Std. Error")
	print(mat)

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

#' @title Tidy a(n) cosinor object
#' @description Tidy summarizes information about the components of a `cosinor`
#'   model.
#' @details `cosinor` objects do not necessarily have a T-statistic as the
#'   standard error is not based on a mean value, but form a joint-confidence
#'   interval. The standard error is generated using Taylor series expansion as
#'   the object is a subspecies of harmonic regressions.
#' @param x A `cosinor` object created by [card::cosinor()]
#' @param conf.int Logical indicating whether or not to include confidence
#'   interval in tidied output
#' @param conf.level The confidence level to use if `conf.int = TRUE`. Must be
#'   between 0 and 1, with default to 0.95 (the 95% confidence interval).
#' @param ... For extensibility
#' @return a `tibble` object
#' @export
tidy.cosinor <- function(x, conf.int = FALSE, conf.level = 0.95, ...) {

  # Get base data
  names(x$coefficients) <- x$coef_names
	coefs <- x$coefficients
  coefs <- coefs[grep("mesor|amp|phi", names(coefs))]
  val <- stats::confint(x, level = conf.level)
	l <- list(coefs = coefs, se = val$se)
	mat <- do.call(cbind, lapply(l, function(x) {x[match(names(l[[1]]), names(x))]}))

	# Tibble it
	result <-
	  mat |>
	  dplyr::as_tibble(rownames = "term") |>
	  dplyr::rename(estimate = coefs, std.error = se)

	if (conf.int) {

		ci <- val$ci
		colnames(ci) <- c("conf.low", "conf.high")
		result <-
			ci |>
	    dplyr::as_tibble(rownames = "term") |>
	    dplyr::left_join(result, ., by = "term")

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
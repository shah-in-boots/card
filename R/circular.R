# Tidy Methods ====
#' @title Tidy a(n) circular object
#' @description Tidy summarizes information about the components of a `circular`
#'   model.
#' @details `circular` object are made using the [circular::lm.circular()] function
#' @param x A `circular` regression object
#' @param conf.int Logical indicating whether or not to include confidence
#'   interval in tidied output
#' @param conf.level The confidence level to use if `conf.int = TRUE`. Must be
#'   between 0 and 1, with default to 0.95 (the 95% confidence interval).
#' @param ... For extensibility
#' @return a `tibble` object
#' @export
tidy.circular <- function(x, conf.int = FALSE, conf.level = 0.95, ...) {

  # Get base data
  names(x$coefficients) <- colnames(x$x)
	coefs <- x$coefficients
	se <- x$se.coef
	l <- list(coefs = coefs, se = se)
	mat <- do.call(cbind, lapply(l, function(x) {x[match(names(l[[1]]), names(x))]}))

	# Tibble it
	result <-
	  mat %>%
	  dplyr::as_tibble(rownames = "term") %>%
	  dplyr::rename(estimate = coefs, std.error = se) %>%
		tibble::add_column(
			statistic = x$t.values,
			p.value = x$p.values
		)

	# Add confidence intervals if needed
	if(conf.int) {
	  tdist <- stats::qt(1 - (1 - conf.level)/2, df = nrow(x$x) - 2)
	  result <-
	  	result %>%
	  	tibble::add_column(
	  		conf.low = coefs - tdist * se,
	  		conf.high = coefs + tdist * se
	  	)
	}

	# Return findings
	return(result)

}

# Parsnip Methods ====

make_circular_reg <- function() {

	# Check to see if already loaded
	current <- parsnip::get_model_env()

	# If not loaded, then set up model
	if(!any(current$models == "circular_reg")) {

		# Start making new model
		parsnip::set_new_model("circular_reg")

		# Add parsnip models to another package
		parsnip::set_model_mode(model = "circular_reg", mode = "regression")
		parsnip::set_model_engine("circular_reg", mode = "regression", eng = "circular")
		parsnip::set_dependency("circular_reg", eng = "circular", pkg = "circular")

		# Arguments = type
		parsnip::set_model_arg(
			model = "circular_reg",
			eng = "circular",
			parsnip = "pattern",
			original = "type",
			func = list(pkg = "circular", fun = "lm.circular"),
			has_submodel = FALSE
		)

		# Arguments = init
		parsnip::set_model_arg(
			model = "circular_reg",
			eng = "circular",
			parsnip = "initial",
			original = "init",
			func = list(pkg = "circular", fun = "lm.circular"),
			has_submodel = FALSE
		)

		# Arguments = tol
		parsnip::set_model_arg(
			model = "circular_reg",
			eng = "circular",
			parsnip = "tolerance",
			original = "tol",
			func = list(pkg = "circular", fun = "lm.circular"),
			has_submodel = FALSE
		)

		# Encoding
		parsnip::set_encoding(
			model = "circular_reg",
			eng = "circular",
			mode = "regression",
			options = list(
				predictor_indicators = "traditional",
				compute_intercept = TRUE,
				remove_intercept = FALSE,
				allow_sparse_x = TRUE
			)
		)

		# Fit
		parsnip::set_fit(
			model = "circular_reg",
			eng = "circular",
			mode = "regression",
			value = list(
				interface = "matrix",
				protect = c("x", "y"),
				func = c(pkg = "circular", fun = "lm.circular"),
				defaults = list(verbose = TRUE)
			)
		)

		# Prediction
		parsnip::set_pred(
			model = "circular_reg",
			eng = "circular",
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


#' @title General Interface for Circular Regression Models
#' @description `circular_reg()` is a _parsnip_ friendly method for
#'   specification of circular regression model before fitting. When using the
#'   [circular::lm.circular] engine, a formula is not given/needed.
#' @param mode A character string that describes the type of model. In this
#'   case, it only supports type of "regression", which is default.
#' @param pattern A character string of either "c-c" or "c-l" which show the
#'   relationship between the dependent and independent variables, identifying
#'   if they are circular or not. This changes the additional parameters
#'   available.
#'
#'   - If __"c-c"__ is selected, neither __initial__ or __tolerance__ are
#'   required
#'
#'   - If __"c-l"__ is selected, both __initial__ or __tolerance__ are required
#'
#' @param initial A vector with initial values the length equal to the columns
#'   of the independent variable, plus the intercept. For example, with 3
#'   predictors (and one intercept), the initial value should be `x = rep(0, 4)`
#' @param tolerance A numerical value, which defaults to 1e-10, can be set at a
#'   lower or higher tolerance which sets the accuracy for algorithm
#'   convergence.
#' @examples
#' f <- az_svg ~ lab_hba1c + cad
#' df <- geh[c("az_svg", "lab_hba1c", "cad")]
#' df$az_svg <- circular::circular(df$az_svg, units = "degrees")
#' circular_reg(pattern = "c-l", initial = rep(0, 3), tolerance = 1e-3) %>%
#'   set_engine("circular") %>%
#'   fit(f, data = df)
#' @export
circular_reg <- function(mode = "regression", pattern = NULL, initial = NULL, tolerance = NULL) {

	# Check correct mode
	if(mode != "regression") {
		stop("`mode` should be 'regression'", call. = FALSE)
	}

	# Capture arguments
	args <- list(
		pattern = rlang::enquo(pattern),
		initial = rlang::enquo(initial),
		tolerance = rlang::enquo(tolerance)
	)

	# Model specs / slots
	parsnip::new_model_spec(
		"circular_reg",
		args = args,
		mode = mode,
		eng_args = NULL,
		method = NULL,
		engine = NULL
	)
}

#' @param object Circular model specification
#' @param ... Not used for `update()`
#' @param fresh A logical for whether the arguments should be modified in place or replaced altogether
#' @method update circular_reg
#' @rdname circular_reg
#' @export
update.circular_reg <- function(object, pattern = NULL, initial = NULL, tolerance = NULL, fresh = FALSE, ...) {
	parsnip::update_dot_check(...)

	# Updated arguments
	args <- list(
		pattern = rlang::enquo(pattern),
		initial = rlang::enquo(initial),
		tolerance = rlang::enquo(tolerance)
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
		"circular_reg",
		args = object$args,
		eng_args = object$eng_args,
		mode = object$mode,
		method = NULL,
		engine = object$engine
	)
}

#' @method print circular_reg
#' @rdname circular_reg
#' @param object circular model specification
#' @param ... Extensible
#' @export
print.circular_reg <- function(object, ...) {
	cat("Circular Model Specification (", object$mode, ")\n\n", sep = "")
	parsnip::model_printer(object, ...)

	if (!is.null(object$method$fit$args)) {
		cat("Model fit template:\n")
		print(parsnip::show_call(object))
	}

	invisible(object)
}

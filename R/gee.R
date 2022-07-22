# Parsnip Methods ====

# nocov start

#' @description Parsnip methods that are called by the `.onLoad()` function to create the `gee` addition to the `linear_reg()` model specification.
#' @noRd
make_gee_reg <- function() {

	# Check to see if already loaded
	current <- parsnip::get_model_env()

	# If not loaded, then set up model
	if (!any(current$models == "gee_reg")) {

		# Start making new model
		parsnip::set_new_model("gee_reg")

		### GEE package

		parsnip::set_model_mode(model = "gee_reg", mode = "regression")
		parsnip::set_model_engine(model = "gee_reg", mode = "regression", eng = "gee")
		parsnip::set_dependency(model = "gee_reg", eng = "gee", pkg = "card")
		parsnip::set_dependency(model = "gee_reg", eng = "gee", pkg = "gee")

		# Arguments = id
		parsnip::set_model_arg(
			model = "gee_reg",
			eng = "gee",
			parsnip = "cluster",
			original = "id",
			func = list(pkg = "gee", fun = "gee"),
			has_submodel = FALSE
		)

		# Arguments = correlation
		parsnip::set_model_arg(
			model = "gee_reg",
			eng = "gee",
			parsnip = "correlation",
			original = "corstr",
			func = list(pkg = "gee", fun = "gee"),
			has_submodel = FALSE
		)

		# Encoding
		parsnip::set_encoding(
			model = "gee_reg",
			eng = "gee",
			mode = "regression",
			options = list(
				predictor_indicators = "none",
				compute_intercept = FALSE,
				remove_intercept = FALSE,
				allow_sparse_x = FALSE
			)
		)

		# Fit
		parsnip::set_fit(
			model = "gee_reg",
			eng = "gee",
			mode = "regression",
			value = list(
				interface = "formula",
				protect = c("formula", "data"),
				func = c(pkg = "gee", fun = "gee"),
				defaults = list(silent = TRUE)
			)
		)

		# Prediction
		parsnip::set_pred(
			model = "gee_reg",
			eng = "gee",
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

		### GEEPACK package

		parsnip::set_model_engine(model = "gee_reg", mode = "regression", eng = "geepack")
		parsnip::set_dependency(model = "gee_reg", eng = "geepack", pkg = "card")
		parsnip::set_dependency(model = "gee_reg", eng = "geepack", pkg = "geepack")

		# Arguments = type
		parsnip::set_model_arg(
			model = "gee_reg",
			eng = "geepack",
			parsnip = "cluster",
			original = "id",
			func = list(pkg = "geepack", fun = "geeglm"),
			has_submodel = FALSE
		)

		# Arguments = correlation
		parsnip::set_model_arg(
			model = "gee_reg",
			eng = "geepack",
			parsnip = "correlation",
			original = "corstr",
			func = list(pkg = "geepack", fun = "geeglm"),
			has_submodel = FALSE
		)

		# Encoding
		parsnip::set_encoding(
			model = "gee_reg",
			eng = "geepack",
			mode = "regression",
			options = list(
				predictor_indicators = "none",
				compute_intercept = FALSE,
				remove_intercept = FALSE,
				allow_sparse_x = FALSE
			)
		)

		# Fit
		parsnip::set_fit(
			model = "gee_reg",
			eng = "geepack",
			mode = "regression",
			value = list(
				interface = "formula",
				protect = c("formula", "data"),
				func = c(pkg = "geepack", fun = "geeglm"),
				defaults = list(family = gaussian)
			)
		)

		# Prediction
		parsnip::set_pred(
			model = "gee_reg",
			eng = "geepack",
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

#'@title General Interface for Generalized Estimating Equations
#'
#'@description `gee_reg()` is a _parsnip_ friendly method for specification of
#'  generalized estimating equation model before fitting. It currently uses the `gee` and `geepack` packages for its engines.
#'
#'@param mode A character string that describes the type of model. In this case,
#'  it currently supports type of "regression", which is default.
#'
#'@param cluster Name of the column that identifies the clustering variable. The
#'  column in the dataset that corresponds to the clustering variable should be
#'  of equal length as the other observations.
#'
#'@param correlation A character string specifying the correlation structure.
#'  Currently supports the following:
#'
#'  - `gee`: "independence", "fixed", "stat_M_dep", "non_stat_M_dep", "exchangeable", "AR-M", "unstructured"
#'
#'  - `geepack`: "independence", "exchangeable", "ar1", "unstructured", "userdefined"
#'
#' @examples
#' library(parsnip)
#' library(magrittr)
#' data(geh)
#' f <- qrs_tang ~ lab_hba1c
#' gee_mod <-
#'   gee_reg(cluster = hhp_id, correlation = "independence") %>%
#'   set_engine("geepack")
#'
#' @export
gee_reg <- function(mode = "regression", cluster = NULL, correlation = NULL) {

	# Check correct mode
	if (mode != "regression") {
		stop("`mode` should be 'regression'", call. = FALSE)
	}

	# Capture arguments
	args <- list(
		cluster = cluster,
		correlation = rlang::enquo(correlation)
	)

	# Model specs / slots
	parsnip::new_model_spec(
		"gee_reg",
		args = args,
		mode = mode,
		eng_args = NULL,
		method = NULL,
		engine = NULL
	)
}

#' @param object Generalized estimating equation model specification
#' @param ... Not used for `update()`
#' @param fresh A logical for whether the arguments should be modified in place or replaced altogether
#' @method update gee_reg
#' @rdname gee_reg
#' @export
update.gee_reg <- function(object, cluster = NULL, correlation = NULL, fresh = FALSE, ...) {
	parsnip::update_dot_check(...)

	# Updated arguments
	args <- list(
		cluster = rlang::enquo(cluster),
		correlation = rlang::enquo(correlation)
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
		"gee_reg",
		args = object$args,
		eng_args = object$eng_args,
		mode = object$mode,
		method = NULL,
		engine = object$engine
	)
}

#' @method print gee_reg
#' @rdname gee_reg
#' @param x GEE model specification
#' @param ... Extensible
#' @export
print.gee_reg <- function(x, ...) {
	cat("Generalized Estimating Equation Model Specification (", x$mode, ")\n\n", sep = "")
	parsnip::model_printer(x, ...)

	if (!is.null(x$method$fit$args)) {
		cat("Model fit template:\n")
		print(parsnip::show_call(x))
	}

	invisible(x)
}

# nocov end
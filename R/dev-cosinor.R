# make parsnip
parsnip::set_new_model("cosinor_reg")
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

# Model function
cosinor_reg <- function(mode = "regression", period = NULL) {

	# Check correct mode
	if(mode != "regression") {
		stop("`mode` should be 'regression'", call. = FALSE)
	}

	# Capture arguments
	args <- list(period = rlang::enquo(period))

	# Save empty slots for future parts of specification
	out <- list(
		args = args,
		mode = mode,
		eng_args = NULL,
		method = NULL,
		engine = NULL
	)

	# Set classes in correct order
	class(out) <- make_classes("cosinor_reg")
	out
}

# Add a fit model
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

# Description thus far
parsnip::show_model_info("cosinor_reg")

# Add modules for prediction
class_info <- list(
	pre = NULL,
	post = NULL,
	func = c(fun = "predict"),
	args =
)

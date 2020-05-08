# Constructor function {{{ ====

# Accepts the outcomes from the fit of the implemented model
new_cosinor <- function(
	coefs,
	coef_names,
	amp,
	phi,
	ellipse,
	blueprint
) {

	# Can validate coefs here
	if (!is.numeric(coefs)) {
		stop("`coef` should be a numeric vector.",
			 call. = FALSE)
	}

	# Names check
	if (!is.character(coef_names)) {
		stop("`coef_names` should be a character vector.",
			 call. = FALSE)
	}

	# Length check
	if(length(coefs) != length(coef_names)) {
		stop("`coefs` and `coef_names` must have same length.")
	}

	# Fit outputs need to match here
	hardhat::new_model(
		coefs = coefs,
		coef_names = coef_names,
		amp = amp,
		phi = phi,
		ellipse = ellipse,
		blueprint = blueprint,
		class = "cosinor"
	)
}


# }}}
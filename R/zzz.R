.onLoad <- function(libname, pkgname) {
	# Loads cosinor_reg in the model database
	make_cosinor_reg()
}


.onAttach <- function(libname, pkgname) {

	# Handle WFDB options
	op <- options()

	# Define default parameters as global options
	afib_parameters <- list(
		weight_symptoms = 0.5,
		weight_afeqt_total = 0.4,
		weight_afeqt_symptoms = 0.2,
		weight_afeqt_activities = 0.2,
		weight_afeqt_treatment = 0.2,
		weight_nyha_class = 1,
		weight_electrical = 0.5,
		mct_cutoff_low = 0.33,
		mct_cutoff_medium = 0.66,
		ecg_cutoff_low = 0.33,
		ecg_cutoff_medium = 0.66
	)


	op.card <- list(
		afib_parameters = afib_parameters
	)

	toset <- !(names(op.card) %in% names(op))
	if (any(toset)) options(op.card[toset])

}

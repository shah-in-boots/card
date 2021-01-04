.onLoad <- function(libname, pkgname) {
	# Loads cosinor_reg in the model database
	make_cosinor_reg()
	make_circular_reg()
	make_gee_reg()
	make_cox_reg_survival()
}
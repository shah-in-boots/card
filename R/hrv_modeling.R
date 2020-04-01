# HRV Linear Modeling {{{ ====

#' @title HRV Linear Modeling
#' @description
#' `hrv_linear_model` Linear models for each HRV measure.
#' @details Linear models built with dependent variable being the HRV measures (e.g. HF, LF, SDNN, etc). Allows for covariates to be included as available.
#' @param data Data frame that contains all covariates and outcomes. First column should be ID
#' @param covar Vector names of the covariates, with first covariate being the primary exposure variable for linear regression
#' @param hrv Vector names of the HRV measures, contained in `data`, that should be used. Can be generalized to any dependent variable set.
#' @param prop.weight This is a logical value if propensity weighting should be done instead of traditional covariate adjustment. This calls for the propensity weighting function defined by  \code{\link{recurrent_propensity}} that will generate both a PROP_SCORE column and PROP_WEIGHT column. Defaults to FALSE
#' @return List of models with names
#' @examples
#' m <- hrv_linear_model(df, c("DEPRESSION", "CHD", "HTN"), c("HF", "LF", SDNN"), FALSE)
#' m[[1:3]] # Models of HF, LF and SDNN as linear outcome variables
#' @export
hrv_linear_model <- function(data, covar, hrv, prop.weight = FALSE) {
	# Important variables/columns
	n <- length(hrv) # number of models to make
	names(data)[1] <- "ID"
	m <- list()

	# Create all of the models sequentially, storing in list
	for(i in 1:n) {
		# Formulas needed
		f <-
			paste(covar, collapse = " + ") %>%
			paste(hrv[i], ., sep = " ~ ") %>%
			as.formula()

		# Assess propensity weighting and dynamically build models
		# Propensity scoring for linear models DOES NOT WORK
		if(prop.weight == TRUE) {
			x <- recurrent_propensity(data, c(hrv[i], covar))
			m[[hrv[i]]] <- lm(f, data = x, weights = x$PROP_WEIGHT)
		} else {
			m[[hrv[i]]] <- lm(f, data = data)
		}
	}

	# Return models
	return(m)
}

# }}}
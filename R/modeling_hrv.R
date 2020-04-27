# Modeling HRV Variables {{{ ====

#' @title Modeling HRV Variables
#' @description Different HRV variables (e.g. HF, LF, SDNN) can be modeling as predictors for an outcome with adjustment by covariates. Can choose between logistic and linear based on the outcome variable as needed. Shortens process for modeling
#' @param data Data containing names of outcome, exposure, and covars
#' @param outcome Outcome of interest
#' @param exposure Exposure(s) of interest. If multiple given, will create a list of models to return.
#' @return List of models
#' @example
#' models <- model_hrv(df, "death", c("HF", "LF"), c("hptn", "dm"))
#' @param covar Covariates to include in model
#' @export
model_hrv <- function(data, outcome, exposure, covar=NULL, ...) {

	# Trim to only important data
	df <- data[c(outcome, exposure, covar)]

	# Identify outcome type
	n <- nlevels(factor(df[[outcome]]))

	# Make list to use
	m <- list()

	#  Modeling linear or logistic models
	for(i in 1:length(exposure)) {

		# Formulas needed
		if(is.null(covar)) {
			f <-
				paste(outcome, exposure[i], sep=" ~ ") %>%
				as.formula()
		} else {
			f <-
				paste(covar, collapse = " + ") %>%
				paste(exposure[i], ., sep = " + ") %>%
				paste(outcome, ., sep = " ~ ") %>%
				as.formula()
		}

		# Select model for by outcome type
		if(n == 2) { # Logistic
			m[[exposure[i]]] <-
				glm(f, data = df, family = binomial("logit"))
		} else if(n > 2) { # Linear
			m[[exposure[i]]] <-
				lm(f, data = df)
		} else {
			stop("`outcome` does fit a linear or logistic pattern.",
				 call. = FALSE)
		}
	}

	# Return list
	return(m)
}

# }}}
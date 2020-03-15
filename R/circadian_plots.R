#' @title
#' Odds Ratio Table by Time Point
#'
#' @description
#' `circadian_odds` creates an OR table for each time point of data given, initially applied to any grouping variable (particularly hour/time of day).
#'
#' @details
#' This function creates an OR table based on the covariate names supplied. It requires that there is an appropriate outcome variable selected. It performs a logistic regression. This model does not allow for conditioning variables (yet).
#'
#' @param dataframe Dataframe containing subsequent columns
#' @param time Column name that contains the grouping variable of time
#' @param outcome Column name that identifies the per-row outcome, binary
#' @param covariates Vector of independent variables names. First variable needs to be exposure.
#' @return A data frame of odds ratios
#'
#' @export
circadian_odds <- function(dataframe, time, outcome, covariates) {
	# Create formula
	f <-
		paste(outcome, paste(covariates, collapse=" + "), sep = " ~ ") %>%
		as.formula()

	# Data frame to use
	df <- dataframe[c(time, outcome, covariates)]
	colnames(df)[1] <- "time"
	df$time %<>% as.factor()

	# Regressions by hour, limited to just stated variables
	odds <-
		df %>%
		group_by(time) %>%
		nest() %>%
		mutate(
			regression = map(data, ~ glm(formula = f, data = .x, family = binomial("logit")))
		)

	# Clean and exponentiate
	odds$OR <- map_dbl(odds$regression, function(x) {exp(coef(x)[2])})
	odds$Lower <- map_dbl(odds$regression, function(x) {exp(confint(x)[2,1])})
	odds$Upper <- map_dbl(odds$regression, function(x) {exp(confint(x)[2,2])})

	# Subset the data and return a table of odds
	ot <- subset(odds, select = c(hour, OR, Lower, Upper))
	return(ot)
}



#' @title
#' Forest Plot of Hourly Odds
#'
#' @description
#' `circadian_forest` creates an OR table for each hour of data given
#'
#' @details
#' This function creates an OR table based on the covariate names supplied. It requires that there is an appropriate outcome variable selected.
#'
#' @export
circadian_forest <- function() {
}
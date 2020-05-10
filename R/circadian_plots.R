# Odds Ratio Table by Time Point {{{ ====

#' @title Odds Ratio Table by Time Point
#'
#' @description `circadian_odds` creates an OR table for each time point
#'   of data given, initially applied to any grouping variable
#'   (particularly hour/time of day).
#'
#' @details This function creates an OR table based on the covariate
#'   names supplied. It requires that there is an appropriate outcome
#'   variable selected. It performs a logistic regression. This model
#'   does not allow for conditioning variables (yet).
#'
#' @param data Dataframe containing subsequent columns
#'
#' @param time Column name that contains the grouping variable of time
#'
#' @param outcome Column name that identifies the per-row outcome,
#'   binary
#'
#' @param covariates Vector of independent variables names. First
#'   variable needs to be exposure.
#'
#' @import magrittr
#'
#' @return A data frame of odds ratios
#'
#' @export
circadian_odds <- function(data, time, outcome, covariates) {
	# Create formula
    f <-
		paste(outcome, paste(covariates, collapse=" + "), sep = " ~ ") %>%
		as.formula()

	# Data frame to use
	df <- data[c(time, outcome, covariates)]
	colnames(df)[1] <- "time"
	df$time %<>% as.factor()

	# Regressions by hour, limited to just stated variables
	odds <-
		df %>%
		dplyr::group_by(time) %>%
		dplyr::nest() %>%
		dplyr::mutate(
			regression = purrr::map(data, ~ glm(formula = f, data = .x, family = binomial("logit")))
		)

	# Clean and exponentiate
	odds$OR <- purrr::map_dbl(odds$regression, function(x) {exp(coef(x)[2])})
	odds$Lower <- purrr::map_dbl(odds$regression, function(x) {exp(confint(x)[2,1])})
	odds$Upper <- purrr::map_dbl(odds$regression, function(x) {exp(confint(x)[2,2])})

	# Subset the data and return a table of odds
	ot <- subset(odds, select = c(time, OR, Lower, Upper))
	return(ot)
}

# }}}

# Forest Plot of Hourly Odds {{{ ====

#' @title
#' Forest Plot of Hourly Odds
#' @description
#' `geom_forest` creates an OR plot for each hour of data given. Its a
#' ggplot format so additional variables, like titles, can be added in.
#' @details This function creates a forest plot using the OR developed
#'   by the `circadian_odds` function in this package. By default, it
#'   takes the output of `circadian odds`, which is a tibble named "ot",
#'   and will generate a forest plot based on the grouping variable
#'   (default is time of day). Original data can be restricted or the
#'   hours can be reduced).
#' @param group Is the grouping variable from data in ggplot, like hour
#' @param or Odds ratio
#' @param lower Lower boundary of 95 percent CI
#' @param upper Upper boundary of 95 percent CI
#' @examples
#' ggplot(OddsTable) + geom_forest("hour", "OddsRatio", "LowerCI", "UpperCI")
#' @return A ggplot geom that has been configured for a forest plot.
#' @export
geom_forest <- function(group="time", or="OR", lower="Lower", upper="Upper", ...) {
	list(
		aes_string(x = group, y = or),
		coord_flip(),
		geom_errorbar(aes_string(ymin = lower, ymax = upper), size = 0.2, width = 1/3),
		geom_point(aes_string(colour = or), size = 3),
		geom_hline(yintercept = 1),
		scale_color_viridis_c(),
		theme_minimal(),
		scale_y_log10(),
		theme(plot.caption = element_text(hjust = 0))
	)
}

# }}}



# Odds Ratio Table by Time Point {{{ ====

#' @title
#' Odds Ratio Table by Time Point
#' @description
#' `circadian_odds` creates an OR table for each time point of data given, initially applied to any grouping variable (particularly hour/time of day).
#' @details
#' This function creates an OR table based on the covariate names supplied. It requires that there is an appropriate outcome variable selected. It performs a logistic regression. This model does not allow for conditioning variables (yet).
#' @param data Dataframe containing subsequent columns
#' @param time Column name that contains the grouping variable of time
#' @param outcome Column name that identifies the per-row outcome, binary
#' @param covariates Vector of independent variables names. First variable needs to be exposure.
#' @return A data frame of odds ratios
#' @examples
#' circadian_odds(df, hour, outcome, c(covar1, covar2))
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
	ot <- subset(odds, select = c(time, OR, Lower, Upper))
	return(ot)
}

# }}}

# Forest Plot of Hourly Odds {{{ ====

#' @title
#' Forest Plot of Hourly Odds
#' @description
#' `geom_forest` creates an OR plot for each hour of data given. Its a ggplot format so additional variables, like titles, can be added in.
#' @details
#' This function creates a forest plot using the OR developed by the `circadian_odds` function in this package. By default, it takes the output of `circadian odds`, which is a tibble named "ot", and will generate a forest plot based on the grouping variable (default is time of day). Original data can be restricted or the hours can be reduced).
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

# Compare Repeated Measurements by Group {{{ ====

#' @title Compare Repeated Measurements by Group
#' @description Takes data and returns a summary table of continuous variable based on a categorical variable. This summary is repeat by time groups to help describe a circadian pattern.
#' @details Applies a simple data transformation to identify the summary statistics of the data frame by the stated variables. Results in a mean, standard deviation, and standard error term. This data is also used for making a t-test based table, which can then also be graphed in [autonomicR::geom_circadian].
#' @param data Dataframe containing all the following variables
#' @param time Name of the time-dependent variable, usually hours
#' @param x Continuous variable of interest (x ~ y)
#' @param y Grouping variable to apply to the `cvar` (x ~ y). Must be binary for t-test, otherwise will return data set without pvalues
#' @return Returns a dataframe that has the time variable, the categorical variable, and the statistics (including p-value) of the continuous variable
#' @example
#' # Data
#' dataDemo %>% # Sample data in package
#' circ_compare_groups(., x = "rDYX", y = "sad_cat", time = "hour")
#' @export
circ_compare_groups <- function(data, x, y, time) {

	# Important variables
	dt <- data.table::as.data.table(data[c(x, y, time)])
	names(dt) <- c("contvar", "catvar", "timegrp")
	dt$catvar %<>% factor() # Ensure categorical variable is factor

	# Data set needs to be summarized
	dt_summary <-
		dt[, list(mean = lapply(.SD, mean, na.rm = TRUE),
				  n = lapply(.SD, length),
				  sd = lapply(.SD, sd),
				  se = lapply(.SD, function(x) {sd(x)/sqrt(length(x))})),
		   by = .(timegrp, catvar),
		   .SDcols = "contvar"]

	# If only two groups, perform t-test
	if(nlevels(dt$catvar) == 2) {

		# T-tests per each time point and group
		dt_ttest <-
			dt[, .(pval = t.test(data=.SD, contvar ~ catvar)$p.value), by = timegrp]

		# Merge in summary data
		x <- dt_summary[dt_ttest, on = "timegrp"] %>%
			unnest(cols = c(mean, n, sd, se))

		# For groups > 2
	} else {

		x <- dt_summary %>%
			unnest(cols = c(mean, n, sd, se))
	}


	# Combine data
	x$timegrp %<>% factor()

	# Rename columns before returning data
	names(x)[names(x) == "timegrp"] <- eval(time)
	names(x)[names(x) == "catvar"] <- eval(y)

	# Return as tibble
	return(x)
}

#  }}}

# Circadian Plot by Group {{{ ====

#' @title Circadian Plot by Group
#' @description
#' `geom_circadian` converts the output of [autonomicR::circadian_ttest] into a complex geom that is broken down by time/hour and HRV (or any other continuous variable). Each hour is then separated by the grouping variable.
#' @details Currently creates a geom_layer that shows a error bar and point estimate of values by group (e.g. clinical status). If t-test values are available in the data frame, shows points of significance.
#' @param time Name of time group variable, such as hours of day, which ends up being the x-axis
#' @param group Name of categorical variable to stratify the y-axis
#' @return Returns a ggplot object of geom type, other layers can be added on as seen in example.
#' @examples
#' ggplot(df_with_ttest) + geom_circadian("hour", "catvar", TRUE)
#' @export
geom_circadian <- function(time, group, pval=FALSE) {

	if(pval) { # If pval present

		list(
			aes_string(x = time, color = group),
			geom_point(aes(y = mean),
					   position = position_dodge(.3)),
			scale_shape_manual(values = c(1,16)),
			geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
						  width = 0.2,
						  position = position_dodge(.3)),
			geom_text(data = df[df$pval < 0.05/24 & df$pval > 0.0005/24, ],
					  aes(y = 2.2, label = "*"),
					  colour = "black", size = 4),
			geom_text(data = df[df$pval < 0.005/24 & df$pval > 0.000005/24, ],
					  aes(y = 2.2, label = "**"),
					  colour = "black", size = 4),
			geom_text(data = df[df$pval < 0.0005/24, ],
					  aes(y = 2.2, label = "***"),
					  colour = "black", size = 4),
			labs(caption = "*p < .05, **p<.005, ***p<.0005"),
			scale_color_viridis_d(option = "E",
								begin = .1, end = .9),
			theme_minimal(),
			theme(
				plot.caption = element_text(hjust = 0),
				legend.position = "bottom",
				legend.box = "horizontal"
			)
		)

	} else { # For pval missing

		list(
			aes_string(x = time, color = group),
			geom_point(aes(y = mean),
					   position = position_dodge(.3)),
			scale_shape(solid = TRUE),
			geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
						  width = 0.2,
						  position = position_dodge(.3)),
			scale_color_viridis_d(option = "E",
								begin = .1, end = .9),
			theme_minimal(),
			theme(
				plot.caption = element_text(hjust = 0),
				legend.position = "bottom",
				legend.box = "horizontal"
			)
		)

	}

}


# }}}

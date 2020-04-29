# Compare Repeated Measurements by Group {{{ ====

#' @title Compare Repeated Measurements by Group
#' @description Takes data and returns a summary table of continuous
#'   variable based on a categorical variable. This summary is repeat by
#'   time groups to help describe a circadian pattern.
#' @details Applies a simple data transformation to identify the summary
#'   statistics of the data frame by the stated variables. Results in a
#'   mean, standard deviation, and standard error term. This data is
#'   also used for making a t-test based table, which can then also be
#'   graphed in [autonomicR::geom_circadian].
#' @param data Dataframe containing all the following variables
#' @param time Name of the time-dependent variable, usually hours
#' @param x Continuous variable of interest (x ~ y)
#' @param y Grouping variable to apply to the `cvar` (x ~ y). Must be
#'   binary for t-test, otherwise will return data set without pvalues
#' @return Returns a dataframe that has the time variable, the
#'   categorical variable, and the statistics (including p-value) of the
#'   continuous variable
#' @example # Data dataDemo %>% # Sample data in package
#'   circ_compare_groups(., x = "rDYX", y = "sad_cat", time = "hour")
#' @import data.table
#' @export
circ_compare_groups <- function(data, x, y, time) {

	# Check arguments
	if (nargs() != 4) {
		stop("Incorrect number of arguments", call. = FALSE)
	}

	# Important variables
	dt <- data.table::as.data.table(data[c(x, y, time)])
	names(dt) <- c("contvar", "catvar", "timegrp")
	dt[, catvar := factor(catvar)] # categorical variable is factor

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
#' @description \code{\link{geom_circadian}} converts the output of
#' \code{\link[autonomicR]{circ_compare_groups}} into a complex geom
#' that is broken down by time/hour and HRV (or any other continuous
#' variable). Each hour is then separated by the grouping variable.
#' @details Currently creates a geom_layer that shows a error bar and
#'   point estimate of values by group (e.g. clinical status). If t-test
#'   values are available in the data frame, shows points of
#'   significance.
#' @param time Name of time group variable, such as hours of day, which
#'   ends up being the x-axis
#' @param group Name of categorical variable to stratify the y-axis
#' @return Returns a ggplot object of geom type, other layers can be
#'   added on as seen in example.
#' @examples
#' ggplot(df_with_ttest) + geom_circadian("hour", "catvar", TRUE)
#' @import ggplot2
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
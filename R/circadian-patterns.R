# Compare Repeated Measurements by Group {{{ ====

#' @title Compare Repeated Measurements by Group
#'
#' @description Takes data and returns a summary table of continuous
#'   variable based on a categorical variable. This summary is repeat by
#'   time groups to help describe a circadian pattern.
#'
#' @details Applies a simple data transformation to identify the summary
#'   statistics of the data frame by the stated variables. Results in a
#'   mean, standard deviation, and standard error term. This data is
#'   also used for making a t-test based table, which can then also be
#'   graphed in [card::ggcircadian].
#'
#' @param data Dataframe containing all the following variables
#'
#' @param time Name of the time-dependent variable, usually hours
#'
#' @param x Continuous variable of interest (x ~ y)
#'
#' @param y Grouping variable to apply to the `cvar` (x ~ y). Must be
#'   binary for t-test, otherwise will return data set without pvalues
#'
#' @return Returns a dataframe that has the time variable, the
#'   categorical variable, and the statistics (including p-value) of the
#'   continuous variable
#'
#' @examples
#' data("twins")
#' circ_compare_groups(data = twins, x = "rDYX", y = "sad_cat", time = "hour")
#' @importFrom data.table data.table .N .I .SD .BY fread :=
#'
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
    dt[, list(
      mean = lapply(.SD, mean, na.rm = TRUE),
      n = lapply(.SD, length),
      sd = lapply(.SD, sd),
      se = lapply(.SD, function(x) {
        sd(x) / sqrt(length(x))
      })
    ),
    by = .(timegrp, catvar),
    .SDcols = "contvar"
    ]

  # If only two groups, perform t-test
  if (nlevels(dt$catvar) == 2) {

    # T-tests per each time point and group
    dt_ttest <-
      dt[, .(pval = stats::t.test(data = .SD, contvar ~ catvar)$p.value), by = timegrp]

    # Merge in summary data
    x <- dt_summary[dt_ttest, on = "timegrp"] %>%
      tidyr::unnest(cols = c(mean, n, sd, se))

    # For groups > 2
  } else {
    x <- dt_summary %>%
      tidyr::unnest(cols = c(mean, n, sd, se))
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
#'
#' @description Converts the output of
#' [card::circ_compare_groups] into a complex geom
#' that is broken down by time/hour and HRV (or any other continuous
#' variable). Each hour is then separated by the grouping variable.
#'
#' @details Currently creates a ggplot that shows a error bar and
#'   point estimate of values by group (e.g. clinical status). If t-test
#'   values are available in the data frame, shows points of
#'   significance.
#'
#'
#' @param data Table generated which has time variable, categorical outcome
#'   variable, and summary statistics, including a possible column called "pval"
#'   which, if present, will document statistical significance in plot.
#'
#' @param mean,n,sd,se,pval Summary statistics to be included in graphics.
#'
#' @param time Name of time group variable, such as hours of day, which
#'   ends up being the x-axis
#'
#' @param outcome Name of categorical variable to stratify the y-axis
#'
#' @return Returns a ggplot object of geom type, other layers can be
#'   added on as seen in example.
#'
#' @examples
#' # Data
#' data(twins)
#' tbl <- circ_compare_groups(twins, "rDYX", "sad_cat", "hour")
#'
#' # Plot
#' library(ggplot2)
#' ggcircadian(tbl, outcome = "sad_cat") +
#'   labs(title = "Example") +
#'   scale_color_viridis_d(option = "A", begin = 0.0, end = 0.75)
#' @import ggplot2
#'
#' @export
ggcircadian <- function(data, outcome, time = "hour", mean = "mean", n = "n", sd = "sd", se = "se", pval = "pval") {

  # Set up errorbars
  data$ymin <- data$mean - data$se
  data$ymax <- data$mean + data$se

  if ("pval" %in% colnames(data)) { # If Pval is present

    # ggplot to return
    gg <- ggplot(data, aes_string(x = time, y = mean, color = outcome)) +
      geom_point(position = position_dodge(0.3)) +
      scale_shape_manual(values = c(1, 16)) +
      geom_errorbar(aes(ymin = ymin, ymax = ymax),
        width = 0.2,
        position = position_dodge(.3)
      ) +
      geom_text(
        data = data[data$pval < 0.05 / 24 & data$pval >= 0.005 / 24, ],
        aes(y = 2.2, label = "*"),
        colour = "black", size = 4
      ) +
      geom_text(
        data = data[data$pval < 0.005 / 24 & data$pval >= 0.0005 / 24, ],
        aes(y = 2.2, label = "**"),
        colour = "black", size = 4
      ) +
      geom_text(
        data = data[data$pval < 0.0005 / 24, ],
        aes(y = 2.2, label = "***"),
        colour = "black", size = 4
      ) +
      labs(caption = "*p < .05, **p<.005, ***p<.0005") +
      scale_color_viridis_d(option = "E", begin = .1, end = .9) +
      theme_minimal() +
      theme(
        plot.caption = element_text(hjust = 0),
        legend.position = "bottom",
        legend.box = "horizontal"
      )
  } else { # If pval is missing

    # ggplot to return
    gg <- ggplot(data, aes_string(x = time, y = mean, color = outcome)) +
      geom_point(position = position_dodge(0.3)) +
      scale_shape_manual(values = c(1, 16)) +
      geom_errorbar(aes(ymin = ymin, ymax = ymax),
        width = 0.2,
        position = position_dodge(.3)
      ) +
      scale_color_viridis_d(option = "E", begin = .1, end = .9) +
      theme_minimal() +
      theme(
        plot.caption = element_text(hjust = 0),
        legend.position = "bottom",
        legend.box = "horizontal"
      )
  }

  # Return
  return(gg)
}

# }}}


# Odds Ratio Table by Time Point {{{ ====

#' @title Odds Ratio Table by Time Point
#'
#' @description Creates an OR table for each time point of data given, initially
#'   applied to any grouping variable (particularly hour/time of day).
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
#' @param covar Vector of independent variables names. First
#'   variable needs to be exposure.
#'
#' @importFrom magrittr %>% %<>%
#'
#' @return A data frame of odds ratios
#'
#' @examples
#' # Data
#' data(twins)
#'
#' # Create odds ratio tables by hour of day for covariate of interest
#' ot <- circ_odds(twins, "hour", "sad_bin", "rDYX")
#' @export
circ_odds <- function(data, time, outcome, covar) {
  # Create formula
  f <-
    paste(outcome, paste(covar, collapse = " + "), sep = " ~ ") %>%
    stats::as.formula()

  # Data frame to use
  df <- data[c(time, outcome, covar)]
  colnames(df)[1] <- "time"
  df$time %<>% as.factor()

  # Regressions by hour, limited to just stated variables
  odds <-
    df %>%
    dplyr::group_by(time) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      regression = purrr::map(data, ~ glm(formula = f, data = .x, family = binomial("logit")))
    )

  # Clean and exponentiate
  odds$OR <-
    purrr::map_dbl(odds$regression, function(x) {
      exp(stats::coef(x)[2])
    })
  odds$Lower <-
    purrr::map_dbl(odds$regression, function(x) {
      exp(stats::confint(x)[2, 1])
    })
  odds$Upper <-
    purrr::map_dbl(odds$regression, function(x) {
      exp(stats::confint(x)[2, 2])
    })

  # Subset the data and return a table of odds
  ot <- subset(odds, select = c(time, OR, Lower, Upper))
  return(ot)
}

# }}}

# Forest Plot of Hourly Odds {{{ ====

#' @title
#' Forest Plot of Hourly Odds
#'
#' @description Creates an OR plot for each hour of data given. Its a
#' ggplot format so additional variables, like titles, can be added in.
#'
#' @details This function creates a forest plot using the OR developed
#'   by the [card::circ_odds] function in this package. By default, it
#'   takes the output, which is a tibble named "ot",
#'   and will generate a forest plot based on the grouping variable
#'   (default is time of day). Original data can be restricted or the
#'   hours can be reduced).
#'
#' @param ot Odd Ratio table with the following columns.
#'
#' @param time Name of time variable (or "grouping" variable)
#'
#' @param or Name of column containing odds ratio
#'
#' @param lower Name of column of lower boundary of 95 percent CI
#'
#' @param upper Name of column of upper boundary of 95 percent CI
#'
#' @examples
#' # Data
#' data(twins)
#' ot <- circ_odds(twins, "hour", "sad_bin", "rDYX")
#'
#' # Plot
#' library(ggplot2)
#' ggforest(ot) +
#'   labs(title = "Example") +
#'   scale_color_viridis_c(option = "A")
#' @return A ggplot of forest plot that can be extended. Default theme is
#'   minimal and default color scheme is viridis.
#'
#' @import ggplot2
#'
#' @export
ggforest <- function(ot, time = "time", or = "OR", lower = "Lower", upper = "Upper") {

  # Ggplot
  gg <- ggplot(data = ot, aes_string(x = time, y = or)) +
    coord_flip() +
    geom_errorbar(aes_string(ymin = lower, ymax = upper),
      size = 0.2,
      width = 1 / 3
    ) +
    geom_point(aes_string(colour = or), size = 3) +
    geom_hline(yintercept = 1) +
    scale_color_viridis_c() +
    theme_minimal() +
    scale_y_log10() +
    theme(plot.caption = element_text(hjust = 0))

  return(gg)
}

# }}}

#' @title HRV Linear Modeling
#' @description `hrv_linear_model` Linear models for each HRV measure.
#' @details Linear models built with dependent variable being the HRV measures
#'   (e.g. HF, LF, SDNN, etc). Allows for covariates to be included as
#'   available.
#' @param data Data frame that contains all covariates and outcomes. First
#'   column should be ID
#' @param covar Vector names of the covariates, with first covariate being the
#'   primary exposure variable for linear regression
#' @param hrv Vector names of the HRV measures, contained in `data`, that should
#'   be used. Can be generalized to any dependent variable set.
#' @param prop.weight This is a logical value if propensity weighting should be
#'   done instead of traditional covariate adjustment. This calls for the
#'   propensity weighting function defined by
#'   [card::recurrent_propensity] that will generate both a PROP_SCORE
#'   column and PROP_WEIGHT column. Defaults to FALSE
#' @return List of models with names
#' @export
hrv_linear_model <-
  function(data, covar, hrv, prop.weight = FALSE) {
    # Important variables/columns
    n <- length(hrv) # number of models to make
    names(data)[1] <- "ID"
    m <- list()

    # Create all of the models sequentially, storing in list
    for (i in 1:n) {
      # Formulas needed
      f <-
        paste(covar, collapse = " + ") %>%
        paste(hrv[i], ., sep = " ~ ") %>%
        stats::as.formula()

      # Assess propensity weighting and dynamically build models
      # Propensity scoring for linear models DOES NOT WORK
      if (prop.weight == TRUE) {
        x <- recurrent_propensity(data, c(hrv[i], covar))
        m[[hrv[i]]] <- stats::lm(f, data = x, weights = x$PROP_WEIGHT)
      } else {
        m[[hrv[i]]] <- stats::lm(f, data = data)
      }
    }

    # Return models
    return(m)
  }

#' @title Model Building
#'
#' @description Simplify the process of building multiple models. Models are
#'   usually built in sequential order, or in parallel when testing multiple
#'   potential patterns. This is particularly helpful in epidemiological cases
#'   of testing effect of additional parameters. Every parameter should be
#'   theoretically a part of the causal model for the exposure-outcome
#'   relationship.
#'
#' @details This is considering what is available with the `modelr` package and
#'   the `tidymodels` approach, and finding an in-between for the causality /
#'   epidemiology approach of building intentional, sequentional models. Expect
#'   changes in the process.
#'
#' @param formula an object of class `formula` that shows the names of the
#'   outcomes (can be more than 1) and the names of the predictors (which should
#'   contain the `exposure` variable). This formula can include mixed effects if
#'   needed.
#'
#' @param data data frame or data table (or tibble) that contains the named
#'   variables
#' @param type Type of model that will be used. The options are:
#'
#'   * `sequential` will build y ~ x1, y ~ x1 + x2 models
#'
#'   * `parallel` will build y ~ x1, y ~ x2 models
#'
#' @param exposure Variable that is forced to be maintained in every model as a
#'   predictor. This is currently used only for `sequential` type models
#' @param engine Set the "engine" or the regression tool that will be used. One
#'   day this may be a useful addition to the `tidymodels` approach, which is
#'   the inspiration for the terminology. Currently limited to the following:
#'
#'   * `linear` uses the stats::lm() for linear regression, and lme4::lmer() for
#'   mixed effect models
#'
#' @return A tidy tibble of models. Each one will likely be grouped by its
#'   outcome, and then with sequential columns using increased/additive models.
#'   Each model, in a tidy format, will have two additional columns.
#'
#'   * `outcomes` identifies which outcome was used for the specific regression
#'
#'   * `covar` number of covariates used in sequence of predictors given, with
#'   exposure always being placed in position 1
#'
#' @examples
#' data(geh)
#' f <- svg_mag + qrs_tang ~ lab_hba1c + bmi
#'
#' @importFrom magrittr %>%
#' @export
build_models <- function(formula, data, type, engine = "linear", exposure = NULL) {

  # Get terms
  o <- all.vars(lme4::nobars(formula)[[2]])
  p <- all.vars(lme4::nobars(formula)[[3]])
  m <- lme4::findbars(formula) # This finds if there is a mixed effect model or note
  no <- length(o)
  np <- length(p)
  nm <- length(m)
  if(nm >= 1) {mixed <- TRUE}

  # Type of model to build
  switch(
    type,
    parallel = {
      l <- list()
      for(i in 1:no) {
        for(j in 1:np) {
          # Add mixed effects here
          if(mixed) {
            mix <- paste0("(", m, ")", collapse = " + ")
            predictors <- paste0(p[j], " + ", mix)
          } else {
            predictors <- p[j]
          }

          # Create formulas
          f <- stats::formula(paste0(o[[i]], " ~ ", predictors))
          l[[o[[i]]]][[j]] <- f
        }
      }
    },
    sequential = {
      # Ensure exposure is maintained if sequential
      if(!is.null(exposure)) {
        p <- p[-(which(p == exposure))]
        p <- c(exposure, p)
      }

      # Creating formulas
      l <- list()
      for(i in 1:no) {
        for(j in 1:np) {
          # Add mixed effects here if needed
          predictors <- paste0(p[1:j], collapse = " + ")
          if(mixed) {
            mix <- paste0("(", m, ")", collapse = " + ")
            f <- stats::formula(paste0(o[[i]], " ~ ", predictors, " + ", mix))
          } else {
            f <- stats::formula(paste0(o[[i]], " ~ ", predictors))
          }

          # Save them
          l[[o[[i]]]][[j]] <- f
        }
      }
    }
  )

  # Create the models using hte formulas in `l`
  models <- list()
  for(i in 1:no) {
    for(j in 1:np) {
      if(mixed) {
        m <- lme4::lmer(formula = l[[i]][[j]], data = data)
        models[[o[[i]]]][[j]] <- broom.mixed::tidy(m, conf.int = TRUE)
      } else {
        m <- stats::lm(formula = f, data = data)
        models[[o[[i]]]][[j]] <- broom::tidy(m, conf.int = TRUE)
      }
    }
  }

  # Tidy it up
  res <-
    dplyr::as_tibble(models) %>%
    tidyr::pivot_longer(
      col = tidyr::everything(),
      names_to = "outcomes",
      values_to = "models"
    ) %>%
    dplyr::mutate(covar = purrr::map_dbl(models, nrow) - 1) %>%
    tidyr::unnest(cols = "models")

  # Return
  return(res)
}

#' @title Plotting Error of Models
#' @description Creates a ggplot geom that can be extended and accept other
#'   ggplot layers. Shows residual error from the regression mean for different
#'   types of regression models.
#' @details Generate residuals for models. Currently accepts only linear models.
#'   Does not account for covariates yet, although may be able to do this in the
#'   future.
#' @param model Model to be analyzed. The function will detect what type of
#'   family the model is (e.g. linear = "gaussian", logistic = "binomial") and
#'   plot the appropriate type of model.
#' @return Returns a ggplot object of geom type, other layers can be added on as
#'   seen in example.
#' @examples
#' data("twins")
#' model <- lm(beck_total ~ HR, data = subset(twins, hour == 7))
#' ggerror(model)
#' @import ggplot2
#' @export
ggerror <- function(model) {

  # Create an augmented df for visualizing
  m <- broom::augment(model, type.predict = "response")

  # Get names of outcome and exposure
  var <- grep("\\.", names(m), value = TRUE, invert = TRUE)
  yaxis <- var[1]
  xaxis <- var[2]

  # Identify what type of model
  type <- stats::family(model)$family

  switch(
    type,
    gaussian = {

      # ggplot geom for linear models
      gg <- ggplot(m, aes_string(x = xaxis, y = yaxis)) +
        geom_point(aes(y = .fitted), shape = 1) +
        # Need string names for axes
        geom_segment(aes_string(xend = xaxis, yend = ".fitted", alpha = 0.2)) +
        geom_point(aes(colour = abs(.resid), size = abs(.resid))) +
        stat_smooth(method = "lm", se = FALSE, colour = "dimgrey") +
        scale_color_continuous(type = "viridis") +
        guides(color = FALSE, size = FALSE, alpha = FALSE) +
        theme_minimal()
    },

    # Working on binomials
    binomial = {
      print("Not yet working for binomials")
    },

    # Catch all
    stop("Neither a linear (lm) or logistic (glm(family = binomial)) model")
  )

  # Proper plot back
  return(gg)
}

#' @title Plotting Residual of a Model
#' @description `geom_residuals` makes a diagnostic plot of residuals versus
#' fitted data for linear models. Does not yet accept logistic models
#' @details Generate residuals versus fitted plot. Functions as an additional
#'   geom layer on ggplot. Models must be linear/gaussian in nature. Covariates
#'   can be included in the model.
#' @param model Model to be analyzed, currently only accepts linear models.
#' @return Returns a ggplot object of geom type, other layers can be added on as
#'   seen in example.
#' @import ggplot2
#' @export
geom_residuals <- function(model) {
  # Augment model
  model_dx <- broom::augment(model, type.predict = "response")

  # Make geom layer
  list(
    aes_string(x = ".fitted", y = ".resid"),
    geom_point(data = model_dx, aes_string(x = ".fitted", y = ".resid")),
    stat_smooth(
      data = model_dx,
      aes(x = .fitted, y = .resid),
      method = "loess",
      color = "cornflowerblue"
    ),
    geom_hline(
      yintercept = 0,
      col = "brown4",
      linetype = "dashed"
    ),
    labs(
      title = "Residual versus fitted plot",
      x = "Fitted",
      y = "Residuals"
    ),
    theme_minimal()
  )
}

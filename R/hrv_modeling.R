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

# HRV Model Building {{{ ====

#' @title HRV Model Building
#' @description
#' `hrv_model_building` creates a list of models using different builds of covariates.
hrv_model_building <- function(data, covar.builds, model, prop.scores = NULL) {
	# Important variables / columns
	n <- length(covar.builds)
	names(data)[1:5] <- c("ID", "TSTART", "TSTOP", "STATUS", "EVENT")
	m <- list()

	# Create all the models in sequence
	for(i in 1:n) {
		# Create formulas
		f <-
			paste(get(covar.builds[i]), collapse = " + ") %>%
			paste("Surv(TSTART, TSTOP, STATUS)", ., sep = " ~ ") %>%
			# Different recurrent event models with cluster and strata
			purrr::when(
				model == "marginal" ~ paste(., "cluster(ID)", sep = " + "),
				model == "pwptt" ~ paste(., "cluster(ID)", "strata(EVENT)", sep = " + "),
				model == "pwpgt" ~ paste(., "cluster(ID)", "strata(EVENT)", sep = " + "),
				~ as.formula()
			) %>%
			as.formula()

		# Assess need for propensity weighting
		# Dynamically save the models
		if(covar.builds[i] %in% prop.scores) {
			# Uses the recurrent_propensity function
			x <- recurrent_propensity(data, get(covar.builds[i]))
			m[[i]] <- coxph(f, method = "breslow", data = x, weights=x$PROP_WEIGHT)
		} else {
			m[[i]] <- coxph(f, method = "breslow", data = data)
		}
	}

	# Return output
	return(m)
}

# }}}

# Plotting Error of Models {{{ ====

#' @title Plotting Error of Models
#' @description
#' `geom_model_error` creates a ggplot geom that can be extended and accept other ggplot layers. Shows residual error from the regression mean for different types of regression models.
#' @details Generate residuals for models. Currently accepts only linear models. Does not account for covariates yet, although may be able to do this in the future.
#' @param model Model to be analyzed. The function will detect what type of family the model is (e.g. linear = "gaussian", logistic = "binomial") and plot the appropriate type of model.
#' @return Returns a ggplot object of geom type, other layers can be added on as seen in example.
#' @examples
#' ggplot() + geom_model_error(model) + labs(x = "HRV values", y = "Depression Score", title = "Model Residuals")
#' @export
geom_model_error <- function(model) {

	# Create an augmented df for visualizing
	model_dx <- broom::augment(model, type.predict = "response")

	# Get names of outcome and exposure
	var <- grep("\\.", names(model_dx), value=TRUE, invert=TRUE)
	yaxis <- var[1]
	xaxis <- var[2]

	# Identify what type of model
	type <- family(model)$family

	# Cases (either logistic or linear)
	switch(
		type,

		# Linear
		gaussian = {
			# ggplot geom for linear models
			list(
				aes_string(x = xaxis, y = yaxis),
				scale_color_continuous(type = "viridis"),
				guides(color = FALSE, size = FALSE),
				geom_point(data = model_dx, aes_string(y = ".fitted"), shape = 1),
				geom_segment(data = model_dx, aes_string(xend = xaxis, yend = ".fitted"), alpha = 0.2),
				geom_point(data = model_dx, aes(colour = abs(.resid), size = abs(.resid))),
				stat_smooth(data = model_dx, method = "lm", se = FALSE, colour = "dimgrey"),
				theme_minimal()
			)
		},

		# Logistic
		binomial = {
			# ggplot geom for logistic models
			list(
				# Plot axes (actual data)
				aes_string(x = xaxis, y = yaxis),
				#scale_color_continuous(type = "viridis"),
				#guides(color = FALSE, size = FALSE),
				geom_point(data = model_dx, aes(y = .fitted), shape = 1),
				#geom_point(data = model_dx %>% filter(!!sym(yaxis) != round(.fitted)), size = 2, color = "red"),
				# Segment from geom
				#geom_segment(data = model_dx, aes_string(xend = xaxis, yend = ".fitted"), alpha = 0.2),
				#geom_point(data = model_dx, aes(colour = abs(.resid), size = abs(.resid))),
				# Predicted values
				#geom_line(data = model_dx, aes(y = .fitted)),
				#stat_smooth(data = model_dx, method = "glm", se = FALSE, method.args = list(family=binomial), colour = "dimgrey"),
				theme_minimal(),
				ylim(range(as.numeric(as.character(model_dx[[yaxis]]))))
			)
		},

		# Catch statement
		stop("Neither a linear (lm) or logistic (glm(family = binomial)) model")
	)
}

# }}}

# Plotting Residual of a Model {{{ ====

#' @title Plotting Residual of a Model
#' @description
#' `geom_residuals` makes a diagnostic plot of residuals versus fitted data for linear models. Does not yet accept logistic models
#' @details Generate residuals versus fitted plot. Functions as an additional geom layer on ggplot. Models must be linear/gaussian in nature. Covariates can be included in the model.
#' @param model Model to be analyzed, currently only accepts linear models.
#' @return Returns a ggplot object of geom type, other layers can be added on as seen in example.
#' @examples
#' ggplot() + geom_residuals(model)
#' @export
geom_residuals <- function(model) {

	# Augment model
	model_dx <- broom::augment(model, type.predict = "response")

	# Make geom layer
	list(
		aes_string(x = ".fitted", y = ".resid"),
		geom_point(data = model_dx, aes_string(x = ".fitted", y = ".resid")),
		stat_smooth(data = model_dx, aes(x = .fitted, y = .resid), method = "loess", color = "cornflowerblue"),
		geom_hline(yintercept = 0, col = "brown4", linetype = "dashed"),
		labs(
			title = "Residual versus fitted plot",
			x = "Fitted",
			y = "Residuals"
		),
		theme_minimal()
	)
}

# }}}

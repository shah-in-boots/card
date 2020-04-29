#' @import magrittr

# Model Constructor {{{ ====
new_cosinor <- function(coefs, coef_names, blueprint) {

	# Coefs check
	if (!is.numeric(coefs)) {
		stop("`coef` should be a numeric vector.",
			 call. = FALSE)
	}

	# Names check
	if (!is.character(coef_names)) {
		stop("`coef_names` should be a character vector.",
			 call. = FALSE)
	}

	# Length check
	if(length(coefs) != length(coef_names)) {
		stop("`coefs` and `coef_names` must have same length.")
	}

	# New model
	hardhat::new_model(
		coefs = coefs,
		coef_names = coef_names,
		blueprint = blueprint,
		class = "cosinor"
	)

}

### Testing

manual_model <-
	new_cosinor(1, "my_coef", hardhat::default_xy_blueprint())

# }}}

# Model Fitting Implementation {{{ ====

# Will require certain variables
cosinor_impl <- function(predictors, outcome) {

	cosinor_fit <- lm.fit(predictors, outcomes)

	coefs <- cosinor_fit$coefficients
	coef_names <- names(coefs)
	coefs <- unname(coefs)

	list(
		coefs = coefs,
		coef_names = coef_names
	)
}

### Testing

predictors <- as.matrix(subset(iris, select = Sepal.Width))
outcomes <- iris$Sepal.Length
cosinor_impl(predictors, outcomes)

# }}}

# Model Fitting Bridge {{{ ====

cosinor_bridge <- function(processed) {

	# Outcomes should be a single column
	hardhat::validate_outcomes_are_univariate(processed$outcomes)

	predictors <- as.matrix(processed$predictors)
	outcomes <- processed$outcomes[[1]]

	fit <- cosinor_impl(predictors, outcomes)

	new_cosinor(
		coefs = fit$coefs,
		coef_names = fit$coef_names,
		blueprint = processed$blueprint
	)
}

### Testing

# Simulate formula interface
proc1 <- hardhat::mold(Sepal.Width ~ Sepal.Length + Species, iris)

# Simular xy interface
proc2 <- hardhat::mold(x = iris["Sepal.Width"], y = iris$Sepal.Width)

# Stop multiple outcomes
proc3 <- hardhat::mold(Sepal.Width + Petal.Width + Species ~ Sepal.Length, iris)

cosinor_bridge(proc1)
cosinor_bridge(proc2)
# Will run an error
#cosinor_bridge(proc3)

# }}}

# User Facing Fitting Function {{{ ====

# Generic
cosinor <- function(x, ...) {
	UseMethod("cosinor")
}

# Default
cosinor.default <- function(x, ...) {
	stop(
		"`cosinor()` is not defined for a '", class(x)[1], "'.",
		call. = FALSE
	)
}

# XY method - data frame
cosinor.data.frame <- function(x, y, intercept = TRUE, ...) {
	blueprint <- hardhat::default_xy_blueprint(intercept = intercept)
	processed <- hardhat::mold(x, y, blueprint = blueprint)
	cosinor_bridge(processed)
}

# XY method - matrix
cosinor.matrix <- function(x, y, intercept = TRUE, ...) {
	blueprint <- hardhat::default_xy_blueprint(intercept = intercept)
	processed <- hardhat::mold(x, y, blueprint = blueprint)
	cosinor_bridge(processed)
}

# Formula method
cosinor.formula <- function(formula, data, intercept = TRUE, ...) {
	blueprint <-
		hardhat::default_formula_blueprint(intercept = intercept)
	processed <- hardhat::mold(formula, data, blueprint = blueprint)
	cosinor_bridge(processed)
}

# Recipe method
cosinor.recipe <- function(x, data, intercept = TRUE, ...) {
	blueprint <-
		hardhat::default_recipe_blueprint(intercept = intercept)
	processed <- hardhat::mold(x, data)
	cosinor_bridge(processed)
}

### Testing

predictors <- iris[c("Sepal.Width", "Petal.Width")]
outcomes_vec <- iris$Sepal.Width
outcomes_df <- iris["Sepal.Length"]

# Vector outcome
cosinor(predictors, outcomes_vec)

# 1 column data frame as outcome
cosinor(predictors, outcomes_df)

# Formula interface
cosinor(Sepal.Length ~ Sepal.Width + Petal.Width, data = iris)

# Preprocessing traditionally
cosinor(Sepal.Length ~ log(Sepal.Width) + Species, iris)

# Preprocessing with recipe
rec <-
	recipes::recipe(Sepal.Length ~ Sepal.Width + Species, iris) %>%
	recipes::step_log(Sepal.Width) %>%
	recipes::step_dummy(Species, one_hot = TRUE)
cosinor(rec, data = iris)

# }}}

# Adding an Intercept Option {{{ ====

#

# }}}
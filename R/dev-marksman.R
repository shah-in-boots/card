# Marksman's Bullets {{{ ====

#' @title Create bullets to be loaded
#' @param f Formula showing relationship of outcomes to predictors
#' @param exposure Variable(s) that is forced to be maintained in every model as a
#'   predictor.
#' @param approach The modeling approach that will be used. The options are:
#'
#'   * `sequential` will build y ~ x1, y ~ x1 + x2 models
#'
#'   * `parallel` will build y ~ x1, y ~ x2 models
#'
#' @param model A model specified by the [`parsnip`
#'   package](https://parsnip.tidymodels.org/articles/parsnip_Intro.html), which
#'   includes the mode and computational engine
#' @param ... For extensibility
#' @examples
#' data(geh)
#' f <- svg_mag + qrs_tang ~ lab_hba1c + age + sex + bmi + cad + htn
#' lm_mod <- parsnip::linear_reg() %>% parsnip::set_engine("lm")
#' b <- bullet(f, exposure = "lab_hba1c", approach = "sequential", model = lm_mod)
#' @return List of hypothesis characteristics needed for analysis
#' @importFrom magrittr %>%
#' @export
bullet <- function(f, exposure = NULL, approach = "sequential", model, ...) {

	# Break apart formula
	outcomes <- all.vars(f[[2]])
	predictors <- all.vars(f[[3]])
	exposures <- exposure
	covariates <- setdiff(predictors, exposures)

	# Items that will be loaded
	shell <- list(
		outcomes = outcomes,
		predictors = predictors,
		exposures = exposures,
		covariates = covariates,
		approach = approach,
		model = model
	)

	# Constructor function being used
	shell <- new_bullet(shell)

	# Return
	return(shell)
}

#' @description Construct a new bullet, with validation built in
#' @noRd
new_bullet <- function(shell) {

	# Confirm that the "shell" is a list of bullet characteristics
	stopifnot(is.list(shell))

	# Final structure defined
	structure(shell, class = "bullet")

}

# Print Method

#' @description Generic print method
#' @param x Object of class `bullet`
#' @param ... arguments to pass on
#' @noRd
#' @export
print.bullet <- function(x, ...) {

	cat("Hypothetical bullet... \n \n")
	cat("Outcomes: ", length(x$outcomes), "\n")
	cat("Exposures: ", length(x$exposures), "\n")
	cat("Covariates: ", length(x$covariates), "\n")
	cat("Approach: ", length(x$approach), "\n")
	cat("Model: ", x$model$engine, x$model$mode, "\n")

}

# Marksman's Aim {{{ ====

#' @title Aiming For Target Hypothesis
#' @description This creates the hypothesis that are being prepared for
#'   analysis. These contain model specifications and the workflows, along with
#'   model identifiers.
#' @param bullets List object of bullets that are to be thought of as the
#'   statistical plan, as defined by each individual bullet. The list can be
#'   named to make it simpler.
#' @param ... For extensibility
#' @return List of `aims` that contain all the relevant hypothesis that were
#'   pre-specified, including workflows and model specifications from
#'   `tidymodels`
#' @examples
#' data(geh)
#' f <- svg_mag + qrs_tang ~ lab_hba1c + age + sex + bmi + cad + htn
#' lm_mod <- parsnip::linear_reg() %>% parsnip::set_engine("lm")
#' b1 <- bullet(f, exposure = "lab_hba1c", approach = "sequential", model = lm_mod)
#' b2 <- bullet(f, exposure = "lab_hba1c", approach = "parallel", model = lm_mod)
#' bullets <- list(seq = b1, par = b2)
#' @importFrom magrittr %>%
#' @export
aim <- function(bullets, ...) {

	# Check to see if this is a single list object or multiple
	if(!is(bullets, "list")) {
		stop("Please wrap the `bullets` argument as a vector of `bullet` objects).",
				 call. = FALSE)
	}

	# Function to add parts together to create workflow
	create_workflows <- function(outcomes, predictors, models) {
		workflows::workflow() %>%
			workflows::add_model(models) %>%
			workflows::add_variables(outcomes = !!outcomes, predictors = !!predictors)
	}

	# Using a map function to create new aim
	# Each object will be of hte aim class, collated into a list
	aims <-
		purrr::map(bullets, function(x) {
			ballistics(x) %>%
				dplyr::mutate(models = list(x$model)) %>%
				dplyr::mutate(
					raw = purrr::pmap(list(outcomes, predictors, models), create_workflows)
				)
		}) %>%
		purrr::map(., new_aim)

	# Return
	return(aims)

}

#' @description Construct a new aim, with validation built in
#' @noRd
new_aim <- function(ammo) {

	# Confirm that the "ammo" is a table in an aims format
	stopifnot(is.data.frame(ammo))

	# Final structure defined
	structure(ammo, class = c(class(ammo), "aim"))

}

#' @description Generic print method
#' @param x List objects of class `aim`
#' @param ... arguments to pass on
#' @noRd
#' @export
print.aim <- function(x, ...) {

	# Intro
	cat("Specific Aims: \n \n")

	cat("There are", length(x), "specific aims that are being modeled.")
}

#' @title Firing using the Aims at Target Hypotheses
#' @description As the analysis time can be quite long, this function separates
#'   out model runtime until after the workflows for analysis are completed.
#'   This function works on a single `aim` object at a time, allowing for list
#'   methods such as [purrr::map] to apply.
#' @param aim A single `aim` object
#' @param data Data frame that contains the variables that were planned for in
#'   the specific aims generated prior.
#' @param ... For extensibility
#' @return Returns the fitted models in an expanded table, still of the `aim` class
#' @importFrom magrittr %>%
#' @export
fire <- function(aim, data, ...) {

	# Check correct aims
	if(!is(aim, "aim")) {
		stop("Please use an `aim` object.", call. = FAlSE)
	}

	# Fire!
	# Create a final target made from the single aim
	# Temporarily take away the `aim` class so dplyr will work
	target <-
		aims$seq %>%
		tibble::as_tibble() %>%
		dplyr::mutate(proc = purrr::map(raw, parsnip::fit, data = data)) %>%
		new_aim()

	# Return
	return(target)

}

# }}}

# Marksman's Tools {{{ ====

#' @title Ballistic Analysis of the Hypotheses
#' @description Look at the overall characteristics of the hypotheses, such as
#'   number of regression models and how they will be put together based on the
#'   approach. Is used to evaluate the objects created by `bullet()` and by
#'   `aim()` (which also uses ballistics to generate the appropriate modeling
#'   matrix).
#' @param mark Represents either a `bullet` or `aim` object to be analyzed for
#'   further information or characteristics
#'
#'   - A single __bullet__ object will return a table that is structured for the
#'   proposed analytical approach
#'
#'   - A single __aim__ object, after the `fire()` function, will return a table
#'   that includes further analytical elements. Currently returns the runtime in
#'   miliseconds.
#'
#' @param ... For extensibility
#' @return The characteristics needed to create an appropriate modeling matrix when using the `aim()` function. Returns a tibble of the appropriate combination of outcomes/predictors.
#' @examples
#' data(geh)
#' f <- svg_mag + qrs_tang ~ lab_hba1c + age + sex + bmi + cad + htn
#' lm_mod <- parsnip::linear_reg() %>% parsnip::set_engine("lm")
#' b <- bullet(f, exposure = "lab_hba1c", approach = "parallel", model = lm_mod)
#' ballistics(b)
#' @family ballistics
#' @export
ballistics <- function(mark, ...) {
	UseMethod("ballistics")
}

#' @export
#' @rdname ballistics
ballistics.default <- function(mark, ...) {
  stop("`ballistics()` is not defined for a '", class(mark)[1], "'.", call. = FALSE)
}

#' @export
#' @rdname ballistics
ballistics.bullet <- function(mark, ...) {

	# This is for a bullet
	bullet <- mark

	# Major variables
	out <- bullet$outcomes
	exp <- bullet$exposures
	covar <- bullet$covariates
	approach <- bullet$approach
	nc <- length(bullet$covariates)

	# Based on approach
	switch(
		approach,
		sequential = {
			tbl <-
				tibble::tibble(model_num = 1:nc) %>%
				dplyr::mutate(predictors = purrr::map(model_num, ~ c(exp, covar[1:.]))) %>%
				tidyr::expand_grid(outcomes = out, .)
		},
		parallel = {
			tbl <-
				tibble::tibble(model_num = 1:nc) %>%
				dplyr::mutate(predictors = purrr::map(model_num, ~ c(exp, covar[.]))) %>%
				tidyr::expand_grid(outcomes = out, .)
		}
	)

	# Return
	return(tbl)

}

#' @export
#' @rdname ballistics
ballistics.aim <- function(mark, ...) {

	# Using aims instead
	aim <- mark

	# Identify if that has been run or not
	fired <- "proc" %in% names(aim)

	# Add run-time
	if (fired) {
		aim <-
			aim %>%
			tibble::as_tibble() %>%
			dplyr::mutate(runtime = purrr::map_dbl(proc, function(x) {
				signif(1000 * (x$fit$fit$elapsed[1] + x$fit$fit$elapsed[3]), digits = 2)
			}))
	} else {
		message("To check the `aim` it has to be `fired` first.")
	}

	return(aim)
}

#' @title Reload either the bullets or the aims
#' @description If, after an `aim` or `bullet` object has been made, it can be
#'   revised or reloaded with new characteristics
#' @param mark Represents either a `bullet` or `aim` object to be analyzed for
#'   further information or characteristics
#'
#'   - A single __bullet__ object.
#'
#'   - A single __aim__ object. If it has been processed by `fire()`, then that
#'   component will be removed. Remember, this exists as one of the named lists
#'   created from the `aim()` function.
#'
#' @param ... Any original parameter from `aim` or `bullet` can be
#'   added/revised. Both require calling the name of the element to be revised.
#'   The function will assess the names and update accordingly. Only a single
#'   element can be modified at a time.
#'
#'   -  If an __aim__ object: c("outcomes", "model_num", "predictors", "models",
#'   "raw")
#'
#'   - If a __bullet__ object: c("outcomes", "predictors", "exposures",
#'   "covariates", "approach", "model"), e.g. `reload(bullet, outcomes =
#'   "new_outcome")`
#'
#' @family reload
#' @return Returns the original `aim` or `bullet` with revised components.
#' @export
reload <- function(mark, ...) {
	UseMethod("reload")
}

#' @export
#' @rdname reload
reload.default <- function(mark, ...) {
  stop("`reload()` is not defined for a '", class(mark)[1], "'.", call. = FALSE)
}

#' @export
#' @rdname reload
reload.bullet <- function(mark, ...) {
	# If bullet
	bullet <- mark
	old_terms <- names(bullet)

	# which terms to update
	new_arg <- list(...)
	new_terms <- names(new_arg)

	# Make sure the term matches
	change <- intersect(new_terms, old_terms)
	bullet[[change]] <- new_arg

	# Return
	return(bullet)
}

#' @export
#' @rdname reload
reload.aim <- function(mark, ...) {

	# If an aim
	# Drop the processed column since its being reloaded
	aim <- mark
	aim <- aim[, -which(names(aim) %in% "proc")]
	old_terms <- names(aim)

	# Which terms to update
	new_arg <- list(...)
	new_terms <- names(new_arg)

	# Make sure the term matches and is compatible
	if(length(aim[[new_terms]]) != length(new_arg[[new_terms]]) | length(intersect(old_terms, new_terms)) != 1) {
		stop("The new data given to `reload` is either not the same length or does not match with an existing aim term.", call. = FALSE)
	}

	# Update and return
	aim[new_terms] <- new_arg
	return(bullet)
}

# }}}
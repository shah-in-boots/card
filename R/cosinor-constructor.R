# Cosinor Regression {{{ ====

#' @title Fit a `cosinor`
#'
#' @description `cosinor()` fits a regression model of a time variable to a
#'   continuous outcome use trigonometric features. This approaches uses the
#'   linearization of the parameters to assess their statistics and
#'   distribution.
#'
#' @param t Represents the _ordered_ time indices that provide the positions for the
#'   cosine wave. Depending on the context:
#'
#'   - A __data frame__ of a time-based predictor/index.
#'
#'   - A __matrix__ of time-based predictor/index.
#'
#'   - A __recipe__ specifying a set of preprocessing steps
#'     created from [recipes::recipe()].
#'
#' @param y When `t` is a __data frame__ or __matrix__, `y` is the outcome
#' specified as:
#'
#'   - A __data frame__ with 1 numeric column.
#'
#'   - A __matrix__ with 1 numeric column.
#'
#'   - A numeric __vector__.
#'
#' @param data When a __recipe__ or __formula__ is used, `data` is specified as:
#'
#'   - A __data frame__ containing both the predictors and the outcome.
#'
#' @param formula A formula specifying the outcome terms on the left-hand side,
#' and the predictor terms on the right-hand side.
#'
#' @param tau A vector that determines the periodicity of the time index. The
#'   number of elements in the vector determine the number of components (e.g.
#'   single versus multiple cosinor).
#'
#'   - A __vector__ with a single element = single-component cosinor, e.g.
#'   period = c(24)
#'   - A __vector__ with multiple elements = multiple-component
#'   cosinor, e.g. period = c(24, 12)
#'
#' @param population Represents the population to be analyzed with a
#'   population-mean cosinor. Defaults to NULL, assuming individual cosinors are
#'   being generated. When a __recipe__ or __formula__ is used, `population` is
#'   specified as:
#'
#'   - A __character__ name of the column contained in `data` that contains
#'   identifiers for each subject. Every row will have a subject name which
#'   should be duplicated for each time index given.
#'
#'   When a __data frame__ or __matrix__ is used, `population` is specified as:
#'
#'   - A __vector__ of the same length as `t`, with values representing each
#'   subject at the correct indices.
#'
#' @param ... Not currently used, but required for extensibility.
#'
#' @return A `cosinor` object.
#'
#' @examples
#' # Data setup
#' data("twins")
#'
#' # Formula interface
#' model <- cosinor(rDYX ~ hour, twins, tau = 24)
#'
#' @family cosinor
#' @export
cosinor <- function(t, ...) {
  UseMethod("cosinor")
}

# Cosinor Methods {{{ ====

## Default method

#' @export
#' @rdname cosinor
cosinor.default <- function(t, ...) {
  stop("`cosinor()` is not defined for a '", class(t)[1], "'.", call. = FALSE)
}

## XY method - data frame

#' @export
#' @rdname cosinor
cosinor.data.frame <- function(t, y, tau, population = NULL, ...) {
  processed <- hardhat::mold(t, y)
  cosinor_bridge(processed, tau, population, data = NULL, ...)
}

## XY method - matrix

#' @export
#' @rdname cosinor
cosinor.matrix <- function(t, y, tau, population = NULL, ...) {
  processed <- hardhat::mold(t, y)
  cosinor_bridge(processed, tau, population, data = NULL, ...)
}

## Formula method - stable, works

#' @export
#' @rdname cosinor
cosinor.formula <- function(formula, data, tau, population = NULL, ...) {
  processed <- hardhat::mold(formula, data)
  if (is.character(population)) {
    population <- data[[population]]
  }
  cosinor_bridge(processed, tau, population, data, ...)
}

## Recipe method - unstable

#' @export
#' @rdname cosinor
cosinor.recipe <- function(t, data, tau, population = NULL, ...) {
  processed <- hardhat::mold(t, data)
  if (is.character(population)) {
    population <- data[[population]]
  }
  cosinor_bridge(processed, tau, population, data, ...)
}

# }}}

# Cosinor Construction {{{ ====

## Bridging Function

#' @description Bridging function takes user-facing call, after it is processed,
#'   and moves it to `cosinor_bridge`, which then calls both `cosinor_impl`, the
#'   fitting algorithm, and `new_cosinor`, the constructor for a new type of S3
#'   class. This also bridges to population-mean cosinor implementation if
#'   needed.
#' @noRd
cosinor_bridge <- function(processed, tau, population, data, ...) {

  ### Create call ====
    # Formal equation
    # y(t) = M + A*cos(2*pi*t/period + phi)
    # y(t) = M + beta*x + gamma*z + error(t)
  y <- names(processed$outcomes)
  t <- names(processed$predictors)
  l <- length(tau)
  ls <- list()

  for (i in 1:l) {
    ls[[i]] <- paste0("A", i, " * cos(2*pi*", t, "/", tau[i], " + phi", i,")")
  }
  f <- paste0(y, " ~ M + ", paste0(ls, collapse = " + "))
  call <- paste0("cosinor(formula = ", f)

  ### Model fit ====

  # Check and format predictors
  hardhat::validate_predictors_are_numeric(processed$predictors)
  predictors <- processed$predictors[[1]]

  # Check and format outcomes
  hardhat::validate_outcomes_are_univariate(processed$outcomes)
  hardhat::validate_outcomes_are_numeric(processed$outcomes)
  outcomes <- processed$outcomes[[1]]

  # If population value is NULL, then perform individual cosinor
  if (is.null(population)) {

    # Implemented function for single and multiple component cosinor
    fit <- cosinor_impl(predictors, outcomes, tau)
    type <- "Individual"

  } else if (length(population) == length(predictors)) {

    # Modified function, usings cosinor_impl internally
    fit <- cosinor_pop_impl(predictors, outcomes, tau, population)
    type <- "Population"

  } else {

    # Error if population cosinor cannot be run either
    stop("Population-mean cosinor error: `population` does not match size of time indices", call. = FALSE)

  }

  ## New Cosinor ====

  # Constructor function receives from implemented function
  new_cosinor(
    coefficients = fit$coefficients,
    coef_names = fit$coef_names,
    fitted.values = fit$fitted.values,
    residuals = fit$residuals,
    call = call, # Made with bridge data
    tau = tau,
    model = fit$model,
    xmat = fit$xmat,
    type = type, # Made at bridge, labels type of cosinor object
    blueprint = processed$blueprint # Made from hardhat, not from fit
  )
}

## Constructor function

#' @description Accepts the output from the fit of the implemented model. Makes a new S3 class with the correct structure. Checks output from model for coherence.
#' @noRd
new_cosinor <- function(
  coefficients,
  coef_names,
  fitted.values,
  residuals,
  call,
  tau,
  model,
  xmat,
  type,
  blueprint
) {

  # Can validate coefs here
  if (!is.numeric(coefficients)) {
    stop("`coefficients` should be a numeric vector.",
      call. = FALSE
    )
  }

  # Names check
  if (!is.character(coef_names)) {
    stop("`coef_names` should be a character vector.",
      call. = FALSE
    )
  }

  # Length check
  if (length(coefficients) != length(coef_names)) {
    stop("`coefficients` and `coef_names` must have same length.")
  }

  # Fit outputs need to match here
  hardhat::new_model(
    coefficients = coefficients,
    coef_names = coef_names,
    fitted.values = fitted.values,
    residuals = residuals,
    call = call,
    tau = tau,
    model = model,
    xmat = xmat,
    type = type,
    blueprint = blueprint,
    class = "cosinor"
  )
}

# }}}

# Cosinor Parsnip Methods {{{ ====

# Wrapper function to load parsnip model
make_cosinor_reg <- function() {

	# Check to see if already loaded
	current <- parsnip::get_model_env()

	# If not loaded, then set up model
	if(!any(current$models == "cosinor_reg")) {

		# Start making new model
		parsnip::set_new_model("cosinor_reg")

		# Add parsnip models to another package
		parsnip::set_model_mode(model = "cosinor_reg", mode = "regression")
		parsnip::set_model_engine("cosinor_reg", mode = "regression", eng = "card")
		parsnip::set_dependency("cosinor_reg", eng = "card", pkg = "card")

		# Arguments
		parsnip::set_model_arg(
			model = "cosinor_reg",
			eng = "card",
			parsnip = "period",
			original = "tau",
			func = list(pkg = "card", fun = "cosinor"),
			has_submodel = FALSE
		)

		# Fit
		parsnip::set_fit(
			model = "cosinor_reg",
			eng = "card",
			mode = "regression",
			value = list(
				interface = "formula",
				protect = c("formula", "data"),
				func = c(pkg = "card", fun = "cosinor"),
				defaults = list()
			)
		)

		# Prediction
		parsnip::set_pred(
			model = "cosinor_reg",
			eng = "card",
			mode = "regression",
			type = "numeric",
			value = list(
				pre = NULL,
				post = NULL,
				func = c(fun = "predict"),
				args = list(
					object = quote(object$fit),
					new_data = quote(new_data),
					type = "numeric"
				)
			)
		)
	}

}


#' @title General Interface for Cosinor Regression Models
#' @description `cosinor_reg()` is a _parsnip_ friendly method for specification of cosinor regression model before fitting.
#' @param mode A character string that describes the type of model. In this case, it only supports type of "regression".
#' @param period A non-negative number or vector of numbers that represent the expected periodicity of the data to be analyzed.
#' @examples
#' library(parsnip)
#' data(twins)
#' cosinor_reg(period = 24) %>%
#'   set_engine("card") %>%
#'   fit(rDYX ~ hour, data = twins)
#' @export
cosinor_reg <- function(mode = "regression", period = NULL) {

	# Check correct mode
	if(mode != "regression") {
		stop("`mode` should be 'regression'", call. = FALSE)
	}

	# Capture arguments
	args <- list(period = rlang::enquo(period))

	# Model specs / slots
	parsnip::new_model_spec(
		"cosinor_reg",
		args = args,
		mode = mode,
		eng_args = NULL,
		method = NULL,
		engine = NULL
	)
}

#' @param object Cosinor model specification
#' @param ... Not used for `update()`
#' @param fresh A logical for whether the arguments should be modified in place or replaced altogether
#' @method update cosinor_reg
#' @rdname cosinor_reg
#' @export
update.cosinor_reg <- function(object, period = NULL, fresh = FALSE, ...) {
	parsnip::update_dot_check(...)

	# Updated arguments
	args <- list(
		period = rlang::enquo(period)
	)

	if (fresh) {
		object$args <- args
	} else {
		null_args <- purrr::map_lgl(args, parsnip::null_value)
		if (any(null_args))
			args <- args[!null_args]
		if (length(args) > 0)
			object$args[names(args)] <- args
	}

	# Model specs / slots
	parsnip::new_model_spec(
		"cosinor_reg",
		args = object$args,
		eng_args = object$eng_args,
		mode = object$mode,
		method = NULL,
		engine = object$engine
	)
}

#' @method print cosinor_reg
#' @rdname cosinor_reg
#' @param x Cosinor model specification
#' @param ... Extensible
#' @export
print.cosinor_reg <- function(x, ...) {
	cat("Cosinor Model Specification (", x$mode, ")\n\n", sep = "")
	parsnip::model_printer(x, ...)

	if (!is.null(x$method$fit$args)) {
		cat("Model fit template:\n")
		print(parsnip::show_call(x))
	}

	invisible(x)
}

# }}}

# Cosinor Generic S3 Methods {{{ ====

## Print Method

#' @description Generic print method
#' @param x Model of class `cosinor`
#' @param ... arguments to pass on
#' @noRd
#' @export
print.cosinor <- function(x, ...) {

	cat("Call: \n")
	cat(x$call, "\n")

	# Coefficients
	cat("\n")
	cat("Coefficients: \n")
	names(x$coefficients) <- x$coef_names
	print(x$coefficients)

}

## Summary Method

#' @description Generic summary method
#' @param object Model of class `cosinor`
#' @param ... arguments to pass on
#' @noRd
#' @export
summary.cosinor <- function(object, ...) {

	# Summary
	cat(paste0(object$type, " Cosinor Model \n"))
	cat(strrep("-", 42))

	# Call
	cat("\n")
	cat("Call: \n")
	cat(object$call, "\n")

	# Periods
	cat("\n")
	cat("Period(s): ")
	cat(paste0(object$tau, collapse = ", "), "\n")

	# Residuals
	cat("\n")
	cat("Residuals: \n")
	print(summary(object$residuals))

	# Coefficients (estimate, SE, t.value, P.value)
	cat("\n")
	cat("Coefficients: \n")
	names(object$coefficients) <- object$coef_names
	coefs <- object$coefficients
  coefs <- coefs[grep("mesor|amp|phi", names(coefs))]
  se <- stats::confint(object)$se
	l <- list(coefs = coefs, se = se)

	mat <- do.call(cbind, lapply(l, function(x) {x[match(names(l[[1]]), names(x))]}))
	colnames(mat) <- c("Estimate", "Std. Error")
	print(mat)

}

#' @description Generic plot method
#' @param x `cosinor` object
#' @param ... For extensibility
#' @noRd
#' @export
plot.cosinor <- function(x, ...) {

	# Model data
	model <- as.data.frame(x$model)
	model$yhat <- x$fitted.values
	model$res <- x$residuals

	# Plotting function
	plot(model$t, model$yhat)
}


# }}}

# Cosinor Tidiers {{{ ====

## Tidy Method

#' @importFrom generics tidy
#' @export
generics::tidy

#' @title Tidy a(n) cosinor object
#' @description Tidy summarizes information about the components of a `cosinor`
#'   model.
#' @details `cosinor` objects do not necessarily have a T-statistic as the
#'   standard error is not based on a mean value, but form a joint-confidence
#'   interval. The standard error is generated using Taylor series expansion as
#'   the object is a subspecies of harmonic regressions.
#' @param x A `cosinor` object created by [card::cosinor()]
#' @param conf.int Logical indicating whether or not to include confidence
#'   interval in tidied output
#' @param conf.level The confidence level to use if `conf.int = TRUE`. Must be
#'   between 0 and 1, with default to 0.95 (the 95% confidence interval).
#' @param ... For extensibility
#' @return a `tibble` object
#' @export
tidy.cosinor <- function(x, conf.int = FALSE, conf.level = 0.95, ...) {

  # Get base data
  names(x$coefficients) <- x$coef_names
	coefs <- x$coefficients
  coefs <- coefs[grep("mesor|amp|phi", names(coefs))]
  val <- stats::confint(x, level = conf.level)
	l <- list(coefs = coefs, se = val$se)
	mat <- do.call(cbind, lapply(l, function(x) {x[match(names(l[[1]]), names(x))]}))

	# Tibble it
	result <-
	  mat %>%
	  dplyr::as_tibble(rownames = "term") %>%
	  dplyr::rename(estimate = coefs, std.error = se)

	if (conf.int) {

		ci <- val$ci
		colnames(ci) <- c("conf.low", "conf.high")
		result <-
			ci %>%
	    dplyr::as_tibble(rownames = "term") %>%
	    dplyr::left_join(result, ., by = "term")

	}

	# Return findings
	result

}

## Augment Method

#' @importFrom generics augment
#' @export
generics::augment

#' @title Augment data with information from a `cosinor` object
#' @description Augment accepts a `cosinor` model object and adds information about each observation in the dataset. This includes the predicted values in the `.fitted` column and the residuals in the `.resid` column. New columns always begin with a `.` prefix to avoid overwriting columns in original dataset.
#' @param x A `cosinor` object created by [card::cosinor()]
#' @param ... For extensibility
#' @return a `tibble` object
#' @export
#' @family cosinor
augment.cosinor <- function(x, ...) {

  # Add fitted and residual values
  result <-
    dplyr::bind_cols(
    dplyr::tibble(x$model),
    dplyr::tibble(.fitted = x$fitted.values),
    dplyr::tibble(.resid = x$residuals)
  )

  # Return
  return(result)

}

# }}}


# Cosinor Plots {{{ ====

## Confidence Region by Ellipse Method

#' @title Graphical Assessment of Amplitude and Acrophase
#' @description This is a ggplot-styled graphical representation of the ellipse
#'   region generated by the cosinor analysis. It requires the same data used by
#'   cosinor model to be fit with the model [card::cosinor]. This includes
#'   the amplitude, acrophase,
#' @param object Requires a cosinor model to extract the correct statistics to
#'   generate the plot.
#' @param level Confidence level for ellipse
#' @param ... Additional parameters may be needed for extensibility
#' @examples
#' data("twins")
#' m <- cosinor(rDYX ~ hour, twins, tau = 24)
#' ggellipse(m)
#' @return Object of class `ggplot` to help identify confidence intervals
#' @import ggplot2
#' @export
ggellipse <- function(object, level = 0.95, ...) {

	if(object$type == "Population") {
		message("Ellipse may not be accurate for population-mean cosinor method.")
	}

	# Area
	area <- cosinor_area(object, level = level)$area
  gseq <- area[, "gseq"]
  bs1 <- area[, "bs1"]
  bs2 <- area[, "bs2"]

	# Confidence level
	a <- 1 - level

	# Parameters
	y <- object$model[,"y"]
	t <- object$model[,"t"]
	n <- length(t)
	p <- length(object$tau)

  for(i in 1:p) {
    assign(paste0("x", i), object$model[, paste0("x", i)])
    assign(paste0("z", i), object$model[, paste0("z", i)])
  }

	xmat <- object$xmat
	yhat <- object$fitted.values
	coefs <- object$coefficients
	names(coefs) <- object$coef_names
  for(i in 1:length(coefs)) {
    assign(names(coefs)[i], unname(coefs[i]))
	}

  # Necessary values for the plot
  theta_clock <- seq(0, 2 * pi, length.out = 24^2)
  clock <- cbind(2 * amp1 * cos(theta_clock), 2 * amp1 * sin(theta_clock))
  rad <- seq(0, 2 * pi - pi / 4, by = pi / 4)
  rad_clock <- cbind(2.2 * amp1 * cos(rad), 2.2 * amp1 * sin(rad))

  # GGplot the values
  ggplot() +
  	# Ellipse
  	geom_line(aes(x = gseq, y = bs1), col = "goldenrod", size = 1) +
  	geom_line(aes(x = gseq, y = bs2), col = "goldenrod", size = 1) +
  	# Line from origin to ellipse
  	geom_line(aes(x = c(0, gamma1),
  								y = c(0, beta1)),
  						lty = 1,
  						size = 1,
  						col = "black") +
  	# Line from ellipse to circumference
  	geom_line(aes(
  		x = c(gamma1, -2 * amp1 * sin(phi1)),
  		y = c(beta1, 2 * amp1 * cos(phi1)),
  		group = 0
  	),
  	size = 1,
  	col = "black",
  	lty = 3) +
  	# Axes
  	geom_line(aes(x = c(0, 0), y = c(-2 * amp1, 2 * amp1)),
  						lty = 5, col = "grey") +
  	geom_line(aes(y = c(0, 0), x = c(-2 * amp1, 2 * amp1)),
  						lty = 5, col = "grey") +
  	# "Clock" shape to help show degrees on unit circle
  	geom_path(aes(x = clock[, 1], y = clock[, 2]), col = "cornflowerblue") +
  	annotate(
  		geom = "text",
  		x = rad_clock[, 1],
  		y = rad_clock[, 2],
  		label = paste(rad * 180 / pi, "\u00B0")
  	) +
  	# Labels
  	xlab(expression(paste(gamma1))) +
  	ylab(expression(paste(beta1))) +
  	xlim(-2.5 * amp1, 2.5 * amp1) +
  	ylim(-2.5 * amp1, 2.5 * amp1) +
  	theme_minimal()
}

#' @title ggplot of cosinor model
#' @description ggplot of cosinor model that can visualize a variety of cosinor
#'   model subtypes, including single-component, multiple-component, individual,
#'   and population cosinor models, built using [card::cosinor]. For single
#'   component cosinor, the following values are plotted:
#'
#'   * M = midline estimating statistic of rhythm
#'
#'   * A = amplitude
#'
#'   * P = phi or acrophase (shift from 0 to peak)
#'
#'   If using a multiple-component cosinor, the terms are different. If the
#'   periods or frequencies resonate or are harmonic, then the following are
#'   calculated. If the periods are not harmonic, the values are just
#'   descriptors of the curve.
#'
#'   * M = midline estimating statistic of rhythm
#'
#'   * Ag = global amplitude, which is the distance between peak and trough
#'   (this is the same value as the amplitude from single component)
#'
#'   * Po = orthophase (the equivalent of the acrophase in a single component),
#'   the lag time to peak value
#'
#'   * Pb = bathyphase, the lag time to trough value
#'
#' @param object Model of class `cosinor`. If instead of a single cosinor model,
#'   multiple objects are to be plotted, can provide a list of cosinor models.
#'   Plotting multiple models simultaneously is preferred if the outcome
#'   variable is similar in scale.
#' @param labels Logical value if annotations should be placed on plot, default
#'   = TRUE. The labels depend on the type of plot. The labels are attempted to
#'   be placed "smartly" using the [ggrepel::geom_label_repel()] function.
#' @param ... For extensibility. This function will use different
#'   implementations based on the type of model (single or multiple component).
#'   Attributes of the object will be passed down, or calculated on the fly.
#' @return Object of class `ggplot` that can be layered
#' @examples
#' \donttest{
#' data(triplets)
#' m1 <- cosinor(rDYX ~ hour, twins, tau = 24)
#' m2 <- cosinor(rDYX ~ hour, twins, tau = c(24, 12))
#' ggcosinor(m1, labels = FALSE)
#' ggcosinor(m2)
#' ggcosinor(list(single = m1, multiple = m2))
#' }
#' @import ggplot2
#' @family cosinor
#' @export
ggcosinor <- function(object, labels = TRUE, ...) {

	# Check to make sure just single object
	if("cosinor" %in% class(object)) {
		# Model basic data
		tau <- object$tau
		p <- length(tau) # Single or multicomponent
		type <- dplyr::case_when(
			p == 1 & object$type == "Individual" ~ "scomp",
			p > 1 & object$type == "Individual" ~ "mcomp",
		)
			if(p == 1 & object$type == "Individual") {
				"single"
			} else if(p > 1 & object$type == "Individual") {
				"multiple"
			} else if(object$type == "Population") {
				stop("`ggcosinor` does not currently support plotting population cosinor models.", call. = FALSE)
			}
	} else if(is.vector(object) & "cosinor" %in% class(object[[1]])) {
		gg <- ggmulticosinor(object, labels)
		return(gg)
	} else {
		stop("Cannot determine if model is cosinor object.", call. = FALSE)
	}

	# Identify which function to call
	aug <- augment(object)

	# Coefficients (include amplitudes)
	coefs <- object$coefficients
	names(coefs) <- object$coef_names
  for(i in 1:length(coefs)) {
    assign(names(coefs)[i], unname(coefs[i]))
  }

	# Acrophases
	for(i in 1:p) {
		assign(paste0("acro", i), abs((get(paste0("phi", i)) * tau[i]) / (2 * pi)))
	}

	## Common geom mappings
	# Amplitude
	amp <- list()
	for(i in 1:p) {
		amp[[i]] <-
			geom_segment(
			aes(
				x = !! get(paste0("acro", i)),
				xend = !! get(paste0("acro", i)),
				y = !! mesor,
				yend = !! (mesor + get(paste0("amp", i)))
			),
			linetype = "twodash", lineend = "butt", linejoin = "mitre",
		)
	}

	# Acrophases
	acro <- list()
	for(i in 1:p) {
		acro[[i]] <-
			geom_segment(
			aes(
				x = 0,
				xend = !! get(paste0("acro", i)),
				y = !! (mesor + get(paste0("amp", i))),
				yend = !! (mesor + get(paste0("amp", i)))
			),
			linetype = "twodash", lineend = "butt", linejoin = "mitre",
		)
	}

	## Data points / labels

	# Features that may be needed
	features <- cosinor_features(object)
	orthophase <- features$orthophase
	bathyphase <- features$bathyphase
	peak <- features$peak
	trough <- features$trough
	zero <- min(aug$t)
	mid <- mean(aug$t)

	glabs <-
		dplyr::bind_cols(
		term = c("M", paste0("A", 1:p), paste0("P", 1:p), "Po", "Pb"),
		value = c(
			mesor,
			unlist(mget(paste0("amp", 1:p))),
			unlist(mget(paste0("acro", 1:p))),
			orthophase,
			bathyphase
		),
		x = c(
			zero,
			unlist(mget(paste0("acro", 1:p))),
			(zero + unlist(mget(paste0("acro", 1:p))))/2,
			orthophase,
			bathyphase
		),
		y = c(
			mesor,
			mesor + unlist(mget(paste0("amp", 1:p)))/2,
			unlist(mget(paste0("amp", 1:p))) + mesor,
			peak,
			trough
		)
	)

	# Basic plot
	g <- ggplot() +
		stat_smooth(
			aes(x = t, y = .fitted), data = aug,
			method = "gam", color = "black", size = 1.2
		) +
		geom_hline(
			yintercept = mesor,
			color = "grey"
		) +
		geom_vline(
			xintercept = orthophase,
			color = "grey"
		)


	# Eventually add in residuals
	gres <- list(
		geom_segment(
			aes(x = t, xend = t, y = y, yend = .fitted), data = aug,
			alpha = 0.3
		),
		geom_point(
			aes(x = t, y = y, colour = abs(.resid), size = abs(.resid)), data = aug
		),
		scale_color_viridis_c(option = "magma")
	)

	# Switch to correct function
	switch(
		type,
		scomp = {

			# For single component, just peak and trough values
			glabs$term[glabs$term == "Po"] <- "Peak"
			glabs$term[glabs$term == "Pb"] <- "Trough"
			glabs$value[glabs$term == "Peak"] <- glabs$y[glabs$term == "Peak"]
			glabs$value[glabs$term == "Trough"] <- glabs$y[glabs$term == "Trough"]

			# Labels if needed
			gl <- unlist(list(

				# Amplitude and Acrophase
				amp,
				acro,

				# Peak and trough
				geom_point(
					aes(x = orthophase, y = peak),
					shape = 18, size = 5
				),
				geom_point(
					aes(x = bathyphase, y = trough),
					shape = 18, size = 5
				),

				# All labeling points from "glabs" created above
				geom_point(aes(x = x, y = y), glabs, alpha = 0),

				# Repelling labels
				ggrepel::geom_label_repel(
					aes(
						x = x, y = y, label = paste0(term, " = ", round(value, 3))
					),
					data = glabs,
					label.size = NA, label.r = 0.25, label.padding = 0.25,
					force = 20,
					nudge_y =
						ifelse(
							glabs$term == "P1", 0.01 * mesor,
							ifelse(glabs$term %in% c("M", "Trough"), -0.01 * mesor, 0)
						),
					nudge_x =
						ifelse(
							glabs$term %in% c("M", "A1"), 0.10 * mid,
							ifelse(glabs$term == "Peak", 0.20 * mid, 0)
						),
					segment.color = "transparent"
				)
			))

			if(labels) {g <- g + gl} else {g <- g}
		},

		mcomp = {

			gl <- list(

				# Mesor
				geom_text(
					aes(x = 2*zero, y = 1.01*mesor),
					label = paste0("M = ", round(mesor, 3))
				),

				# Orthophase
				geom_segment(
					aes(x = zero, xend = orthophase, y = peak, yend = peak),
					linetype = "dotted", size = 0.5
				),
				geom_text(
					aes(x = (orthophase - zero) / 2, y = peak + 0.01*mesor),
					label = paste0("Po = ", round(orthophase, 3))
				),

				# Bathyphase
				geom_vline(xintercept = bathyphase, color = "grey"),
				geom_segment(
					aes(x = zero, xend = bathyphase, y = trough, yend = trough),
					linetype = "dotted", size = 0.5
				),
				geom_text(
					aes(x = (bathyphase + zero) / 2, y = trough - 0.01*mesor),
					label = paste0("Pb = ", round(bathyphase, 3))
				),

				# Global Amplitude
				geom_segment(
					aes(
						x = orthophase, xend = (orthophase + bathyphase)/2,
						y = peak, yend = peak
					),
					linetype = "twodash", size = 0.5
				),
				geom_segment(
					aes(
						x = bathyphase, xend = (orthophase + bathyphase)/2,
						y = trough, yend = trough
					),
					linetype = "twodash", size = 0.5
				),
				geom_segment(
					aes(
						x = (orthophase + bathyphase)/2, xend = (orthophase + bathyphase)/2,
						y = mesor, yend = trough
					),
					linetype = "twodash", size = 0.5,
					arrow = arrow(type = "closed", length = unit(0.02, "npc"))
				),
				geom_segment(
					aes(
						x = (orthophase + bathyphase)/2, xend = (orthophase + bathyphase)/2,
						y = mesor, yend = peak
					),
					linetype = "twodash", size = 0.5,
					arrow = arrow(type = "closed", length = unit(0.02, "npc"))
				),
				geom_text(
					aes(x = (bathyphase + orthophase)/2 + max(aug$t)/4, y = 1.01*mesor),
					label = paste0("Ag = ", round((peak - trough) / 2), 3)
				)
			)

			# Return plot
			if(labels) {g <- g + gl} else {g <- g}
		}
	)

	# Dress up output
	gg <-
		g +
		theme_minimal() +
		theme(
			legend.position = "none",
			panel.grid.major = element_blank(),
			panel.grid.minor = element_blank()
		)

	# Return
	return(gg)

}

#' @noRd
#' @family cosinor
ggmulticosinor <- function(object, labels, ...) {

	# Number of cosinor objects
	n <- length(object)
	# Can be character vector or NULL
	objNames <- if(is.null(names(object))) {
		c(1:n)
	} else {
		names(object)
	}

	# Augmented data of all types
	aug <- list()
	features <- list()
	mesor <- list()
	for(i in 1:n) {
		aug[[i]] <-
			augment(object[[i]])[c("y", "t", ".fitted", ".resid")]
		features[[i]] <- cosinor_features(object[[i]])
		mesor[[i]] <- object[[i]]$coefficients[1]
	}

	# Merge together
	aug <- dplyr::bind_rows(aug, .id = ".id")
	features <-
		dplyr::bind_rows(features, .id = ".id") %>%
		tibble::add_column(mesor = unlist(mesor))

	# Plot
	g <- ggplot() +
		stat_smooth(
			aes(x = t, y = .fitted, colour = .id), data = aug,
			method = "gam", size = 1.2
		) +
		geom_hline(
			aes(yintercept = mesor, colour = .id), data = features, alpha = 0.5
		) +
		geom_vline(
			aes(xintercept = orthophase, colour = .id), data = features, alpha = 0.5
		) +
		geom_vline(
			aes(xintercept = bathyphase, colour = .id), data = features, alpha = 0.5
		) +
		scale_color_viridis_d(
			option = "plasma", end = 0.8,
			name = "Objects",
			labels = objNames
		) +
		theme_minimal()

	# Labels
	gl <- list(

		# Mesor
		ggrepel::geom_label_repel(
			aes(
				x = min(aug$t), y = mesor, colour = .id,
				label = paste0("MESOR = ", round(mesor, 2))
			),
			data = features,
			show.legend = FALSE,
			label.size = NA, label.r = 0.25, label.padding = 0.25,
			force = 10,
			segment.color = "transparent"
		),

		# Peaks
		geom_point(
			aes(x = orthophase, y = peak, colour = .id), data = features,
			shape = 19, alpha = 0
		),
		ggrepel::geom_label_repel(
			aes(
				x = orthophase, y = peak, colour = .id,
				label = paste0("Peak = ", round(peak, 2))
			),
			data = features,
			show.legend = FALSE,
			label.size = NA, label.r = 0.25, label.padding = 0.25,
			force = 50,
			segment.color = "transparent"
		),

		# Troughs
		geom_point(
			aes(x = bathyphase, y = trough, colour = .id), data = features,
			shape = 19, alpha = 0
		),
		ggrepel::geom_label_repel(
			aes(
				x = bathyphase, y = trough, colour = .id,
				label = paste0("Trough = ", round(trough, 2))
			),
			data = features,
			show.legend = FALSE,
			label.size = NA, label.r = 0.25, label.padding = 0.25,
			force = 50,
			segment.color = "transparent"
		),

		# Amplitude
		ggrepel::geom_label_repel(
			aes(
				x = (orthophase + bathyphase) / 2, y = mesor + ampGlobal/2, colour = .id,
				label = paste0("Amplitude = ", round(ampGlobal, 2))
			),
			data = features,
			show.legend = FALSE,
			label.size = NA, label.r = 0.25, label.padding = 0.25,
			force = 1,
			segment.color = "transparent"
		)
	)

	if(labels) return(g + gl) else return(g)

}

# }}}


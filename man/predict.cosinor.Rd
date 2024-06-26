% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cosinor-predict.R
\name{predict.cosinor}
\alias{predict.cosinor}
\title{Predict from a \code{cosinor}}
\usage{
\method{predict}{cosinor}(object, new_data, type = "numeric", ...)
}
\arguments{
\item{object}{A \code{cosinor} object.}

\item{new_data}{A data frame or matrix of new predictors.}

\item{type}{A single character. The type of predictions to generate. Valid
options are:
\itemize{
\item \code{"numeric"} for numeric predictions.
}}

\item{...}{Additional arguments passed to the prediction function}
}
\value{
A tibble of predictions. The number of rows in the tibble is
guaranteed to be the same as the number of rows in \code{new_data}.
}
\description{
Predict from a \code{cosinor}
}

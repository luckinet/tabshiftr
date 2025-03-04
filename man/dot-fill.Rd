% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{.fill}
\alias{.fill}
\title{Convenience wrapper around tidyr::fill()}
\usage{
.fill(x = NULL, direction = TRUE)
}
\arguments{
\item{x}{[\code{data.frame(1)}]\cr table in which to fill NA values.}

\item{direction}{[\code{character(3)}]\cr direction in which to fill missing values,
possible values are "down", "up" and "right"; if several directions are
required, provide them in the order required.}
}
\description{
Convenience wrapper around tidyr::fill()
}

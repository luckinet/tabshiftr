% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getClusterVar.R
\name{getClusterVar}
\alias{getClusterVar}
\title{Extract cluster variables}
\usage{
getClusterVar(schema = NULL, input = NULL)
}
\arguments{
\item{schema}{[\code{character(1)}]\cr the (validated) schema description of
\code{input}.}

\item{input}{[\code{character(1)}]\cr table to reorganise.}
}
\value{
a list per cluster with values of the cluster variable
}
\description{
This function extracts the cluster variable from a table by applying a schema
description to it.
}
\examples{
input <- tabs2shift$clusters_nested
schema <- setCluster(id = "sublevel",
                     group = "territories", member = c(1, 1, 2),
                     left = 1, top = c(3, 8, 15)) \%>\%
  setIDVar(name = "territories", columns = 1, rows = c(2, 14)) \%>\%
  setIDVar(name = "sublevel", columns = 1, rows = c(3, 8, 15)) \%>\%
  setIDVar(name = "year", columns = 7) \%>\%
  setIDVar(name = "commodities", columns = 2) \%>\%
  setObsVar(name = "harvested", columns = 5) \%>\%
  setObsVar(name = "production", columns = 6)

validateSchema(schema = schema, input = input) \%>\%
   getClusterVar(input = input)
}

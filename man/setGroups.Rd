% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/setGroups.R
\name{setGroups}
\alias{setGroups}
\title{Set Groups}
\usage{
setGroups(schema = NULL, rows = NULL, columns = NULL)
}
\arguments{
\item{schema}{[\code{schema(1)}]\cr In case this information is added to an
already existing schema, provide that schema here (overwrites previous
information).}

\item{rows}{[\code{list(3)}]\cr the output of \code{\link{.sum}} indicating
the rows and a function according to which those rows should be summarised.}

\item{columns}{[\code{list(3)}]\cr the output of \code{\link{.sum}}
indicating the columns and a function according to which those columns
should be summarised.}
}
\value{
An object of class \code{\link{schema}}.
}
\description{
This function allows to set groups for rows, columns or clusters that shall
be summarised.
}
\examples{
# please check the vignette for examples
}
\seealso{
Other functions to describe table arrangement: 
\code{\link{setCluster}()},
\code{\link{setFilter}()},
\code{\link{setFormat}()},
\code{\link{setIDVar}()},
\code{\link{setObsVar}()}
}
\concept{functions to describe table arrangement}

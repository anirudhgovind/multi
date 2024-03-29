% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convenience_functions.R
\name{add_unique_id}
\alias{add_unique_id}
\title{Add a unique identifier to each observation.}
\usage{
add_unique_id(x)
}
\arguments{
\item{x}{a \code{sf} object with geometries.}
}
\value{
A \code{sf} object with an additional column called \code{id}
that contains a unique identifier for each observation
}
\description{
Add a unique identifier to each observation.
}
\examples{
bangalore_boundary <- add_unique_id(bangalore_boundary)
bangalore_boundary
}

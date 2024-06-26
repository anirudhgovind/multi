% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/st_nest_coordinates.R
\name{st_nest_coordinates}
\alias{st_nest_coordinates}
\title{Nest \code{POINT} geometries to \code{LINESTRING}}
\usage{
st_nest_coordinates(x)
}
\arguments{
\item{x}{a \code{df} containing coordinates in columns named \code{x1}, \code{y1}, \code{x2},
and \code{y2}.}
}
\value{
A \code{sf} object containing \code{LINESTRING} geometries \strong{without} a CRS.
}
\description{
Nest \code{POINT} geometries to \code{LINESTRING}
}

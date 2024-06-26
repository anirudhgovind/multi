% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/st_extract_intersections.R
\name{st_extract_intersections}
\alias{st_extract_intersections}
\title{Extract street intersections.}
\usage{
st_extract_intersections(x, threshold = 2)
}
\arguments{
\item{x}{a \code{sf} object with \code{LINESTRING} geometries representing street networks.}

\item{threshold}{integer; the minimum number of intersecting roads needed to
count as an intersection. Default = 2.}
}
\value{
A \code{sf} object with \code{POINT} geometries representing street intersections.
}
\description{
Extract street intersections.
}
\examples{
intersections <- st_extract_intersections(bangalore_highways)
plot(intersections)
}

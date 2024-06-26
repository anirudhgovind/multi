#' Checks and converts `LINESTRING` geometries.
#'
#' @param x a `sf` object with `LINESTRING` geometries.
#'
#' @return A `sf` object with an additional column called `id`
#' that contains a unique identifier for each observation
#' @keywords internal
#' @noRd
process_linestrings <- function(x) {
  # Check x is of type sf

  if (!inherits(x, "sf")) {
    stop("x must be in `sf` format.")
  }

  # Check geometry types

  if (all(sf::st_is(x,
                    c("LINESTRING",
                      "MULTILINESTRING")) != TRUE)) {
    stop("x must contain `LINESTRING` geometries.")
  }

  # Select just the geometry column

  x_geometry <- sf::st_geometry(x)

  # Convert x to linestring geometry

  x_linestrings <- sf::st_cast(x_geometry,
                               "LINESTRING",
                               group_or_split = TRUE)

  return(x_linestrings)
}

#' Checks and converts `POLYGON` geometries.
#'
#' @param x a `sf` object with `POLYGON` geometries.
#'
#' @return A `sf` object with an additional column called `id`
#' that contains a unique identifier for each observation
#' @keywords internal
#' @noRd
process_polygons <- function(x) {
  # Check x is of type sf

  if (!inherits(x, "sf")) {
    stop("x must be in `sf` format.")
  }

  # Check geometry types

  if (all(sf::st_is(x,
                    c("POLYGON",
                      "MULTIPOLYGON")) != TRUE)) {
    stop("x must contain `POLYGON` geometries.")
  }

  # Select just the geometry column

  x_geometry <- sf::st_geometry(x)

  # Convert x to polygon geometry

  x_polygons <- sf::st_cast(x_geometry,
                               "POLYGON")

  x_polygons <- sf::st_as_sf(x_polygons)

  return(x_polygons)
}

#' Calculate Euclidean distances between points.
#'
#' @param pointA first `sf` object with `POINT` geometries
#' @param pointB second `sf` object with `POINT` geometries
#'
#' @return The Euclidean distance between the two points.
#' @keywords internal
#' @noRd
euclideanDistance <- function (pointA, pointB) {
  sqrt(((pointA[, 1] - pointB[, 1]) ^ 2) + ((pointA[, 2] - pointB[, 2]) ^ 2))
}

#' Calculate the angle between a line and the horizontal given two
#' coordinate locations.
#'
#' @param x1 the x-coordinate of the first point.
#' @param y1 the y-coordinate of the first point.
#' @param x2 the x-coordinate of the second point.
#' @param y2 the y-coordinate of the second point.
#'
#' @return The angle between the line and the horizontal.
#' @keywords internal
#' @noRd
calc_angle <- function(x1,
                       y1,
                       x2,
                       y2) {

  # Get the height

  height <- abs(y2 - y1)

  # Get the base

  base <- abs(x2 - x1)

  # Get the angle

  angle <- round(rad_to_deg(atan(height/base)),
                 3)
}

#' Convert an angle in radians to degrees.
#'
#' @param radians the angle in radians.
#'
#' @return The angle in degrees.
#' @keywords internal
#' @noRd
rad_to_deg <- function(radians) {
  return(radians * 180 / pi)
}

#' Add a unique identifier to each observation.
#'
#' @param x a `sf` object with geometries.
#'
#' @return A `sf` object with an additional column called `id`
#' that contains a unique identifier for each observation
#' @export
#'
#' @examples
#' bangalore_boundary <- add_unique_id(bangalore_boundary)
#' bangalore_boundary
add_unique_id <- function(x) {
  if (!inherits(x, "sf")) {
    stop("Input data must be in `sf` format.")
  }

  x$temp_id <- seq(1:nrow(x))

  return(x)
}

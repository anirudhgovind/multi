#' Extract connected streets from a data frame containing linestrings representing street networks.
#'
#' @param x an `sf` object with `LINESTRING` geometries.
#'
#' @return An `sf` object containing a subset of `x`.
#' @export
#'
#' @examples
#' connected_streets <- st_extract_connected(bangalore_highways)
#' connected_streets
st_extract_connected <- function(x) {
  # Check x is of type sf and convert to linestrings using an internal function

  x_lines <- process_linestrings(x)

  # Intersect the linestrings with themselves.

  x_intersects <-
    sf::st_intersects(sf::st_as_sf(x_lines),
                      sf::st_as_sf(x_lines),
                      sparse = TRUE)

  # Extract connected linestrings

  x_connected <- x[lengths(x_intersects) > 1,]

  return(x_connected)
}



#' Extract connected streets from a data frame containing linestrings representing street networks.
#'
#' @param x an `sf` object with `LINESTRING` geometries.
#'
#' @return An `sf` object containing a subset of `x`.
#' @export
#'
#' @examples
#' disconnected_streets <- st_extract_disconnected(bangalore_highways)
#' disconnected_streets
st_extract_disconnected <- function(x) {
  # Check x is of type sf and convert to linestrings using an internal function

  x_lines <- process_linestrings(x)

  # Intersect the linestrings with themselves.

  x_intersects <-
    sf::st_intersects(sf::st_as_sf(x_lines),
                      sf::st_as_sf(x_lines),
                      sparse = TRUE)

  # Extract connected linestrings

  x_disconnected <- x[lengths(x_intersects) == 1,]

  return(x_disconnected)
}

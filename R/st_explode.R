#' Explode geometry to manipulate constituent elements.
#'
#' @param x an `sf` object with `LINESTRING` geometries.
#'
#' @return An `sf` object with `LINESTRING` geometries with each line segment in
#' a separate row.
#' @export
#'
#' @examples
#' exploded_geom <- st_explode(bangalore_highways)
#' exploded_geom
st_explode <- function(x) {

  # Keep track of CRS

  x_crs <- sf::st_crs(x)

  x_coords <- st_unnest_coordinates(x)

  x_expl_lines <- st_nest_coordinates(x_coords)

  # Add CRS

  sf::st_crs(x_expl_lines) <- x_crs

  return(x_expl_lines)
}

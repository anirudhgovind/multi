#' Extract street intersections.
#'
#' @param x an `sf` object with `LINESTRING` geometries representing street networks.
#'
#' @return An `sf` object with `POINT` geometries representing street intersections.
#' @export
#'
#' @examples
#' intersections <- st_extract_intersections(bangalore_highways)
#' plot(bangalore_highways, col = "black")
#' plot(intersections, col = "red", add = TRUE)
st_extract_intersections <- function(x) {

  # Explode lines. Data format checks are taken care of in the st_explode function.

  x_expl_lines <- st_explode(x)

  # Exract start and end points

  x_split_end <-
    sf::st_as_sf(lwgeom::st_endpoint(x_expl_lines))

  x_split_start <-
    sf::st_as_sf(lwgeom::st_startpoint(x_expl_lines))

  # Bind the start and endpoints into one object

  x_split <- rbind(x_split_end,
                   x_split_start)

  # Intersect the split object with itself

  suppressWarnings(suppressMessages(
    x_split_intersects <-
      sf::st_intersects(x_split,
                        x_split,
                        remove_self = T)
  ))

  # Filter the split intersections to keep only instances with at least two unique intersections

  x_split_intersections <-
    x_split[lengths(x_split_intersects) > 1, ]

  # Keep only distinct points

  intersections <- dplyr::distinct(x_split_intersections)

  # Rename geometry column

  sf::st_geometry(intersections) <- "geometry"

  return(intersections)

}

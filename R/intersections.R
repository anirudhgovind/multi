#' Extract street intersections.
#'
#' @param x an `sf` object with `LINESTRING` geometries representing street networks.
#'
#' @return An `sf` object with `POINT` geometries representing street intersections.
#' @export
#'
#' @examples
#' intersections <- st_extract_intersections(bangalore_highways)
#' plot(bangalore_highways)
#' plot(intersections, add = TRUE)
st_extract_intersections <- function(x) {
  # Check x is of type sf and convert to linestrings using an internal function

  x_lines <- process_linestrings(x)

  # Extract points from the linestring geometry

  x_points <-
    sf::st_collection_extract(sf::st_intersection(x_lines),
                              "POINT")

  # Split the linestrings using the points

  x_split <- lwgeom::st_split(x_lines,
                              x_points)

  # Extract start and enpoints of all the split linestrings

  x_split_end <-
    sf::st_as_sf(lwgeom::st_endpoint(sf::st_collection_extract(x_split,
                                                               "LINESTRING")))

  x_split_start <-
    sf::st_as_sf(lwgeom::st_startpoint(sf::st_collection_extract(x_split,
                                                                 "LINESTRING")))

  # Bind the start and endpoints into one object

  x_split <- rbind(x_split_end,
                   x_split_start)

  # Intersect the split object with itself

  x_split_intersects <-
    sf::st_intersects(x_split,
                      x_split,
                      remove_self = T)

  # Filter the split intersections to keep only instances with at least two unique intersections

  x_split_intersections <-
    x_split[lengths(x_split_intersects) > 1, ]

  # Keep only distinct points

  intersections <- dplyr::distinct(x_split_intersections)

  return(intersections)

}

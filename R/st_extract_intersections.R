#' Extract street intersections from a data frame containing linestrings representing street networks.
#'
#' @param x an `sf` object with `LINESTRING` geometries.
#'
#' @return An `sf` object with `POINT` geometries representing street intersections.
#' @export
#'
#' @examples
st_extract_intersections <- function(x) {
  # Convert x to linestring geometry

  xLines <- dplyr::mutate(x,
                          geometry = sf::st_cast(geometry,
                                                 "LINESTRING"))

  # Extract points from the linestring geometry

  xPoints <-
    sf::st_collection_extract(sf::st_intersection(xLines),
                              "POINT")

  # Split the linestrings using the points

  xSplit <- lwgeom::st_split(xLines,
                             xPoints)

  # Extract start and enpoints of all the split linestrings

  xSplitA <-
    sf::st_as_sf(lwgeom::st_endpoint(sf::st_collection_extract(xSplit,
                                                               "LINESTRING")))

  xSplitB <-
    sf::st_as_sf(lwgeom::st_startpoint(sf::st_collection_extract(xSplit,
                                                                 "LINESTRING")))

  # Bind the start and endpoints into one object

  xSplit <- rbind(xSplitA,
                  xSplitB)

  # Intersect the split object with itself

  xSplitIntersects <-
    sf::st_intersects(xSplit,
                      xSplit,
                      remove_self = T)

  # Filter the split intersections to keep only instances with at least two unique intersections

  intersections <-
    xSplit[lengths(xSplitIntersects) > 1,]

  return(intersections)

}

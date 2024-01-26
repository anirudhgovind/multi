#' Explode geometry in order to manipulate constituent elements.
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

  # Check types and convert to linestrings.

  x_lines <- process_linestrings(x)

  # Keep track of CRS

  x_crs <- sf::st_crs(x)

  # Get coordinates

  x_coords <- as.data.frame(sf::st_coordinates(x_lines))

  # Set colnames

  colnames(x_coords) <- c("x1", "y1", "id")

  # Group by id. Mutate x2 and y2 as lead values.
  # This should result in a number of NAs.

  # Add x2 and y2

  x_coords_grouped <- x_coords |>
    dplyr::group_by(id) |>
    dplyr::mutate(x2 = dplyr::lead(x1,
                                   n = 1,
                                   order_by = id),
                  y2 = dplyr::lead(y1,
                                   n = 1,
                                   order_by = id))

  # Ungroup

  x_coords <- dplyr::ungroup(x_coords_grouped)

  # Drop missing values

  x_coords <- na.omit(x_coords)

  # Create linestrings

  x_expl_lines <-
    sf::st_sf(data.frame(geometry = sf::st_as_sfc(
      paste0(
        "LINESTRING (",
        x_coords$x1,
        " ",
        x_coords$y1,
        ", ",
        x_coords$x2,
        " ",
        x_coords$y2,
        ")"
      )
    )))

  # Add CRS

  sf::st_crs(x_expl_lines) <- x_crs

  return(x_expl_lines)
}

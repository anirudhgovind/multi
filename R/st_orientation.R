#' Determine the interior angle between a `LINESTRING` and the horizontal.
#'
#' @param x a `sf` object containing `LINESTRING` geometries.
#' @param nest logical; If `TRUE`, `LINESTRING` geometries will be recreated.
#'
#' @return An unnested `sf` object containing the orientation of each line
#' segment making up the original `LINESTRING` geometries.
#' @export
st_orientation <- function(x,
                           nest = TRUE) {

  # Housekeeping

  id <- x1 <- y1 <- x2 <- y2 <- NULL

  # Keep track of the crs

  x_crs <- sf::st_crs(x)

  # The extract coordinates function adds columns titled x1, y1, x2, and y2
  # It then computes the angles of each line string.

  x_orientation <- st_unnest_coordinates(x) |>
    dplyr::mutate(
      orientation = dplyr::case_when(
        x2 > x1 & y2 < y1 ~ -calc_angle(x1,
                                        y1,
                                        x2,
                                        y2),
        x2 < x1 &
          y2 > y1 ~ -calc_angle(x1,
                                y1,
                                x2,
                                y2),
        y2 == y1 ~ 0,
        x2 == x1 ~ 90,
        TRUE ~ calc_angle(x1,
                          y1,
                          x2,
                          y2)
      )
    )

  # Nest the columns to form line strings

  if (nest == TRUE) {

    x_orientation <- st_nest_coordinates(x_orientation)

    # Add the crs back in

    sf::st_crs(x_orientation) <- x_crs

  }

  return(x_orientation)

}



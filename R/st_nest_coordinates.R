#' Nest `POINT` geometries to `LINESTRING`
#'
#' @param x a `df` containing coordinates in columns named `x1`, `y1`, `x2`,
#' and `y2`.
#'
#' @return A `sf` object containing `LINESTRING` geometries **without** a CRS.
#' @export
#'
st_nest_coordinates <- function(x) {

  # Housekeeping

  x1 <- y1 <- x2 <- y2 <- NULL

  # If cols are present in the data run the following:

  if (any(c("x1", "y1", "x2", "y2") %in% colnames(x))) {
    # Keep other columns

    x_other <- x |>
      dplyr::select(-x1,
                    -y1,
                    -x2,
                    -y2)

    # Convert to an sf object

    x_nested <- sf::st_sf(data.frame(geometry = sf::st_as_sfc(
      paste0("LINESTRING (",
             x$x1,
             " ",
             x$y1,
             ", ",
             x$x2,
             " ",
             x$y2,
             ")")
    )))

    # Add the other columns back in

    x_nested <- cbind(x_nested,
                      x_other)

    return(x_nested)

  } else {
    stop("Coordinates must be provided in columns x1, y1, x2, and y2.")
  }

}


#' Unnest `POINT` geometries from `LINESTRINGS`
#'
#' @param x a `sf` object containing `LINESTRING` geometries.
#'
#' @return A `df` where each row contains the `x1`, `y1`, `x2`, and `y2`
#' coordinates for a line.
#'
#' @export
st_unnest_coordinates <- function(x) {

  # Housekeeping

  id <- temp_id <-  x1 <- y1 <- x2 <- y2 <- NULL

  # Check types and convert to linestrings.

  x_lines <- process_linestrings(x)

  # Keep other columns

  x_other <- x |>
    add_unique_id() |>
    sf::st_set_geometry(NULL)

  # Get coordinates

  x_coords <- as.data.frame(sf::st_coordinates(x_lines))

  # Set colnames

  colnames(x_coords) <- c("x1", "y1", "id")

  # Group by id. Mutate x2 and y2 as lead values.
  # This should result in a number of NAs.
  # Drop them.

  # Add x2 and y2

  x_coords <- x_coords |>
    dplyr::group_by(id) |>
    dplyr::mutate(x2 = dplyr::lead(x1,
                                   n = 1,
                                   order_by = id),
                  y2 = dplyr::lead(y1,
                                   n = 1,
                                   order_by = id)) |>
    dplyr::ungroup() |>
    stats::na.omit()

  # Add other columns

  x_coords <- dplyr::left_join(x_coords,
                               x_other,
                               by = c("id" = "temp_id")) |>
    dplyr::select(-id)

  return(x_coords)
}

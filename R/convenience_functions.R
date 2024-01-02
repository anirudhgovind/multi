process_linestrings <- function(x) {
  # Check x is of type sf

  if (!inherits(x, "sf")) {
    stop("Street networks must be in `sf` format.")
  }

  # Check geometry types

  if (all(sf::st_is(x,
                    "LINESTRING") != TRUE)) {
    stop("x must contain `LINESTRING` geometries.")
  }

  # Select just the geometry column

  x_geometry <- sf::st_geometry(x)

  # Convert x to linestring geometry

  x_linestrings <- sf::st_cast(x_geometry,
                               "LINESTRING")

  return(x_linestrings)
}

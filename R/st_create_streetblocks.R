#' Create street block spatial units.
#'
#' @param x an `sf` object with `LINESTRING` geometries representing a street
#' network.
#' @param boundary an `sf` object with `POLYGON` geometries representing the
#' extents of the study area.
#' @param merge_threshold numeric; value represents the smallest acceptable
#' size for a spatial unit. Contiguous units will be iteratively merged until
#' this value is reached. To skip this process, set `merge_threshold = NULL`.
#' @param verbose logical; if `FALSE` no status messages will be output.
#'
#' @return An `sf` object with `POLYGON` geometries representing street block
#' spatial units.
#' @export
#'
#' @examples
#' street_blocks <- st_create_streetblocks(x = bangalore_highways,
#' boundary = bangalore_boundary, merge_threshold = 1, verbose = FALSE)
#' plot(street_blocks)
st_create_streetblocks <- function(x,
                                   boundary,
                                   merge_threshold = 4050,
                                   verbose = T) {
  # Check linestrings and polygons

  x <- process_linestrings(x)

  boundary <- process_polygons(boundary)

  # Select just the geometry column.

  x_geometry <- sf::st_as_sf(sf::st_geometry(x))

  boundary_geometry <- sf::st_geometry(boundary)

  # Clip x to the boundary

  suppressWarnings(suppressMessages(
    x_geometry <- sf::st_intersection(x_geometry,
                                      boundary_geometry)
  ))

  # Cast polygon to linestrings

  boundary_geometry <- sf::st_as_sf(sf::st_cast(boundary_geometry,
                                                "LINESTRING",
                                                do_split = T))

  # Combine it with the rest of the linestring geometry

  z <- rbind(x_geometry,
             boundary_geometry)

  # Union the geometry

  suppressWarnings(suppressMessages(z_union <- sf::st_union(z)))

  # Convert lines to polygons

  z_polygonize <- sf::st_polygonize(z_union)

  # Convert to sf objects

  z_sf <- sf::st_as_sf(z_polygonize)

  # Extract to individual polygons
  z_sf <- sf::st_collection_extract(z_sf)

  # Rename column

  z_sf <- sf::st_geometry(z_sf)

  # Convert to an sf object and rename column

  z_sf <- sf::st_as_sf(z_sf)

  sf::st_geometry(z_sf) <- "geometry"

  if (verbose == T) {
    message(paste0(nrow(z_sf), " street blocks created."))
  }

  if (is.null(merge_threshold)) {
    warning("Small geometries (< 1 m^2) may be present.")
  }

  if (!is.null(merge_threshold)) {
    z_sf <- st_merge_spatialunits(x = z_sf,
                                  merge_threshold = merge_threshold,
                                  verbose = verbose)
  }

  return(z_sf)
}

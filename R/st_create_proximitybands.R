#' Create proximity band spatial units.
#'
#' @param x a `sf` object with `LINESTRING` geometries representing a street
#' network.
#' @param band_width numeric; width of the band as measured from the centerline
#' of the `LINESTRING` representing the street.
#' @param segment_length numeric; Length of segments street linestrings are
#' subdivided into. Default = 2.
#' @param merge_threshold numeric; value represents the smallest acceptable
#' size for a spatial unit. Contiguous units will be iteratively merged until
#' this value is reached. To skip this process, set `merge_threshold = NULL`.
#' See st_merge_spatialunits() for more details.
#' @param merge_type string; Passed on to st_merge_spatialunits().
#' Criteria with which polygons are merged. Must be one of
#' "min_centroid_distance", "min_shared_boundary", or "max_shared_boundary".
#' Default = "min_centroid_distance".
#' See st_merge_spatialunits() for more details.
#' @param contiguity string; one of "queen" or "rook". Default = "rook".
#' @param verbose logical; if `FALSE` no status messages will be output.
#'
#' @return A `sf` object with `POLYGON` geometries representing tessellated
#' spatial units.
#' @seealso st_merge_spatialunits, [moter::motess()]
#' @references Araldi, A., & Fusco, G. (2019). From the street to the
#' metropolitan region: Pedestrian perspective in urban fabric analysis.
#' Environment and Planning B: Urban Analytics and City Science, 46(7),
#' 1243–1263. https://doi.org/10.1177/2399808319832612
#' @export
#'
#' @examples
#' proximity_bands <- st_create_proximitybands(x = bangalore_highways,
#' verbose = FALSE, merge_threshold = NULL)
#' plot(proximity_bands)
st_create_proximitybands <- function(x,
                                     band_width = 10,
                                     segment_length = 2,
                                     merge_threshold = 1015,
                                     merge_type = "min_centroid_distance",
                                     contiguity = "rook",
                                     verbose = T) {

  # Turn off s2 geometry

  sf::sf_use_s2(FALSE)

  # Check polygons (if present) and linestrings data

  if (sf::st_is_longlat(x) == TRUE) {
    # Keep track of original CRS

    x_crs <- sf::st_crs(x)

    # Reproject to 3857

    x <- sf::st_transform(x,
                          3857)

  }
  # Process line strings

  x <- process_linestrings(x)

  x <- sf::st_sf(x)

  # Add IDs to the linestrings

  x <- add_unique_id(x)

  # Draw buffers around the street network

  x_buffer <- sf::st_buffer(x,
                            band_width)

  x_buffer <- sf::st_union(x_buffer)

  voronoi_polygons <- segment_and_tessellate(x = x,
                                             segment_length = segment_length)

  # Clip vor polygons to the drawn buffer

  suppressMessages(suppressWarnings(
    x_prox_band <- sf::st_intersection(voronoi_polygons,
                                       x_buffer)
  ))

  if (verbose == T) {
    message(paste0(nrow(x_prox_band)," proximity bands created."))
  }

  if (is.null(merge_threshold)) {
    warning(
      "Small geometries (< 1 m^2) may be present. Use st_merge_spatialunits() to aggregate them."
    )
  }

  if (!is.null(merge_threshold)) {
    x_prox_band <- st_merge_spatialunits(
      x = x_prox_band,
      merge_threshold = merge_threshold,
      merge_type = merge_type,
      contiguity = contiguity,
      verbose = verbose
    )
  }

  # If geometry is long lat, convert CRS back to the original.

  if (sf::st_is_longlat(x) == TRUE) {
    x_prox_band <- sf::st_transform(x_prox_band,
                                    x_crs)
  }

  return(x_prox_band)

}

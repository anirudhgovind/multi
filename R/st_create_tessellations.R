#' Create tessellated spatial units.
#'
#' @param x an `sf` object with `POLYGON` geometries representing buildings.
#' @param boundary an `sf` object with `POLYGON` geometries representing the
#' extents of the study area.
#' @param segment_length numeric; Length of segments building polygons are
#' subdivided into. Default = 0.5. See Fleischmann et al., 2020 for details.
#' @param shrink_extent numeric; Extent of inward buffer on building polygons.
#' Default = 0.4. See Fleischmann et al., 2020 for details.
#' @param merge_threshold numeric; value represents the smallest acceptable
#' size for a spatial unit. Contiguous units will be iteratively merged until
#' this value is reached. To skip this process, set `merge_threshold = NULL`.
#' See st_merge_spatialunits() for more details.
#' @param merge_type string; Passed on to st_merge_spatialunits().
#' Criteria with which polygons are merged. Must be one of
#' "min_centroid_distance", "min_shared_boundary", or "max_shared_boundary".
#' Default = "min_centroid_distance".
#' See st_merge_spatialunits() for more details.
#' @param type string; Type of tessellated spatial unit to create.
#' Default = "morphological".
#' Note: Currently, only "morphological" tessellation is implemented.
#' @param contiguity string; one of "queen" or "rook". Default = "rook".
#' @param verbose logical; if `FALSE` no status messages will be output.
#'
#' @return An `sf` object with `POLYGON` geometries representing tessellated
#' spatial units.
#' @export
#' @seealso st_merge_spatialunits, [moter::motess()]
#' @references Fleischmann, M., Feliciotti, A., Romice, O., & Porta, S. (2020).
#' Morphological tessellation as a way of partitioning space: Improving consistency
#' in urban morphology at the plot scale. Computers, Environment and Urban Systems,
#' 80, 101441.
#' @examples
#' tessellations <- st_create_tessellations(x = bangalore_buildings,
#' boundary = bangalore_boundary, segment_length = 0.5, shrink_extent = 0.4,
#' merge_threshold = NULL, verbose = FALSE)
#' plot(tessellations)
st_create_tessellations <-
  function(x,
           boundary,
           type = "morphological",
           segment_length = 0.5,
           shrink_extent = 0.4,
           merge_threshold = NULL,
           merge_type = "min_centroid_distance",
           contiguity = "rook",
           verbose = T) {


    # Init vars used further down

    temp_id <- geometry <- NULL

    # Turn off s2 geometry

    sf::sf_use_s2(FALSE)

    if (type == "morphological") {

      # Check data types.

      x <- process_polygons(x)

      boundary <- process_polygons(boundary)

      # Add IDs to the buildings

      x <- add_unique_id(x)

      # If geometry is long lat, transform it

      if (sf::st_is_longlat(x) == TRUE |
          sf::st_is_longlat(boundary) == TRUE) {
        # Keep track of original CRS

        x_crs <- sf::st_crs(x)
        boundary_crs <- sf::st_crs(boundary)

        # Reproject to 3857

        x <- sf::st_transform(x,
                              3857)
        boundary <- sf::st_transform(boundary,
                                     3857)
      }

      # Buffer the buildings inwards

      x <- sf::st_buffer(x,
                         dist = (-1 * shrink_extent))

      # Segment these lines and tessellate them

      voronoi_polygons <- segment_and_tessellate(x = x,
                                                 segment_length = segment_length)

      # Clip to boundary

      suppressMessages(suppressWarnings(
        voronoi_polygons <- sf::st_intersection(voronoi_polygons,
                                                boundary)
      ))

      # Drop the temporary ID column

      tessellations <- voronoi_polygons |>
        dplyr::select(-temp_id)


      if (verbose == T) {
        message(paste0(nrow(tessellations)," ", type, " tessellations created."))
      }

      if (is.null(merge_threshold)) {
        warning(
          "Small geometries (< 1 m^2) may be present. Use st_merge_spatialunits() to aggregate them."
        )
      }

      if (!is.null(merge_threshold)) {
        tessellations <- st_merge_spatialunits(
          x = tessellations,
          merge_threshold = merge_threshold,
          merge_type = merge_type,
          contiguity = contiguity,
          verbose = verbose
        )
      }

      # If geometry is long lat, convert CRS back to the original.

      if (sf::st_is_longlat(x) == TRUE |
          sf::st_is_longlat(boundary) == TRUE) {

        tessellations <- sf::st_transform(tessellations,
                                          x_crs)
      }

      return(tessellations)
    } else {

      stop("Type must (currently) be 'morphological'.")
    }

  }

#' Segment `LINESTRING` or `POLYGON` geometries and create Voronoi tessellations.
#'
#' @param x a `sf` object with `LINESTRING` or `POLYGON` geometries.
#'
#' @return A `sf` object with Voronoi tessellations
#' @keywords internal
#' @noRd
segment_and_tessellate <- function(x, segment_length) {

  # Init vars used further down

  temp_id <- geometry <- NULL

  # Slice into segments

  segments <- sf::st_segmentize(x, segment_length)

  # Extract points from the segments

  suppressMessages(suppressWarnings(points <-
                                      sf::st_cast(segments,
                                                  "POINT")))

  # Remove duplicates

  points_unique <- unique(points)

  # Draw Voronoi Polygons

  voronoi_polygons <-
    sf::st_collection_extract(sf::st_voronoi(sf::st_union(points_unique)))

  # Dissolve polygons

  voronoi_polygons <-
    voronoi_polygons[unlist(sf::st_intersects(points_unique,
                                              voronoi_polygons))]

  # Join points to the polygons

  voronoi_polygons <- sf::st_join(sf::st_sf(voronoi_polygons),
                                  points_unique)

  # Make geometry valid

  voronoi_polygons <- sf::st_make_valid(voronoi_polygons)

  voronoi_polygons <- sf::st_buffer(voronoi_polygons,
                                    0)

  # Merge polygons

  suppressMessages(
    suppressWarnings(
      voronoi_polygons <- voronoi_polygons |>
        dplyr::group_by(temp_id) |>
        dplyr::summarise(geometry = sf::st_union(geometry)) |>
        dplyr::ungroup()
    )
  )

  return(voronoi_polygons)

}

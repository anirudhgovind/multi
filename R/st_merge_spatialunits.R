#' Iteratively merge spatial units to ensure all of them meet a minimum area threshold.
#'
#' @param x an `sf` object with `POLYGON` geometries representing the
#' spatial units to be merged.
#' @param merge_threshold numeric; value in square meters represents the
#' smallest acceptable area for a spatial unit. Contiguous units will be
#' iteratively merged until this value is reached. To skip this process,
#' set `merge_threshold = NULL`.
#' @param verbose logical; if `FALSE` no status messages will be output.
#'
#' @return An `sf` object with `POLYGON` geometries representing street block
#' spatial units.
#' @export
#'
#' @examples
st_merge_spatialunits <- function(x,
                                  merge_threshold,
                                  verbose = T) {
  # Turn off s2 geometry

  sf::sf_use_s2(FALSE)

  # Check x is of type sf

  if (!inherits(x, "sf")) {
    stop("Street networks must be in `sf` format.")
  }

  # Check geometry types

  if (all(sf::st_is(x,
                    "POLYGON") != TRUE)) {
    stop("x must contain `POLYGON` geometries.")
  }

  # Check merge_threshold is numeric

  if (is.numeric(merge_threshold) != TRUE) {
    stop("merge_threshold must be a numeric value.")
  }

  # Check name of geometry column

  if (!"geometry" %in% colnames(x)) {
    sf::st_geometry(x) <- "geometry"
    x <- sf::st_as_sf(x)
    warning("The `sf` geometry column was renamed before merging.")
  }

  # Run this as long as units have an area below the merge threshold

  while (any(as.integer(sf::st_area(x)) < merge_threshold)) {
    for (i in 1:nrow(x)) {
      # Calculate areas

      x$areas <- as.integer(sf::st_area(x))

      # First check to see if unit i is valid and if it has an area less than the merge threshold

      if (is.na(x$areas[i])) {
        x <- x[-i,]
      }

      else if (x$areas[i] < merge_threshold) {
        # Find neighbours

        suppressWarnings(suppressMessages(neighbours <-
                                            sfdep::st_contiguity(x,
                                                                 queen = F)))

        x$neighbours <- neighbours

        # Extract the ref_unit

        ref_unit <- x[i, ]

        # Extract the neighbours

        ref_unit_neighbours <- unlist(ref_unit$neighbours)

        # Get geometry of the neighbours

        ref_unit_neighbours_geo <- x[ref_unit_neighbours,]

        ref_unit_neighbours_geo$neighbours <- NULL
        ref_unit_neighbours_geo$areas <- NULL
        ref_unit_neighbours_geo$index <-
          rownames(ref_unit_neighbours_geo)

        # Find the closest polygon

        suppressWarnings(suppressMessages(
          dist <-
            euclideanDistance(
              sf::st_coordinates(sf::st_centroid(ref_unit)),
              sf::st_coordinates(sf::st_centroid(ref_unit_neighbours_geo))
            )
        ))

        closest_unit <-
          as.integer(ref_unit_neighbours_geo$index[which.min(dist)])

        # Extract units to merge and retain

        x_retained <- x[-c(i, closest_unit), ]

        x_retained$neighbours <- NULL

        x_retained$areas <- NULL

        x_merged <- x[c(i, closest_unit), ]

        suppressWarnings(suppressMessages(x_merged <-
                                            sf::st_as_sf(sf::st_union(x_merged))))

        sf::st_geometry(x_merged) <- "geometry"

        # Create a new dataframe

        x <- rbind(x_merged,
                   x_retained)

        if (verbose == TRUE) {
          message(i, ": Polygons merged.")
        }

      }

      else {
        if (verbose == TRUE) {
          message(i, ": Polygon area is larger than merge threshold.")
        }

      }

    }
  }

  x$neighbours <- NULL

  if (verbose == T) {
    message("Merging completed.")
  }

  return(x)
}

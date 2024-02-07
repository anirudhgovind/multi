#' Iteratively merge spatial units to ensure all of them meet a minimum area threshold.
#'
#' @param x an `sf` object with `POLYGON` geometries representing the
#' spatial units to be merged.
#' @param merge_threshold numeric; value in square meters represents the
#' smallest acceptable area for a spatial unit. Contiguous units will be
#' iteratively merged until this value is reached. To skip this process,
#' set `merge_threshold = NULL`.
#' @param merge_type string; criteria with which polygons are merged. Must be one of
#' "min_centroid_distance", "min_shared_boundary", or "max_shared_boundary".
#' Default = "min_centroid_distance".
#' @param contiguity string; one of "queen" or "rook". Default = "rook".
#' @param verbose logical; if `FALSE` no status messages will be output.
#'
#' @return A `sf` object with `POLYGON` geometries representing street block
#' spatial units.
#' @export
#'
#' @examples
#' street_blocks <- st_create_streetblocks(x = bangalore_highways,
#' boundary = bangalore_boundary, merge_threshold = NULL, verbose = FALSE)
#' street_blocks_merged <- st_merge_spatialunits(x = street_blocks,
#' merge_threshold = 4050, verbose = FALSE)
#' plot(street_blocks_merged)
st_merge_spatialunits <- function(x,
                                  merge_threshold,
                                  merge_type = "min_centroid_distance",
                                  contiguity = "rook",
                                  verbose = T) {
  # Turn off s2 geometry

  sf::sf_use_s2(FALSE)

  # Check polygons

  x <- process_polygons(x)

  # Check merge_threshold is numeric

  if (is.numeric(merge_threshold) != TRUE) {
    stop("merge_threshold must be a numeric value.")
  }

  # Check type is one of defined defaults

  if (!merge_type %in% c("min_centroid_distance",
                         "min_shared_boundary",
                         "max_shared_boundary")) {
    stop(
      "merge_type must be one of 'min_centroid_distance', 'min_shared_boundary', or 'max_shared_boundary'."
    )
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
        x <- x[-i, ]
      }

      else if (x$areas[i] < merge_threshold) {
        # Find neighbours

        if (contiguity == "rook") {
          suppressWarnings(suppressMessages(neighbours <-
                                              sfdep::st_contiguity(x,
                                                                   queen = F)))
        } else if (contiguity == "queen") {
          suppressWarnings(suppressMessages(neighbours <-
                                              sfdep::st_contiguity(x,
                                                                   queen = T)))
        } else {
          stop("contiguity must be one of 'queen' or 'rook'.")
        }

        x$neighbours <- neighbours

        # Extract the ref_unit

        ref_unit <- x[i,]

        # Extract the neighbours

        ref_unit_neighbours <- unlist(ref_unit$neighbours)

        # Get geometry of the neighbours

        ref_unit_neighbours_geo <- x[ref_unit_neighbours, ]

        ref_unit_neighbours_geo$neighbours <- NULL
        ref_unit_neighbours_geo$areas <- NULL
        ref_unit_neighbours_geo$index <-
          rownames(ref_unit_neighbours_geo)

        # Find the index of the polygon to merge with based on the type of merging selected

        if (merge_type == "min_centroid_distance") {
          selected_unit <- min_centroid_distance(ref_unit = ref_unit,
                                                 ref_unit_neighbours_geo = ref_unit_neighbours_geo)
        } else if (merge_type == "min_shared_boundary" |
                   merge_type == "max_shared_boundary") {
          selected_unit <- shared_boundary_length(
            ref_unit = ref_unit,
            ref_unit_neighbours_geo = ref_unit_neighbours_geo,
            merge_type = merge_type
          )
        }

        # Extract units to merge and retain

        x_retained <- x[-c(i, selected_unit),]

        x_retained$neighbours <- NULL

        x_retained$areas <- NULL

        x_merged <- x[c(i, selected_unit),]

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

# Function to select polygon based on minimum centroid distance

min_centroid_distance <- function(ref_unit,
                                  ref_unit_neighbours_geo) {
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

  return(closest_unit)

}

# Function to select polygon based on shared boundary length

shared_boundary_length <- function(ref_unit,
                                   ref_unit_neighbours_geo,
                                   merge_type = merge_type) {
  # Intersect the ref_unit with all other units and find its length

  suppressWarnings(suppressMessages(
    lengths <- sf::st_intersection(ref_unit,
                                   ref_unit_neighbours_geo)
  ))

  lengths$length <- sf::st_length(lengths)

  if (merge_type == "min_shared_boundary") {
    closest_unit <-
      as.integer(ref_unit_neighbours_geo$index[which.min(lengths$length)])

    return(closest_unit)
  }


  if (merge_type == "max_shared_boundary") {
    closest_unit <-
      as.integer(ref_unit_neighbours_geo$index[which.max(lengths$length)])

    return(closest_unit)
  }
}

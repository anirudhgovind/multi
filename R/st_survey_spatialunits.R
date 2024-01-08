#' Survey a set of spatial units to get summary statistics
#'
#' @param x an `sf` object with `POLYGON` geometries representing the
#' spatial units to be surveyed.
#' @param trim numeric; fraction (0 to 0.5) of values to be trimmed from each
#' end to compute a trimmed mean
#'
#' @return A `data.frame` object with numeric values of min, max, mean, and
#' median.
#' @export
#'
#' @examples
#' street_blocks <- st_create_streetblocks(x = bangalore_highways,
#' boundary = bangalore_boundary, merge_threshold = NULL, verbose = FALSE)
#' survey_report <- st_survey_spatialunits(x = street_blocks)
st_survey_spatialunits <- function(x,
                                   trim = NULL) {

  # Check x is of type sf

  if (!inherits(x, "sf")) {
    stop("Street networks must be in `sf` format.")
  }

  # Check geometry types

  if (all(sf::st_is(x,
                    "POLYGON") != TRUE)) {
    stop("x must contain `POLYGON` geometries.")
  }

  # Calculate areas

  x$areas <- sf::st_area(x)

  # Report summary values

  x_min <- min(x$areas)
  x_max <- max(x$areas)
  x_mean <- mean(x$areas)
  x_median <- mean(x$areas)

  # Survey Result

  output <- data.frame(
    description = c("min",
                    "max",
                    "mean",
                    "median"),
    value = c(x_min,
              x_max,
              x_mean,
              x_median)
  )

  # Conditional to add a trimmed mean value
  if (!is.null(trim)) {
    if (!is.numeric(trim)) {
      stop("Trim should contain a numeric value.")
    } else {
      x_trim <- mean(x$areas,
                     trim = trim)

      suffix <- data.frame(description = c("trimmed_mean"),
                           value = c(x_trim))

      output <- rbind(output,
                      suffix)
    }
  }

  return(output)
}

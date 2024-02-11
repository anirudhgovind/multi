#' Survey a set of spatial units to get summary statistics.
#'
#' @param x an `sf` object with `POLYGON` geometries representing the
#' spatial units to be surveyed.
#' @param trim numeric; fraction (0 to 0.5) of values to be trimmed from the lower
#' end to compute a trimmed mean.
#' @param viz binary; Displays a scatterplot of area distributions.
#'
#' @return A `data.frame` object with numeric values of min, max, mean, and
#' median.
#' @export
#'
#' @examples
#' street_blocks <- st_create_streetblocks(x = bangalore_highways,
#' boundary = bangalore_boundary, merge_threshold = NULL, verbose = FALSE)
#' survey_report <- st_survey_spatialunits(x = street_blocks)
#' survey_report
st_survey_spatialunits <- function(x,
                                   trim = NULL,
                                   viz = TRUE) {

  # Init var for later

  areas <- NULL

  # Check x is of type sf

  if (!inherits(x, "sf")) {
    stop("Street networks must be in `sf` format.")
  }

  # Check geometry types

  if (all(sf::st_is(x,
                    c("POLYGON",
                      "MULTIPOLYGON")) != TRUE)) {
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
    category = c("Area",
                 "Area",
                 "Area",
                 "Area"),
    description = c("Min",
                    "Max",
                    "Mean",
                    "Median"),
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
      x_trim <- x |>
        sf::st_set_geometry(NULL) |>
        dplyr::arrange(areas) |>
        dplyr::slice_tail(prop = 1 - trim) |>
        dplyr::summarise(value = mean(areas))

      suffix <- data.frame(
        category = c("Area"),
        description = c(paste0("Trimmed Mean (",
                               trim,
                               ")")),
        value = c(x_trim)
      )

      output <- rbind(output,
                      suffix)
    }
  }

  if (viz == TRUE) {
    plot(x$areas,
         col = "gray",
         xlab = "Spatial Unit Index",
         ylab = "Areas")
    graphics::abline(h = c(x_min,
                           x_max,
                           x_mean,
                           x_median),
                     lty = 2)
    graphics::text(
      x = c(0,
            nrow(x),
            0,
            nrow(x)),
      y = c(x_min,
            x_max,
            x_mean,
            x_median),
      labels = c("min",
                 "max",
                 "mean",
                 "median")
    )
    if (!is.null(trim)) {
      graphics::abline(h = x_trim,
                       col = "black")
      graphics::text(x = nrow(x) / 2,
                     y = x_trim,
                     labels = "trimmed mean")
    }
  }

  print(output,
        right = FALSE)
}

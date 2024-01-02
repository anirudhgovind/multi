euclideanDistance <- function (pointA, pointB) {
  sqrt(((pointA[, 1] - pointB[, 1]) ^ 2) + ((pointA[, 2] - pointB[, 2]) ^ 2))
}

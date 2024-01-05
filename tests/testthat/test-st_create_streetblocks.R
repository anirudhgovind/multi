# Input Checks

test_that("st_create_streetblocks() fails on non `sf` objects", {
  expect_error(st_create_streetblocks(x = 42,
                                      boundary = bangalore_boundary,
                                      merge_threshold = NULL,
                                      verbose = FALSE))
})

test_that("st_create_streetblocks() fails on non-numeric merge thresholds", {
  expect_error(st_create_streetblocks(x = bangalore_boundary,
                                      boundary = bangalore_boundary,
                                      merge_threshold = bangalore_highways,
                                      verbose = FALSE))
})

test_that("st_create_streetblocks() fails on non-linestring objects passed to x", {
  expect_error(st_create_streetblocks(x = bangalore_boundary,
                                      boundary = bangalore_highways,
                                      merge_threshold = 1,
                                      verbose = FALSE))
})

test_that("st_create_streetblocks() fails on non-polygon objects passed to boundary", {
  expect_error(st_create_streetblocks(x = bangalore_boundary,
                                      boundary = bangalore_highways,
                                      merge_threshold = 1,
                                      verbose = FALSE))
})

test_that("st_create_streetblocks() fails on objects with lengths > 1 passed to boundary", {
  expect_error(st_create_streetblocks(x = bangalore_boundary,
                                      boundary = rbind(bangalore_boundary,
                                                       bangalore_boundary),
                                      merge_threshold = 1,
                                      verbose = FALSE))
})

# Output checks

test_that("st_create_streetblocks() output consists only of POLYGONS", {
  expect_equal(all(sf::st_is(
    st_create_streetblocks(
      x = bangalore_highways,
      boundary = bangalore_boundary,
      merge_threshold = NULL,
      verbose = FALSE
    ),
    "POLYGON"
  )),
  TRUE)
})

test_that("st_create_streetblocks() output consists only of POLYGONS even with merging", {
  expect_equal(all(sf::st_is(
    st_create_streetblocks(
      x = bangalore_highways,
      boundary = bangalore_boundary,
      merge_threshold = 1,
      verbose = FALSE
    ),
    "POLYGON"
  )),
  TRUE)
})

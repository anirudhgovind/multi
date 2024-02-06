test_that("st_merge_spatialunits() fails on non `sf` objects", {
  expect_error(st_merge_spatialunits(x = 42,
                                     merge_threshold = NULL))
})

test_that("st_merge_spatialunits() fails on non-numeric merge thresholds",
          {
            expect_error(
              st_merge_spatialunits(x = bangalore_boundary,
                                    merge_threshold = bangalore_highways)
            )
          })

test_that("st_merge_spatialunits() fails on non-polygon objects", {
  expect_error(st_merge_spatialunits(x = bangalore_highways,
                                     merge_threshold = 100))
})

test_that("st_merge_spatialunits() fails on non-default merge types", {
  expect_error(st_create_streetblocks(
    x = bangalore_highways,
    boundary = bangalore_boundary,
    merge_threshold = 4050,
    merge_type = "random",
    verbose = FALSE
  ))
})

# Output tests

test_that("st_merge_spatialunits() output has the right number of polygons on merging by min. centroid distance", {
  expect_equal(nrow(
    st_create_streetblocks(
      x = bangalore_highways,
      boundary = bangalore_boundary,
      merge_threshold = 4050,
      verbose = FALSE
    )),
    92)
})

test_that("st_merge_spatialunits() output has the right number of polygons on merging by min. shared boundary", {
  expect_equal(nrow(
    st_create_streetblocks(
      x = bangalore_highways,
      boundary = bangalore_boundary,
      merge_threshold = 4050,
      merge_type = "min_shared_boundary",
      verbose = FALSE
    )),
    84)
})

test_that("st_merge_spatialunits() output has the right number of polygons on merging by max. shared boundary", {
  expect_equal(nrow(
    st_create_streetblocks(
      x = bangalore_highways,
      boundary = bangalore_boundary,
      merge_threshold = 4050,
      merge_type = "max_shared_boundary",
      verbose = FALSE
    )),
    74)
})

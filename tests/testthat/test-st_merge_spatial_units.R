test_that("st_merge_spatial_units() fails on non `sf` objects", {
  expect_error(st_merge_spatial_units(x = 42,
                                      merge_threshold = NULL))
})

test_that("st_merge_spatial_units() fails on non-numeric merge thresholds", {
  expect_error(st_merge_spatial_units(x = bangalore_boundary,
                                      merge_threshold = bangalore_highways))
})

test_that("st_merge_spatial_units() fails on non-polygon objects", {
  expect_error(st_merge_spatial_units(x = bangalore_highways,
                                      merge_threshold = 100))
})

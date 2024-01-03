test_that("st_create_streetblocks() fails on non `sf` objects", {
  expect_error(st_create_streetblocks(x = 42,
                                      boundary = bangalore_boundary,
                                      merge_threshold = NULL))
})

test_that("st_create_streetblocks() fails on non-numeric merge thresholds", {
  expect_error(st_create_streetblocks(x = bangalore_boundary,
                                      boundary = bangalore_boundary,
                                      merge_threshold = bangalore_highways))
})

test_that("st_create_streetblocks() fails on non-linestring objects passed to x", {
  expect_error(st_create_streetblocks(x = bangalore_boundary,
                                      boundary = bangalore_highways,
                                      merge_threshold = 100))
})

test_that("st_create_streetblocks() fails on non-polygon objects passed to boundary", {
  expect_error(st_create_streetblocks(x = bangalore_boundary,
                                      boundary = bangalore_highways,
                                      merge_threshold = 100))
})

test_that("st_create_streetblocks() fails on objects with lengths > 1 passed to boundary", {
  expect_error(st_create_streetblocks(x = bangalore_boundary,
                                      boundary = rbind(bangalore_boundary,
                                                       bangalore_boundary),
                                      merge_threshold = 100))
})

test_that("st_extract_connected() fails on non `sf` objects", {
  expect_error(st_extract_connected(42))
})

test_that("st_extract_connected() fails on non-linestring objects", {
  expect_error(st_extract_connected(bangalore_boundary))
})

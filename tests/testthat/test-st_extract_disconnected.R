test_that("st_extract_disconnected() fails on non `sf` objects", {
  expect_error(st_extract_disconnected(42))
})

test_that("st_extract_disconnected() fails on non-linestring objects", {
  expect_error(st_extract_disconnected(bangalore_boundary))
})

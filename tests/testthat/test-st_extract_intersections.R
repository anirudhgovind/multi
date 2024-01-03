test_that("st_extract_intersections() extracts intersections", {
  expect_equal(nrow(st_extract_intersections(bangalore_highways)),
               500)
})

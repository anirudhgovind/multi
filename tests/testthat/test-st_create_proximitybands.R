test_that("st_create_proximitybands() fails on non `sf` objects", {
  expect_error(
    st_create_proximitybands(
      x = 42,
      merge_threshold = NULL,
      verbose = FALSE
    )
  )
})

test_that("st_create_proximitybands() fails on non-linestring objects passed to x",
          {
            expect_error(
              st_create_tessellations(
                x = bangalore_buildings,
                merge_threshold = NULL,
                verbose = FALSE
              )
            )
          })

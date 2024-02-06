test_that("st_create_tessellations() fails on non `sf` objects", {
  expect_error(
    st_create_tessellations(
      x = 42,
      boundary = bangalore_boundary,
      id = "uID",
      type = "morphological",
      verbose = FALSE
    )
  )
})

test_that("st_create_tessellations() fails on non-polygon objects passed to x",
          {
            expect_error(
              st_create_tessellations(
                x = bangalore_highways,
                boundary = bangalore_boundary,
                type = "morphological",
                verbose = FALSE
              )
            )
          })

test_that("st_create_tessellations() fails on non-polygon objects passed to boundary",
          {
            expect_error(
              st_create_tessellations(
                x = bangalore_boundary,
                boundary = bangalore_highways,
                type = "morphological",
                verbose = FALSE
              )
            )
          })

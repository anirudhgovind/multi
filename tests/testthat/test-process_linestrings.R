test_that("process_linestrings() fails on non `sf` objects", {
  expect_error(process_linestrings(42))
})

test_that("process_linestrings() returns `LINESTRING` geometries", {
  expect_equal(all(sf::st_is(bangalore_highways,
                             c("LINESTRING"))),
               TRUE)
})


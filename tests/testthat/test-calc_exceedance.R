test_that("calc_exceedance works", {

  ## test for expected values
  x <- seq(.1, .9, by = .1)
  expect_equal(calc_exceedance(x),
               rev(x))

  ## test for expected errors
  expect_error(calc_exceedance("one"))
})

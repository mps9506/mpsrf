test_that("calc_geomean works", {

  ## test for expected values
  x <- rlnorm(100)

  expect_equal(calc_geomean(x),
               (prod(x))^(1/length(x)))

  ## test for expected errors
  expect_error(calc_geom(c(-1,100)))
  expect_error(calc_geom(c(0,100)))

  ## test for expected structure
  expect_equal(length(calc_geomean(x, ci = .95)),
               3)
})

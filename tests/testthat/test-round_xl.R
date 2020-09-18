test_that("round_xl works", {

  ## test for expected values
  expect_equal(round_xl(.5 + -2:4),
               c(-2,-1,1,2,3,4,5))

  ## test for expected errors
  expect_error(round_xl("a"))
})

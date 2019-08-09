test_that("Recycling is carried out using tidyverse recycling rules", {
  x0 <- integer()
  x1 <- 1L
  x2 <- c(2L, 2L)
  x3 <- c(3L, 3L, 3L)

  expect_equal(slide2(x0, x0, ~.x), list())
  expect_equal(slide2(x0, x1, ~.x), list())
  expect_error(slide2(x0, x2, ~.x), class = "vctrs_error_incompatible_size")
  expect_equal(slide2(x1, x1, ~.x), list(x1))
  expect_equal(slide2(x1, x2, ~.x), list(x1, x1))
  expect_error(slide2(x2, x3, ~.x), class = "vctrs_error_incompatible_size")
})

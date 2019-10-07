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

test_that("slide2() forces arguments in the same way as base R / map2()", {
  f_slide <- slide2(1:2, 1:2, function(i, j) function(x) x + i + j)
  f_base <- mapply(function(i, j) function(x) x + i + j, 1:2, 1:2)

  expect_equal(f_slide[[1]](0), f_base[[1]](0))
  expect_equal(f_slide[[2]](0), f_base[[2]](0))
})

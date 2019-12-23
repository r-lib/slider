test_that("Recycling is carried out using tidyverse recycling rules", {
  x0 <- integer()
  x1 <- 1L
  x2 <- c(2L, 2L)
  x3 <- c(3L, 3L, 3L)

  expect_equal(hop2(x0, x0, integer(), integer(), ~.x), list())
  expect_equal(hop2(x0, x1, 1, 1, ~.x), list(NULL))
  expect_equal(hop2(x0, x1, integer(), integer(), ~.x), list())
  expect_error(hop2(x0, x2, 1:2, 1:2, ~.x), class = "vctrs_error_incompatible_size")
  expect_equal(hop2(x1, x1, 1, 1, ~.x), list(x1))
  expect_equal(hop2(x1, x2, 1:2, 1:2, ~.x), list(x1, x1))
  expect_error(hop2(x2, x3, 1:2, 1:2, ~.x), class = "vctrs_error_incompatible_size")
})

test_that("hop2() forces arguments in the same way as base R / map2()", {
  f_slide <- hop2(1:2, 1:2, 1:2, 1:2, function(i, j) function(x) x + i + j)
  f_base <- mapply(function(i, j) function(x) x + i + j, 1:2, 1:2)

  expect_equal(f_slide[[1]](0), f_base[[1]](0))
  expect_equal(f_slide[[2]](0), f_base[[2]](0))
})

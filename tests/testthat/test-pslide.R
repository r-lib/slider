test_that("An empty list() results in empty `ptype` returned", {
  expect_equal(pslide(list(), ~.x), list())
  expect_equal(pslide_dbl(list(), ~.x), numeric())
  expect_equal(pslide_vec(list(), ~.x, .ptype = 1:5), integer())
})

test_that("Recycling is carried out using tidyverse recycling rules", {
  x0 <- integer()
  x1 <- 1L
  x2 <- c(2L, 2L)
  x3 <- c(3L, 3L, 3L)

  expect_equal(pslide(list(x0, x0), ~.x), list())
  expect_equal(pslide(list(x0, x1), ~.x), list())
  expect_error(pslide(list(x0, x2), ~.x), class = "vctrs_error_incompatible_size")
  expect_equal(pslide(list(x1, x1), ~.x), list(x1))
  expect_equal(pslide(list(x1, x2), ~.x), list(x1, x1))
  expect_error(pslide(list(x2, x3), ~.x), class = "vctrs_error_incompatible_size")
})

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

test_that("pslide() can iterate over a data frame", {
  x <- data.frame(x = 1:5, y = 6:10)
  expect_equal(pslide(x, ~.x + .y), as.list(x$x + x$y))
})

test_that("pslide() can iterate over a data frame with a data frame column", {
  x <- data.frame(c1 = 1:2)
  x$x <- x

  expect_equal(
    pslide(x, ~list(...)),
    list(as.list(vec_slice(x, 1)), as.list(vec_slice(x, 2)))
  )
})

test_that("pslide() requires a list-like input", {
  expect_error(pslide(1:5, ~.x), "list, not integer")
})

test_that("pslide() forces arguments in the same way as base R / pmap()", {
  f_slide <- pslide(list(1:2, 1:2, 1:2), function(i, j, k) function(x) x + i + j + k)
  f_base <- mapply(function(i, j, k) function(x) x + i + j + k, 1:2, 1:2, 1:2)

  expect_equal(f_slide[[1]](0), f_base[[1]](0))
  expect_equal(f_slide[[2]](0), f_base[[2]](0))
})

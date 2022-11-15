test_that("Empty starts/stops results in empty `ptype` returned", {
  expect_equal(phop(list(1), integer(), integer(), ~.x), list())
  expect_equal(phop_vec(list(1), integer(), integer(), ~.x, .ptype = integer()), integer())
})

test_that("Recycling is carried out using tidyverse recycling rules", {
  x0 <- integer()
  x1 <- 1L
  x2 <- c(2L, 2L)
  x3 <- c(3L, 3L, 3L)

  expect_equal(phop(list(x0, x0), integer(), integer(), ~.x), list())
  expect_equal(phop(list(x0, x0), 1, 1, ~.x), list(integer()))
  expect_equal(phop(list(x0, x1), integer(), integer(), ~.x), list())
  expect_equal(phop(list(x0, x1), 1, 1, ~.x), list(integer()))
  expect_snapshot((expect_error(phop(list(x0, x2), 1, 1, ~.x), class = "vctrs_error_incompatible_size")))
  expect_equal(phop(list(x1, x1), 1, 1, ~.x), list(x1))
  expect_equal(phop(list(x1, x2), 1:2, 1:2, ~.x), list(x1, x1))
  expect_snapshot((expect_error(phop(list(x2, x3), 1:3, 1:3, ~.x), class = "vctrs_error_incompatible_size")))
})

test_that("phop() can iterate over a data frame", {
  x <- data.frame(x = 1:5, y = 6:10)
  expect_equal(phop(x, 1:5, 1:5, ~.x + .y), as.list(x$x + x$y))
})

test_that("phop() can iterate over a data frame with a data frame column", {
  x <- data.frame(c1 = 1:2)
  x$x <- x

  expect_equal(
    phop(x, 1:2, 1:2, ~list(...)),
    list(as.list(vec_slice(x, 1)), as.list(vec_slice(x, 2)))
  )
})

test_that("phop() requires a list-like input", {
  expect_snapshot(error = TRUE, phop(1:5, ~.x))
})

test_that("phop() forces arguments in the same way as base R / pmap()", {
  f_slide <- phop(list(1:2, 1:2, 1:2), 1:2, 1:2, function(i, j, k) function(x) x + i + j + k)
  f_base <- mapply(function(i, j, k) function(x) x + i + j + k, 1:2, 1:2, 1:2)

  expect_equal(f_slide[[1]](0), f_base[[1]](0))
  expect_equal(f_slide[[2]](0), f_base[[2]](0))
})

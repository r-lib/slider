# ------------------------------------------------------------------------------
# hop_index2_vec

test_that("hop_index2_vec() works", {
  expect_equivalent(hop_index2_vec(1L, 1L, 1, 1, 1, ~.x + .y), list(2L))
})

test_that("hop_index2_vec() retains names of x", {
  expect_equivalent(hop_index2_vec(c(x = 1L), c(y = 1L), 1,  1, 1, ~.x + .y), list(x = 2L))
})

test_that("hop_index2_vec() can simplify automatically", {
  expect_equivalent(hop_index2_vec(1, 2, 1,  1, 1, ~.x + .y, .ptype = NULL), 3)
})

test_that("hop_index2_vec() errors if it can't simplify", {
  fn <- function(x, y) if (x == 1L) {1} else {"hi"}
  expect_error(
    hop_index2_vec(1:2, 1:2, 1:2, 1:2, 1:2, fn, .ptype = NULL),
    class = "vctrs_error_incompatible_type"
  )
})


# ------------------------------------------------------------------------------
# suffix tests

test_that("hop_index2_int() works", {
  expect_equal(hop_index2_int(1L, 1L, 1, 1, 1, ~.x), 1L)
})

test_that("hop_index2_int() can coerce", {
  expect_equal(hop_index2_int(1, 1, 1, 1, 1, ~.x), 1L)
})

test_that("hop_index2_dbl() works", {
  expect_equal(hop_index2_dbl(1, 1, 1, 1, 1, ~.x), 1)
})

test_that("hop_index2_dbl() can coerce", {
  expect_equal(hop_index2_dbl(1L, 1, 1, 1, 1, ~.x), 1)
})

test_that("hop_index2_chr() works", {
  expect_equal(hop_index2_chr("x", 1, 1, 1, 1, ~.x), "x")
})

test_that("hop_index2_chr() can coerce", {
  expect_equal(hop_index2_chr(1, 1, 1, 1, 1, ~.x), "1")
})

test_that("hop_index2_lgl() works", {
  expect_equal(hop_index2_lgl(TRUE, 1, 1, 1, 1, ~.x), TRUE)
})

test_that("hop_index2_lgl() can coerce", {
  expect_equal(hop_index2_lgl(1, 1, 1, 1, 1, ~.x), TRUE)
})

# ------------------------------------------------------------------------------
# data frame suffix tests

test_that("hop_index2_dfr() works", {
  expect_equal(
    hop_index2_dfr(1:2, 1:2, 1:2, c(1, 1), c(1, 2), ~.x),
    slide_dfr(1:2, ~.x, .before = 1)
  )

  x <- 1:2
  expect_equal(
    hop_index2_dfr(x, x, 1:2, c(1, 1), c(1, 2), ~data.frame(x = .x)),
    slide_dfr(x, ~data.frame(x = .x), .before = 1)
  )
})

test_that("hop_index2_dfc() works", {
  expect_equal(
    hop_index2_dfc(1:2, 1:2, 1:2, c(1, 1), c(1, 2), ~.x),
    slide_dfc(1:2, ~.x, .before = 1)
  )

  x <- 1:2
  expect_equal(
    hop_index2_dfc(x, x, 1:2, c(1, 1), c(1, 2), ~data.frame(x = .x)),
    slide_dfc(x, ~data.frame(x = .x), .before = 1)
  )
})

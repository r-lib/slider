# ------------------------------------------------------------------------------
# phop_index_vec

test_that("phop_index_vec() works", {
  expect_equivalent(phop_index_vec(list(1L, 1L), 1, 1, 1, ~.x + .y), list(2L))
})

test_that("phop_index_vec() retains names of first input", {
  expect_equivalent(phop_index_vec(list(c(x = 1L), c(y = 1L)), 1, 1, 1, ~.x + .y), list(x = 2L))
})

test_that("phop_index_vec() can simplify automatically", {
  expect_equivalent(phop_index_vec(list(1, 2), 1, 1, 1, ~.x + .y, .ptype = NULL), 3)
})

test_that("phop_index_vec() errors if it can't simplify", {
  fn <- function(x, y) if (x == 1L) {1} else {"hi"}
  expect_error(
    phop_index_vec(list(1:2, 1:2), 1:2, 1:2, 1:2, fn, .ptype = NULL),
    class = "vctrs_error_incompatible_type"
  )
})

test_that("completely empty input returns ptype", {
  expect_equal(phop_index_vec(list(), integer(), integer(), integer(), ~.x), list())
  expect_equal(phop_index_vec(list(), integer(), integer(), integer(), ~.x, .ptype = int()), int())
})

test_that("empty `.l` and `.i`, but size `n > 0` `.starts` and `.stops` returns size `n` empty ptype", {
  expect_equal(
    phop_index_vec(list(), integer(), 1:2, 2:3, ~.x, .ptype = int()),
    c(NA_integer_, NA_integer_)
  )
})

# ------------------------------------------------------------------------------
# suffix tests

test_that("phop_index_int() works", {
  expect_equivalent(phop_index_int(list(1L, 1L), 1, 1, 1, ~.x + .y), 2L)
})

test_that("phop_index_int() can coerce", {
  expect_equivalent(phop_index_int(list(1, 1), 1, 1, 1, ~.x + .y), 2L)
})

test_that("phop_index_dbl() works", {
  expect_equivalent(phop_index_dbl(list(1, 1), 1, 1, 1, ~.x), 1)
})

test_that("phop_index_dbl() can coerce", {
  expect_equivalent(phop_index_dbl(list(1L, 1L), 1, 1, 1, ~.x + .y), 2)
})

test_that("phop_index_chr() works", {
  expect_equivalent(phop_index_chr(list("x", 1), 1, 1, 1, ~.x), "x")
})

test_that("phop_index_chr() can coerce", {
  expect_equivalent(phop_index_chr(list(1, 1), 1, 1, 1, ~.x + .y), "2")
})

test_that("phop_index_lgl() works", {
  expect_equivalent(phop_index_lgl(list(TRUE, 1), 1, 1, 1, ~.x), TRUE)
})

test_that("phop_index_lgl() can coerce", {
  expect_equivalent(phop_index_lgl(list(1, 0), 1, 1, 1, ~.x + .y), TRUE)
})

# ------------------------------------------------------------------------------
# data frame suffix tests

test_that("phop_index_dfr() works", {
  expect_equal(
    phop_index_dfr(list(1:2, 1:2), 1:2, c(1, 1), c(1, 2), ~c(.x, .y)),
    data.frame(
      ...1 = c(1, 1),
      ...2 = c(1, 2),
      ...3 = c(NA, 1),
      ...4 = c(NA, 2)
    )
  )

  x <- 1:2
  expect_equal(
    phop_index_dfr(list(x, x), 1:2, c(1, 1), c(1, 2), ~data.frame(x = .x, y = .y)),
    data.frame(x = c(1, 1, 2), y = c(1, 1, 2))
  )
})

test_that("phop_index_dfc() works", {
  x <- 1:2
  expect_equal(
    phop_index_dfc(list(x, x), 1:2, c(1, 1), c(1, 2), ~data.frame(x = .x, y = .y)),
    data.frame(
      x...1 = c(1, 1),
      y...2 = c(1, 1),
      x...3 = c(1, 2),
      y...4 = c(1, 2)
    )
  )
})

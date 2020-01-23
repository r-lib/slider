# ------------------------------------------------------------------------------
# phop_index_vec

test_that("phop_index_vec() works", {
  expect_equivalent(phop_index_vec(list(1L, 1L), 1, 1, 1, ~.x + .y), 2L)
})

test_that("phop_index_vec() retains names of first input", {
  expect_equivalent(phop_index_vec(list(c(x = 1L), c(y = 1L)), 1, 1, 1, ~.x + .y), c(x = 2L))
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
  expect_equal(phop_index_vec(list(), integer(), integer(), integer(), ~.x), NULL)
  expect_equal(phop_index_vec(list(), integer(), integer(), integer(), ~.x, .ptype = list()), list())
  expect_equal(phop_index_vec(list(), integer(), integer(), integer(), ~.x, .ptype = int()), int())
})

test_that("empty `.l` and `.i`, but size `n > 0` `.starts` and `.stops` returns size `n` empty ptype", {
  expect_equal(
    phop_index_vec(list(), integer(), 1:2, 2:3, ~.x, .ptype = int()),
    c(NA_integer_, NA_integer_)
  )
})

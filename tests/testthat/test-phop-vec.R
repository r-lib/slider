# ------------------------------------------------------------------------------
# phop_vec

test_that("phop_vec() works", {
  expect_equivalent(phop_vec(list(1L, 1L), 1, 1, ~.x + .y), 2L)
})

test_that("phop_vec() retains names of first input", {
  expect_equivalent(phop_vec(list(c(x = 1L), c(y = 1L)), 1, 1, ~.x + .y), c(x = 2L))
})

test_that("phop_vec() can simplify automatically", {
  expect_equivalent(phop_vec(list(1, 2), 1, 1, ~.x + .y, .ptype = NULL), 3)
})

test_that("phop_vec() errors if it can't simplify", {
  fn <- function(x, y) if (x == 1L) {1} else {"hi"}
  expect_error(
    phop_vec(list(1:2, 1:2), 1:2, 1:2, fn, .ptype = NULL),
    class = "vctrs_error_incompatible_type"
  )
})

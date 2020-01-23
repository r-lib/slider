# ------------------------------------------------------------------------------
# hop2_vec

test_that("hop2_vec() works", {
  expect_equivalent(hop2_vec(1L, 1L, 1, 1, ~.x + .y), 2L)
})

test_that("hop2_vec() retains names of x", {
  expect_equivalent(hop2_vec(c(x = 1L), c(y = 1L), 1, 1, ~.x + .y), c(x = 2L))
})

test_that("hop2_vec() can simplify automatically", {
  expect_equivalent(hop2_vec(1, 2, 1, 1, ~.x + .y, .ptype = NULL), 3)
})

test_that("hop2_vec() errors if it can't simplify", {
  fn <- function(x, y) if (x == 1L) {1} else {"hi"}
  expect_error(
    hop2_vec(1:2, 1:2, 1:2, 1:2, fn, .ptype = NULL),
    class = "vctrs_error_incompatible_type"
  )
})

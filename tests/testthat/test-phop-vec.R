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

# ------------------------------------------------------------------------------
# .ptype

test_that("`.ptype = NULL` validates that element lengths are 1", {
  expect_error(
    phop_vec(list(1:2, 1:2), 1:2, 1:2, ~if(.x == 1L) {1:2} else {1}, .ptype = NULL),
    "In iteration 1, the result of `.f` had size 2, not 1."
  )
  expect_error(
    phop_vec(list(1:2, 1:2), 1:2, 1:2, ~if(.x == 1L) {NULL} else {2}, .ptype = NULL),
    "In iteration 1, the result of `.f` had size 0, not 1."
  )
})

test_that("`phop_vec()` falls back to `c()` method as required", {
  local_c_foobar()

  expect_identical(phop_vec(list(1:3, 1:3), 1:3, 1:3, ~foobar(.x), .ptype = foobar()), foobar(1:3))
  expect_condition(phop_vec(list(1:3, 1:3), 1:3, 1:3, ~foobar(.x), .ptype = foobar()), class = "slider_c_foobar")

  expect_identical(phop_vec(list(1:3, 1:3), 1:3, 1:3, ~foobar(.x)), foobar(1:3))
  expect_condition(phop_vec(list(1:3, 1:3), 1:3, 1:3, ~foobar(.x)), class = "slider_c_foobar")
})

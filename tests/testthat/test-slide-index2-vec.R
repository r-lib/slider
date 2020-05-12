# ------------------------------------------------------------------------------
# slide_index2_*()

test_that("slide_index2_*() works", {
  expect_equivalent(slide_index2_vec(1L, 1L, 1, ~.x + .y), 2L)
  expect_equivalent(slide_index2_int(1L, 1L, 1, ~.x + .y), 2L)
})

test_that("slide_index2_*() retains names of x", {
  expect_equivalent(slide_index2_vec(c(x = 1L), c(y = 1L), 1, ~.x + .y), c(x = 2L))
  expect_equivalent(slide_index2_int(c(x = 1L), c(y = 1L), 1, ~.x + .y), c(x = 2L))
})

test_that("slide_index2_vec() can simplify automatically", {
  expect_equivalent(slide_index2_vec(1, 2, 1, ~.x + .y, .ptype = NULL), 3)
})

test_that("slide_index2_*() errors if it can't simplify", {
  fn <- function(x, y) if (x == 1L) {1} else {"hi"}
  expect_error(
    slide_index2_vec(1:2, 1:2, 1:2, fn, .ptype = NULL),
    class = "vctrs_error_incompatible_type"
  )
  expect_error(
    slide_index2_int(1:2, 1:2, 1:2, fn),
    class = "vctrs_error_incompatible_type"
  )
})

# ------------------------------------------------------------------------------
# suffix tests

test_that("slide_index2_int() works", {
  expect_equal(slide_index2_int(1L, 1L, 1, ~.x), 1L)
})

test_that("slide_index2_int() can coerce", {
  expect_equal(slide_index2_int(1, 1, 1, ~.x), 1L)
})

test_that("slide_index2_dbl() works", {
  expect_equal(slide_index2_dbl(1, 1, 1, ~.x), 1)
})

test_that("slide_index2_dbl() can coerce", {
  expect_equal(slide_index2_dbl(1L, 1, 1, ~.x), 1)
})

test_that("slide_index2_chr() works", {
  expect_equal(slide_index2_chr("x", 1, 1, ~.x), "x")
})

test_that("slide_index2_chr() cannot coerce", {
  expect_error(slide_index2_chr(1, 1, 1, ~.x), class = "vctrs_error_incompatible_type")
})

test_that("slide_index2_lgl() works", {
  expect_equal(slide_index2_lgl(TRUE, 1, 1, ~.x), TRUE)
})

test_that("slide_index2_lgl() can coerce", {
  expect_equal(slide_index2_lgl(1, 1, 1, ~.x), TRUE)
})

# ------------------------------------------------------------------------------
# data frame suffix tests

test_that("slide_index2_dfr() works", {
  expect_equal(
    slide_index2_dfr(1:2, 1:2, 1:2, ~.x, .before = 1),
    slide_dfr(1:2, ~.x, .before = 1)
  )

  x <- 1:2
  expect_equal(
    slide_index2_dfr(x, x, 1:2, ~data.frame(x = .x), .before = 1),
    slide_dfr(x, ~data.frame(x = .x), .before = 1)
  )
})

test_that("slide_index2_dfc() works", {
  expect_equal(
    slide_index2_dfc(1:2, 1:2, 1:2, ~.x, .before = 1),
    slide_dfc(1:2, ~.x, .before = 1)
  )

  x <- 1:2
  expect_equal(
    slide_index2_dfc(x, x, 1:2, ~data.frame(x = .x), .before = 1),
    slide_dfc(x, ~data.frame(x = .x), .before = 1)
  )
})

# ------------------------------------------------------------------------------
# .ptype

test_that("`.ptype = NULL` is size stable (#78)", {
  expect_length(slide_index2_vec(1:4, 1:4, 1:4, ~1, .before = 1, .complete = TRUE), 4)
})

test_that("size 0 inputs returns .ptype", {
  expect_identical(slide_index2_vec(integer(), integer(), integer(), ~.x, .ptype = NULL), NULL)
  expect_identical(slide_index2_vec(integer(), integer(), integer(), ~.x, .ptype = double()), double())
})

test_that("`slide_index2_vec()` falls back to `c()` method as required", {
  local_c_foobar()

  expect_identical(slide_index2_vec(1:3, 1:3, 1:3, ~foobar(.x), .ptype = foobar()), foobar(1:3))
  expect_condition(slide_index2_vec(1:3, 1:3, 1:3, ~foobar(.x), .ptype = foobar()), class = "slider_c_foobar")

  expect_identical(slide_index2_vec(1:3, 1:3, 1:3, ~foobar(.x)), foobar(1:3))
  expect_condition(slide_index2_vec(1:3, 1:3, 1:3, ~foobar(.x)), class = "slider_c_foobar")
})

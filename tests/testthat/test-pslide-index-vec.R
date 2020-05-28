# ------------------------------------------------------------------------------
# pslide_index_*()

test_that("pslide_index_*() works", {
  expect_equivalent(pslide_index_vec(list(1L, 1L), 1, ~.x + .y), 2L)
  expect_equivalent(pslide_index_int(list(1L, 1L), 1, ~.x + .y), 2L)
})

test_that("pslide_index_*() retains names of first input", {
  expect_equivalent(pslide_index_vec(list(c(x = 1L), c(y = 1L)), 1, ~.x + .y), c(x = 2L))
  expect_equivalent(pslide_index_int(list(c(x = 1L), c(y = 1L)), 1, ~.x + .y), c(x = 2L))
})

test_that("pslide_index_vec() can simplify automatically", {
  expect_equivalent(pslide_index_vec(list(1, 2), 1, ~.x + .y, .ptype = NULL), 3)
})

test_that("pslide_index_*() errors if it can't simplify", {
  fn <- function(x, y) if (x == 1L) {1} else {"hi"}
  expect_error(
    pslide_index_vec(list(1:2, 1:2), 1:2, fn, .ptype = NULL),
    class = "vctrs_error_incompatible_type"
  )
  expect_error(
    pslide_index_int(list(1:2, 1:2), 1:2, fn),
    class = "vctrs_error_incompatible_type"
  )
})

test_that("completely empty input returns ptype", {
  expect_equal(pslide_index_vec(list(), integer(), ~.x), NULL)
  expect_equal(pslide_index_vec(list(), integer(), ~.x, .ptype = list()), list())
  expect_equal(pslide_index_vec(list(), integer(), ~.x, .ptype = int()), int())
  expect_equal(pslide_index_int(list(), integer(), ~.x), int())
})

# ------------------------------------------------------------------------------
# suffix tests

test_that("pslide_index_int() works", {
  expect_equivalent(pslide_index_int(list(1L, 1L), 1, ~.x + .y), 2L)
})

test_that("pslide_index_int() can coerce", {
  expect_equivalent(pslide_index_int(list(1, 1), 1, ~.x + .y), 2L)
})

test_that("pslide_index_dbl() works", {
  expect_equivalent(pslide_index_dbl(list(1, 1), 1, ~.x), 1)
})

test_that("pslide_index_dbl() can coerce", {
  expect_equivalent(pslide_index_dbl(list(1L, 1L), 1, ~.x + .y), 2)
})

test_that("pslide_index_chr() works", {
  expect_equivalent(pslide_index_chr(list("x", 1), 1, ~.x), "x")
})

test_that("pslide_index_chr() cannot coerce", {
  expect_error(pslide_index_chr(list(1, 1), 1, ~.x + .y), class = "vctrs_error_incompatible_type")
})

test_that("pslide_index_lgl() works", {
  expect_equivalent(pslide_index_lgl(list(TRUE, 1), 1, ~.x), TRUE)
})

test_that("pslide_index_lgl() can coerce", {
  expect_equivalent(pslide_index_lgl(list(1, 0), 1, ~.x + .y), TRUE)
})

# ------------------------------------------------------------------------------
# data frame suffix tests

test_that("pslide_index_dfr() works", {
  expect_equal(
    pslide_index_dfr(list(1:2, 1:2), 1:2, ~c(.x, .y), .before = 1),
    data.frame(
      ...1 = c(1, 1),
      ...2 = c(1, 2),
      ...3 = c(NA, 1),
      ...4 = c(NA, 2)
    )
  )

  x <- 1:2
  expect_equal(
    pslide_index_dfr(list(x, x), 1:2, ~data.frame(x = .x, y = .y), .before = 1),
    data.frame(x = c(1, 1, 2), y = c(1, 1, 2))
  )
})

test_that("pslide_index_dfc() works", {
  x <- 1:2
  expect_equal(
    pslide_index_dfc(list(x, x), 1:2, ~data.frame(x = .x, y = .y), .before = 1),
    data.frame(
      x...1 = c(1, 1),
      y...2 = c(1, 1),
      x...3 = c(1, 2),
      y...4 = c(1, 2)
    )
  )
})

# ------------------------------------------------------------------------------
# .ptype

test_that("`.ptype = NULL` is size stable (#78)", {
  expect_length(pslide_index_vec(list(1:4, 1:4), 1:4, ~1, .before = 1, .complete = TRUE), 4)
})

test_that("`pslide_index_vec()` falls back to `c()` method as required", {
  local_c_foobar()

  expect_identical(pslide_index_vec(list(1:3, 1:3), 1:3, ~foobar(.x), .ptype = foobar(integer())), foobar(1:3))
  expect_condition(pslide_index_vec(list(1:3, 1:3), 1:3, ~foobar(.x), .ptype = foobar(integer())), class = "slider_c_foobar")

  expect_identical(pslide_index_vec(list(1:3, 1:3), 1:3, ~foobar(.x)), foobar(1:3))
  expect_condition(pslide_index_vec(list(1:3, 1:3), 1:3, ~foobar(.x)), class = "slider_c_foobar")
})

# ------------------------------------------------------------------------------
# .complete

test_that(".complete produces typed `NA` values", {
  expect_identical(pslide_index_int(list(1:3, 1:3), 1:3, ~1L, .before = 1, .complete = TRUE), c(NA, 1L, 1L))
  expect_identical(pslide_index_dbl(list(1:3, 1:3), 1:3, ~1, .before = 1, .complete = TRUE), c(NA, 1, 1))
  expect_identical(pslide_index_chr(list(1:3, 1:3), 1:3, ~"1", .before = 1, .complete = TRUE), c(NA, "1", "1"))
  expect_identical(pslide_index_vec(list(1:3, 1:3), 1:3, ~1, .before = 1, .complete = TRUE), c(NA, 1, 1))
  expect_identical(pslide_index_vec(list(1:3, 1:3), 1:3, ~1, .before = 1, .complete = TRUE, .ptype = integer()), c(NA, 1L, 1L))
})

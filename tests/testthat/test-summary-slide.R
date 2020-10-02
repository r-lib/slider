# ------------------------------------------------------------------------------
# slide_sum()

test_that("integer before works", {
  x <- 1:4 + 0

  expect_identical(slide_sum(x, before = 1), c(1, 3, 5, 7))
  expect_identical(slide_sum(x, before = 2), c(1, 3, 6, 9))
})

test_that("integer after works", {
  x <- 1:4 + 0

  expect_identical(slide_sum(x, after = 1), c(3, 5, 7, 4))
  expect_identical(slide_sum(x, after = 2), c(6, 9, 7, 4))
})

test_that("negative before/after works", {
  x <- 1:4 + 0

  expect_identical(slide_sum(x, before = -1, after = 2), c(5, 7, 4, 0))
  expect_identical(slide_sum(x, before = 2, after = -1), c(0, 1, 3, 5))

  expect_identical(slide_sum(x, before = -1, after = 2, complete = TRUE), c(5, 7, NA, NA))
  expect_identical(slide_sum(x, before = 2, after = -1, complete = TRUE), c(NA, NA, 3, 5))
})

test_that("`Inf` before/after works", {
  x <- 1:4 + 0

  expect_identical(slide_sum(x, before = Inf), cumsum(x))
  expect_identical(slide_sum(x, after = Inf), rev(cumsum(rev(x))))
})

test_that("step / complete works", {
  x <- 1:4 + 0

  expect_identical(slide_sum(x, before = 1, step = 2), c(1, NA, 5, NA))
  expect_identical(slide_sum(x, before = 1, step = 2, complete = TRUE), c(NA, 3, NA, 7))
})

test_that("NA / NaN results are correct", {
  x <- c(rep(1, 10), rep(NA, 10), 1:4)
  y <- c(rep(NA, 10), rep(NaN, 10), 1:4)

  expect_identical(
    slide_sum(x, before = 3),
    slide_dbl(x, sum, .before = 3)
  )
  expect_identical(
    slide_sum(y, before = 3),
    slide_dbl(y, sum, .before = 3)
  )
  expect_identical(
    slide_sum(rev(y), before = 3),
    slide_dbl(rev(y), sum, .before = 3)
  )
})

test_that("`na_rm = TRUE` works", {
  x <- NA
  y <- c(1, NA, 2, 3)

  expect_identical(slide_sum(x, na_rm = TRUE), 0)
  expect_identical(slide_sum(y, na_rm = TRUE, before = 1), c(1, 1, 2, 5))
})

test_that("Inf and -Inf results are correct", {
  x <- c(1, Inf, -Inf, 1)
  expect_identical(slide_sum(x, before = 1), c(1, Inf, NaN, -Inf))
})

test_that("precision matches base R (long doubles)", {
  x <- rep(1/7, 10)
  expect_identical(sum(x), slide_sum(x, Inf)[[length(x)]])
})

test_that("Inf + -Inf = NaN propagates with `na_rm = TRUE`", {
  x <- c(-Inf, Inf, rep(1, SEGMENT_TREE_FANOUT - 2L))
  before <- SEGMENT_TREE_FANOUT - 1L

  expect_identical(
    slide_sum(x, before = before, na_rm = T),
    slide_dbl(x, sum, .before = before, na_rm = T)
  )
})

# ------------------------------------------------------------------------------
# slide_prod()

test_that("integer before works", {
  x <- 1:4 + 0

  expect_identical(slide_prod(x, before = 1), c(1, 2, 6, 12))
  expect_identical(slide_prod(x, before = 2), c(1, 2, 6, 24))
})

test_that("integer after works", {
  x <- 1:4 + 0

  expect_identical(slide_prod(x, after = 1), c(2, 6, 12, 4))
  expect_identical(slide_prod(x, after = 2), c(6, 24, 12, 4))
})

test_that("negative before/after works", {
  x <- 1:4 + 0

  expect_identical(slide_prod(x, before = -1, after = 2), c(6, 12, 4, 1))
  expect_identical(slide_prod(x, before = 2, after = -1), c(1, 1, 2, 6))

  expect_identical(slide_prod(x, before = -1, after = 2, complete = TRUE), c(6, 12, NA, NA))
  expect_identical(slide_prod(x, before = 2, after = -1, complete = TRUE), c(NA, NA, 2, 6))
})

test_that("`Inf` before/after works", {
  x <- 1:4 + 0

  expect_identical(slide_prod(x, before = Inf), cumprod(x))
  expect_identical(slide_prod(x, after = Inf), rev(cumprod(rev(x))))
})

test_that("step / complete works", {
  x <- 1:4 + 0

  expect_identical(slide_prod(x, before = 1, step = 2), c(1, NA, 6, NA))
  expect_identical(slide_prod(x, before = 1, step = 2, complete = TRUE), c(NA, 2, NA, 12))
})

test_that("NA / NaN results are correct", {
  x <- c(rep(1, 10), rep(NA, 10), 1:4)
  y <- c(rep(NA, 10), rep(NaN, 10), 1:4)

  expect_identical(
    slide_prod(x, before = 3),
    slide_dbl(x, prod, .before = 3)
  )
  expect_identical(
    slide_prod(y, before = 3),
    slide_dbl(y, prod, .before = 3)
  )
  expect_identical(
    slide_prod(rev(y), before = 3),
    slide_dbl(rev(y), prod, .before = 3)
  )
})

test_that("`na_rm = TRUE` works", {
  x <- NA
  y <- c(1, NA, 2, 3)

  expect_identical(slide_prod(x, na_rm = TRUE), 1)
  expect_identical(slide_prod(y, na_rm = TRUE, before = 1), c(1, 1, 2, 6))
})

test_that("Inf and -Inf results are correct", {
  x <- c(1, Inf, -Inf, 0)
  expect_identical(slide_prod(x, before = 1), c(1, Inf, -Inf, NaN))
})

test_that("Inf * 0 = NaN propagates with `na_rm = TRUE`", {
  x <- c(Inf, 0, rep(1, SEGMENT_TREE_FANOUT - 2L))
  before <- SEGMENT_TREE_FANOUT - 1L

  expect_identical(
    slide_prod(x, before = before, na_rm = T),
    slide_dbl(x, prod, .before = before, na_rm = T)
  )
})

# ------------------------------------------------------------------------------
# slide_mean()

test_that("precision matches base R (long doubles)", {
  x <- c(1/7, 1/7, 1/3)
  expect_identical(mean(x), slide_mean(x, Inf)[[length(x)]])
})

test_that("Inf + -Inf = NaN propagates with `na_rm = TRUE`", {
  x <- c(-Inf, Inf, rep(1, SEGMENT_TREE_FANOUT - 2L))
  before <- SEGMENT_TREE_FANOUT - 1L

  expect_identical(
    slide_mean(x, before = before, na_rm = T),
    slide_dbl(x, mean, .before = before, na_rm = T)
  )
})

# ------------------------------------------------------------------------------
# All

test_that("names are kept (even on casting)", {
  expect_named(slide_sum(c(x = 1, y = 2), before = 1), c("x", "y"))
  expect_named(slide_sum(c(x = 1L, y = 2L), before = 1), c("x", "y"))
})

test_that("can cast integer and logical input", {
  expect_identical(slide_sum(1:5, 1), slide_sum(1:5 + 0, 1))
  expect_identical(slide_sum(c(TRUE, FALSE, TRUE), 1), slide_sum(c(1, 0, 1), 1))
})

test_that("types that can't be cast to numeric are not supported", {
  expect_error(slide_sum("x"), class = "vctrs_error_incompatible_type")
})

test_that("arrays of dimensionality 1 are supported", {
  expect_identical(
    slide_sum(array(1:5), 1),
    slide_sum(1:5, 1)
  )
})

test_that("arrays of dimensionality >1 are not supported", {
  expect_error(slide_sum(array(1:3, dim = c(2, 2)), 1), class = "vctrs_error_incompatible_type")
})

test_that("works when the window is completely OOB", {
  expect_identical(
    slide_sum(1:3, before = 4, after = -4),
    c(NA_real_, NA_real_, NA_real_)
  )
})

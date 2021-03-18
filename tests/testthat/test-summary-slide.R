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
  # The NA / NaN ordering is platform dependent
  # expect_identical(
  #   slide_sum(rev(y), before = 3),
  #   slide_dbl(rev(y), sum, .before = 3)
  # )
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
  skip_if_no_long_double()
  x <- rep(1/7, 10)
  expect_identical(sum(x), slide_sum(x, before = Inf)[[length(x)]])
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
  # The NA / NaN ordering is platform dependent
  # expect_identical(
  #   slide_prod(rev(y), before = 3),
  #   slide_dbl(rev(y), prod, .before = 3)
  # )
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

test_that("integer before works", {
  x <- 1:4 + 0

  expect_identical(slide_mean(x, before = 1), slide_dbl(x, mean, .before = 1))
  expect_identical(slide_mean(x, before = 2), slide_dbl(x, mean, .before = 2))
})

test_that("integer after works", {
  x <- 1:4 + 0

  expect_identical(slide_mean(x, after = 1), slide_dbl(x, mean, .after = 1))
  expect_identical(slide_mean(x, after = 2), slide_dbl(x, mean, .after = 2))
})

test_that("negative before/after works", {
  x <- 1:4 + 0

  expect_identical(slide_mean(x, before = -1, after = 2), slide_dbl(x, mean, .before = -1, .after = 2))
  expect_identical(slide_mean(x, before = 2, after = -1), slide_dbl(x, mean, .before = 2, .after = -1))

  expect_identical(slide_mean(x, before = -1, after = 2, complete = TRUE), slide_dbl(x, mean, .before = -1, .after = 2, .complete = TRUE))
  expect_identical(slide_mean(x, before = 2, after = -1, complete = TRUE), slide_dbl(x, mean, .before = 2, .after = -1, .complete = TRUE))
})

test_that("`Inf` before/after works", {
  x <- 1:4 + 0

  expect_identical(slide_mean(x, before = Inf), slide_dbl(x, mean, .before = Inf))
  expect_identical(slide_mean(x, after = Inf), slide_dbl(x, mean, .after = Inf))
})

test_that("step / complete works", {
  x <- 1:4 + 0

  expect_identical(slide_mean(x, before = 1, step = 2), slide_dbl(x, mean, .before = 1, .step = 2))
  expect_identical(slide_mean(x, before = 1, step = 2, complete = TRUE), slide_dbl(x, mean, .before = 1, .step = 2, .complete = TRUE))
})

test_that("NA / NaN results are correct", {
  x <- c(rep(1, 10), rep(NA, 10), 1:4)
  y <- c(rep(NA, 10), rep(NaN, 10), 1:4)

  expect_identical(
    slide_mean(x, before = 3),
    slide_dbl(x, mean, .before = 3)
  )
  expect_identical(
    slide_mean(y, before = 3),
    slide_dbl(y, mean, .before = 3)
  )
  # The NA / NaN ordering is platform dependent
  # expect_identical(
  #   slide_mean(rev(y), before = 3),
  #   slide_dbl(rev(y), mean, .before = 3)
  # )
})

test_that("`na_rm = TRUE` works", {
  x <- NA
  y <- c(1, NA, 2, 3)

  expect_identical(slide_mean(x, na_rm = TRUE), NaN)
  expect_identical(slide_mean(y, na_rm = TRUE, before = 1), c(1, 1, 2, 2.5))
})

test_that("Inf and -Inf results are correct", {
  x <- c(1, Inf, -Inf, 1)
  expect_identical(slide_mean(x, before = 1), c(1, Inf, NaN, -Inf))
})

test_that("precision matches base R (long doubles)", {
  skip_if_no_long_double()
  x <- c(1/7, 1/7, 1/3)
  expect_identical(mean(x), slide_mean(x, before = Inf)[[length(x)]])
})

test_that("Inf + -Inf = NaN propagates with `na_rm = TRUE`", {
  x <- c(-Inf, Inf, rep(1, SEGMENT_TREE_FANOUT - 2L))
  before <- SEGMENT_TREE_FANOUT - 1L

  expect_identical(
    slide_mean(x, before = before, na_rm = T),
    slide_dbl(x, mean, .before = before, na_rm = T)
  )
})

test_that("computes correctly with wider width", {
  expect_identical(
    slide_mean(1:1000, before = 100),
    slide_dbl(1:1000, mean, .before = 100)
  )
})

# ------------------------------------------------------------------------------
# slide_min()

test_that("integer before works", {
  x <- 1:4 + 0

  expect_identical(slide_min(x, before = 1), slide_dbl(x, min, .before = 1))
  expect_identical(slide_min(x, before = 2), slide_dbl(x, min, .before = 2))
})

test_that("integer after works", {
  x <- 1:4 + 0

  expect_identical(slide_min(x, after = 1), slide_dbl(x, min, .after = 1))
  expect_identical(slide_min(x, after = 2), slide_dbl(x, min, .after = 2))
})

test_that("negative before/after works", {
  x <- 1:4 + 0

  expect_identical(slide_min(x, before = -1, after = 2), c(2, 3, 4, Inf))
  expect_identical(slide_min(x, before = 2, after = -1), c(Inf, 1, 1, 2))

  expect_identical(slide_min(x, before = -1, after = 2, complete = TRUE), slide_dbl(x, min, .before = -1, .after = 2, .complete = TRUE))
  expect_identical(slide_min(x, before = 2, after = -1, complete = TRUE), slide_dbl(x, min, .before = 2, .after = -1, .complete = TRUE))
})

test_that("`Inf` before/after works", {
  x <- 1:4 + 0

  expect_identical(slide_min(x, before = Inf), slide_dbl(x, min, .before = Inf))
  expect_identical(slide_min(x, after = Inf), slide_dbl(x, min, .after = Inf))
})

test_that("step / complete works", {
  x <- 1:4 + 0

  expect_identical(slide_min(x, before = 1, step = 2), slide_dbl(x, min, .before = 1, .step = 2))
  expect_identical(slide_min(x, before = 1, step = 2, complete = TRUE), slide_dbl(x, min, .before = 1, .step = 2, .complete = TRUE))
})

test_that("NA / NaN results are correct", {
  x <- c(rep(1, 10), rep(NA, 10), 1:4)
  y <- c(rep(NA, 10), rep(NaN, 10), 1:4)

  expect_identical(
    slide_min(x, before = 3),
    slide_dbl(x, min, .before = 3)
  )
  expect_identical(
    slide_min(y, before = 3),
    slide_dbl(y, min, .before = 3)
  )
  expect_identical(
    slide_min(rev(y), before = 3),
    slide_dbl(rev(y), min, .before = 3)
  )
})

test_that("`na_rm = TRUE` works", {
  x <- NA
  y <- c(1, NA, 2, 3)

  expect_identical(slide_min(x, na_rm = TRUE), Inf)
  expect_identical(slide_min(y, na_rm = TRUE, before = 1), c(1, 1, 2, 2))
})

test_that("Inf and -Inf results are correct", {
  x <- c(1, Inf, -Inf, 1)
  expect_identical(slide_min(x, before = 1), c(1, 1, -Inf, -Inf))
})

# ------------------------------------------------------------------------------
# slide_max()

test_that("integer before works", {
  x <- 1:4 + 0

  expect_identical(slide_max(x, before = 1), slide_dbl(x, max, .before = 1))
  expect_identical(slide_max(x, before = 2), slide_dbl(x, max, .before = 2))
})

test_that("integer after works", {
  x <- 1:4 + 0

  expect_identical(slide_max(x, after = 1), slide_dbl(x, max, .after = 1))
  expect_identical(slide_max(x, after = 2), slide_dbl(x, max, .after = 2))
})

test_that("negative before/after works", {
  x <- 1:4 + 0

  expect_identical(slide_max(x, before = -1, after = 2), c(3, 4, 4, -Inf))
  expect_identical(slide_max(x, before = 2, after = -1), c(-Inf, 1, 2, 3))

  expect_identical(slide_max(x, before = -1, after = 2, complete = TRUE), slide_dbl(x, max, .before = -1, .after = 2, .complete = TRUE))
  expect_identical(slide_max(x, before = 2, after = -1, complete = TRUE), slide_dbl(x, max, .before = 2, .after = -1, .complete = TRUE))
})

test_that("`Inf` before/after works", {
  x <- 1:4 + 0

  expect_identical(slide_max(x, before = Inf), slide_dbl(x, max, .before = Inf))
  expect_identical(slide_max(x, after = Inf), slide_dbl(x, max, .after = Inf))
})

test_that("step / complete works", {
  x <- 1:4 + 0

  expect_identical(slide_max(x, before = 1, step = 2), slide_dbl(x, max, .before = 1, .step = 2))
  expect_identical(slide_max(x, before = 1, step = 2, complete = TRUE), slide_dbl(x, max, .before = 1, .step = 2, .complete = TRUE))
})

test_that("NA / NaN results are correct", {
  x <- c(rep(1, 10), rep(NA, 10), 1:4)
  y <- c(rep(NA, 10), rep(NaN, 10), 1:4)

  expect_identical(
    slide_max(x, before = 3),
    slide_dbl(x, max, .before = 3)
  )
  expect_identical(
    slide_max(y, before = 3),
    slide_dbl(y, max, .before = 3)
  )
  expect_identical(
    slide_max(rev(y), before = 3),
    slide_dbl(rev(y), max, .before = 3)
  )
})

test_that("`na_rm = TRUE` works", {
  x <- NA
  y <- c(1, NA, 2, 3)

  expect_identical(slide_max(x, na_rm = TRUE), -Inf)
  expect_identical(slide_max(y, na_rm = TRUE, before = 1), c(1, 1, 2, 3))
})

test_that("Inf and -Inf results are correct", {
  x <- c(1, Inf, -Inf, 1)
  expect_identical(slide_max(x, before = 1), c(1, Inf, Inf, 1))
})

# ------------------------------------------------------------------------------
# slide_all()

test_that("integer before works", {
  x <- c(TRUE, FALSE, TRUE, TRUE)

  expect_identical(slide_all(x, before = 1), slide_lgl(x, all, .before = 1))
  expect_identical(slide_all(x, before = 2), slide_lgl(x, all, .before = 2))
})

test_that("integer after works", {
  x <- c(TRUE, FALSE, TRUE, TRUE)

  expect_identical(slide_all(x, after = 1), slide_lgl(x, all, .after = 1))
  expect_identical(slide_all(x, after = 2), slide_lgl(x, all, .after = 2))
})

test_that("negative before/after works", {
  x <- c(TRUE, FALSE, TRUE, TRUE)

  expect_identical(slide_all(x, before = -1, after = 2), slide_lgl(x, all, .before = -1, .after = 2))
  expect_identical(slide_all(x, before = 2, after = -1), slide_lgl(x, all, .before = 2, .after = -1))

  expect_identical(slide_all(x, before = -1, after = 2, complete = TRUE), slide_lgl(x, all, .before = -1, .after = 2, .complete = TRUE))
  expect_identical(slide_all(x, before = 2, after = -1, complete = TRUE), slide_lgl(x, all, .before = 2, .after = -1, .complete = TRUE))
})

test_that("`Inf` before/after works", {
  x <- c(TRUE, FALSE, TRUE, TRUE)

  expect_identical(slide_all(x, before = Inf), slide_lgl(x, all, .before = Inf))
  expect_identical(slide_all(x, after = Inf), slide_lgl(x, all, .after = Inf))
})

test_that("step / complete works", {
  x <- c(TRUE, FALSE, TRUE, TRUE)

  expect_identical(slide_all(x, before = 1, step = 2), slide_lgl(x, all, .before = 1, .step = 2))
  expect_identical(slide_all(x, before = 1, step = 2, complete = TRUE), slide_lgl(x, all, .before = 1, .step = 2, .complete = TRUE))
})

test_that("NA / NaN results are correct", {
  x <- c(rep(TRUE, 10), rep(NA, 10), c(TRUE, TRUE, FALSE, TRUE))

  expect_identical(
    slide_all(x, before = 3),
    slide_lgl(x, all, .before = 3)
  )
})

test_that("FALSE dominates NAs, matching all()", {
  x <- c(NA, FALSE, FALSE)
  expect_identical(slide_all(x, before = 2), c(NA, FALSE, FALSE))
  expect_identical(slide_all(x, before = 2), slide_lgl(x, all, .before = 2))

  x <- c(FALSE, NA, FALSE)
  expect_identical(slide_all(x, before = 2), c(FALSE, FALSE, FALSE))
  expect_identical(slide_all(x, before = 2), slide_lgl(x, all, .before = 2))

  x <- c(FALSE, FALSE, NA)
  expect_identical(slide_all(x, before = 2), c(FALSE, FALSE, FALSE))
  expect_identical(slide_all(x, before = 2), slide_lgl(x, all, .before = 2))
})

test_that("`na_rm = TRUE` works", {
  x <- NA
  y <- c(TRUE, NA, FALSE, NA, TRUE)

  expect_identical(slide_all(x, na_rm = TRUE), TRUE)
  expect_identical(slide_all(y, na_rm = TRUE, before = 1), slide_lgl(y, all, na.rm = TRUE, .before = 1))
})

test_that("works when the window is completely OOB", {
  x <- c(TRUE, FALSE, NA)

  expect_identical(slide_all(x, before = 4, after = -4), c(TRUE, TRUE, TRUE))
  expect_identical(slide_all(x, before = 4, after = -4), slide_lgl(x, all, .before = 4, .after = -4))
})

test_that("input must be castable to logical", {
  expect_error(slide_all(1:5), class = "vctrs_error_cast_lossy")
})

# ------------------------------------------------------------------------------
# slide_any()

test_that("integer before works", {
  x <- c(FALSE, TRUE, FALSE, FALSE)

  expect_identical(slide_any(x, before = 1), slide_lgl(x, any, .before = 1))
  expect_identical(slide_any(x, before = 2), slide_lgl(x, any, .before = 2))
})

test_that("integer after works", {
  x <- c(FALSE, TRUE, FALSE, FALSE)

  expect_identical(slide_any(x, after = 1), slide_lgl(x, any, .after = 1))
  expect_identical(slide_any(x, after = 2), slide_lgl(x, any, .after = 2))
})

test_that("negative before/after works", {
  x <- c(FALSE, TRUE, FALSE, FALSE)

  expect_identical(slide_any(x, before = -1, after = 2), slide_lgl(x, any, .before = -1, .after = 2))
  expect_identical(slide_any(x, before = 2, after = -1), slide_lgl(x, any, .before = 2, .after = -1))

  expect_identical(slide_any(x, before = -1, after = 2, complete = TRUE), slide_lgl(x, any, .before = -1, .after = 2, .complete = TRUE))
  expect_identical(slide_any(x, before = 2, after = -1, complete = TRUE), slide_lgl(x, any, .before = 2, .after = -1, .complete = TRUE))
})

test_that("`Inf` before/after works", {
  x <- c(FALSE, TRUE, FALSE, FALSE)

  expect_identical(slide_any(x, before = Inf), slide_lgl(x, any, .before = Inf))
  expect_identical(slide_any(x, after = Inf), slide_lgl(x, any, .after = Inf))
})

test_that("step / complete works", {
  x <- c(FALSE, TRUE, FALSE, FALSE)

  expect_identical(slide_any(x, before = 1, step = 2), slide_lgl(x, any, .before = 1, .step = 2))
  expect_identical(slide_any(x, before = 1, step = 2, complete = TRUE), slide_lgl(x, any, .before = 1, .step = 2, .complete = TRUE))
})

test_that("NA results are correct", {
  x <- c(rep(FALSE, 10), rep(NA, 10), c(FALSE, FALSE, TRUE, FALSE))

  expect_identical(
    slide_any(x, before = 3),
    slide_lgl(x, any, .before = 3)
  )
})

test_that("TRUE dominates NAs, matching any()", {
  x <- c(NA, TRUE, TRUE)
  expect_identical(slide_any(x, before = 2), c(NA, TRUE, TRUE))
  expect_identical(slide_any(x, before = 2), slide_lgl(x, any, .before = 2))

  x <- c(TRUE, NA, TRUE)
  expect_identical(slide_any(x, before = 2), c(TRUE, TRUE, TRUE))
  expect_identical(slide_any(x, before = 2), slide_lgl(x, any, .before = 2))

  x <- c(TRUE, TRUE, NA)
  expect_identical(slide_any(x, before = 2), c(TRUE, TRUE, TRUE))
  expect_identical(slide_any(x, before = 2), slide_lgl(x, any, .before = 2))
})

test_that("`na_rm = TRUE` works", {
  x <- NA
  y <- c(TRUE, NA, FALSE, NA, TRUE)

  expect_identical(slide_any(x, na_rm = TRUE), FALSE)
  expect_identical(slide_any(y, na_rm = TRUE, before = 1), slide_lgl(y, any, na.rm = TRUE, .before = 1))
})

test_that("works when the window is completely OOB", {
  x <- c(TRUE, FALSE, NA)

  expect_identical(slide_any(x, before = 4, after = -4), c(FALSE, FALSE, FALSE))
  expect_identical(slide_any(x, before = 4, after = -4), slide_lgl(x, any, .before = 4, .after = -4))
})

test_that("input must be castable to logical", {
  expect_error(slide_any(1:5), class = "vctrs_error_cast_lossy")
})

# ------------------------------------------------------------------------------
# Misc

test_that("works with size 0 input", {
  expect_identical(slide_sum(integer()), double())
  expect_identical(slide_sum(integer(), before = 5, after = 1), double())
  expect_identical(slide_sum(integer(), step = 2, na_rm = TRUE), double())
})

test_that("names are kept (even on casting)", {
  expect_named(slide_sum(c(x = 1, y = 2), before = 1), c("x", "y"))
  expect_named(slide_sum(c(x = 1L, y = 2L), before = 1), c("x", "y"))
})

test_that("can cast integer and logical input", {
  expect_identical(slide_sum(1:5, before = 1), slide_sum(1:5 + 0, before = 1))
  expect_identical(slide_sum(c(TRUE, FALSE, TRUE), before = 1), slide_sum(c(1, 0, 1), before = 1))
})

test_that("types that can't be cast to numeric are not supported", {
  expect_error(slide_sum("x"), class = "vctrs_error_incompatible_type")
})

test_that("arrays of dimensionality 1 are supported", {
  expect_identical(
    slide_sum(array(1:5), before = 1),
    slide_sum(1:5, before = 1)
  )
})

test_that("arrays of dimensionality >1 are not supported", {
  expect_error(slide_sum(array(1:4, dim = c(2, 2)), before = 1), class = "vctrs_error_incompatible_type")
})

test_that("works when the window is completely OOB", {
  expect_identical(
    slide_sum(1:3, before = 4, after = -4),
    c(0, 0, 0)
  )
})

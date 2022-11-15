# ------------------------------------------------------------------------------
# slide_index_sum()

test_that("integer before works", {
  x <- 1:4 + 0
  i <- c(1, 2, 4, 5)

  expect_identical(slide_index_sum(x, i, before = 1), slide_index_dbl(x, i, sum, .before = 1))
  expect_identical(slide_index_sum(x, i, before = 2), slide_index_dbl(x, i, sum, .before = 2))
})

test_that("integer after works", {
  x <- 1:4 + 0
  i <- c(1, 2, 4, 5)

  expect_identical(slide_index_sum(x, i, after = 1), slide_index_dbl(x, i, sum, .after = 1))
  expect_identical(slide_index_sum(x, i, after = 2), slide_index_dbl(x, i, sum, .after = 2))
})

test_that("negative before/after works", {
  x <- 1:4 + 0
  i <- c(1, 2, 4, 5)

  expect_identical(slide_index_sum(x, i, before = -1, after = 2), slide_index_dbl(x, i, sum, .before = -1, .after = 2))
  expect_identical(slide_index_sum(x, i, before = 2, after = -1), slide_index_dbl(x, i, sum, .before = 2, .after = -1))

  expect_identical(slide_index_sum(x, i, before = -1, after = 2, complete = TRUE), slide_index_dbl(x, i, sum, .before = -1, .after = 2, .complete = TRUE))
  expect_identical(slide_index_sum(x, i, before = 2, after = -1, complete = TRUE), slide_index_dbl(x, i, sum, .before = 2, .after = -1, .complete = TRUE))
})

test_that("`Inf` before/after works", {
  x <- 1:4 + 0
  i <- c(1, 2, 4, 5)

  expect_identical(slide_index_sum(x, i, before = Inf), slide_index_dbl(x, i, sum, .before = Inf))
  expect_identical(slide_index_sum(x, i, after = Inf), slide_index_dbl(x, i, sum, .after = Inf))
})

test_that("NA / NaN results are correct", {
  x <- c(rep(1, 10), rep(NA, 10), 1:4)
  y <- c(rep(NA, 10), rep(NaN, 10), 1:4)
  i <- seq_along(x)

  expect_identical(
    slide_index_sum(x, i, before = 3),
    slide_index_dbl(x, i, sum, .before = 3)
  )
  expect_identical(
    slide_index_sum(y, i, before = 3),
    slide_index_dbl(y, i, sum, .before = 3)
  )
  # The NA / NaN ordering is platform dependent
  # expect_identical(
  #   slide_index_sum(rev(y), i, before = 3),
  #   slide_index_dbl(rev(y), i, sum, .before = 3)
  # )
})

test_that("`na_rm = TRUE` works", {
  x <- NA
  y <- c(1, NA, 2, 3)

  expect_identical(slide_index_sum(x, 1, na_rm = TRUE), 0)
  expect_identical(slide_index_sum(y, 1:4, na_rm = TRUE, before = 1), c(1, 1, 2, 5))
})

test_that("Inf and -Inf results are correct", {
  x <- c(1, Inf, -Inf, 1)
  i <- seq_along(x)
  expect_identical(slide_index_sum(x, i, before = 1), c(1, Inf, NaN, -Inf))
})

# ------------------------------------------------------------------------------
# slide_index_prod()

test_that("integer before works", {
  x <- 1:4 + 0
  i <- c(1, 2, 4, 5)

  expect_identical(slide_index_prod(x, i, before = 1), slide_index_dbl(x, i, prod, .before = 1))
  expect_identical(slide_index_prod(x, i, before = 2), slide_index_dbl(x, i, prod, .before = 2))
})

test_that("integer after works", {
  x <- 1:4 + 0
  i <- c(1, 2, 4, 5)

  expect_identical(slide_index_prod(x, i, after = 1), slide_index_dbl(x, i, prod, .after = 1))
  expect_identical(slide_index_prod(x, i, after = 2), slide_index_dbl(x, i, prod, .after = 2))
})

test_that("negative before/after works", {
  x <- 1:4 + 0
  i <- c(1, 2, 4, 5)

  expect_identical(slide_index_prod(x, i, before = -1, after = 2), slide_index_dbl(x, i, prod, .before = -1, .after = 2))
  expect_identical(slide_index_prod(x, i, before = 2, after = -1), slide_index_dbl(x, i, prod, .before = 2, .after = -1))

  expect_identical(slide_index_prod(x, i, before = -1, after = 2, complete = TRUE), slide_index_dbl(x, i, prod, .before = -1, .after = 2, .complete = TRUE))
  expect_identical(slide_index_prod(x, i, before = 2, after = -1, complete = TRUE), slide_index_dbl(x, i, prod, .before = 2, .after = -1, .complete = TRUE))
})

test_that("`Inf` before/after works", {
  x <- 1:4 + 0
  i <- c(1, 2, 4, 5)

  expect_identical(slide_index_prod(x, i, before = Inf), slide_index_dbl(x, i, prod, .before = Inf))
  expect_identical(slide_index_prod(x, i, after = Inf), slide_index_dbl(x, i, prod, .after = Inf))
})

test_that("NA / NaN results are correct", {
  x <- c(rep(1, 10), rep(NA, 10), 1:4)
  y <- c(rep(NA, 10), rep(NaN, 10), 1:4)
  i <- seq_along(x)

  expect_identical(
    slide_index_prod(x, i, before = 3),
    slide_index_dbl(x, i, prod, .before = 3)
  )
  expect_identical(
    slide_index_prod(y, i, before = 3),
    slide_index_dbl(y, i, prod, .before = 3)
  )
  # The NA / NaN ordering is platform dependent
  # expect_identical(
  #   slide_index_prod(rev(y), i, before = 3),
  #   slide_index_dbl(rev(y), i, prod, .before = 3)
  # )
})

test_that("`na_rm = TRUE` works", {
  x <- NA
  y <- c(1, NA, 2, 3)

  expect_identical(slide_index_prod(x, 1, na_rm = TRUE), 1)
  expect_identical(slide_index_prod(y, 1:4, na_rm = TRUE, before = 1), c(1, 1, 2, 6))
})

test_that("Inf and -Inf results are correct", {
  x <- c(1, Inf, -Inf, 0)
  expect_identical(slide_index_prod(x, 1:4, before = 1), c(1, Inf, -Inf, NaN))
})

# ------------------------------------------------------------------------------
# slide_index_mean()

test_that("integer before works", {
  x <- 1:4 + 0
  i <- c(1, 2, 4, 5)

  expect_identical(slide_index_mean(x, i, before = 1), slide_index_dbl(x, i, mean, .before = 1))
  expect_identical(slide_index_mean(x, i, before = 2), slide_index_dbl(x, i, mean, .before = 2))
})

test_that("integer after works", {
  x <- 1:4 + 0
  i <- c(1, 2, 4, 5)

  expect_identical(slide_index_mean(x, i, after = 1), slide_index_dbl(x, i, mean, .after = 1))
  expect_identical(slide_index_mean(x, i, after = 2), slide_index_dbl(x, i, mean, .after = 2))
})

test_that("negative before/after works", {
  x <- 1:4 + 0
  i <- c(1, 2, 4, 5)

  expect_identical(slide_index_mean(x, i, before = -1, after = 2), slide_index_dbl(x, i, mean, .before = -1, .after = 2))
  expect_identical(slide_index_mean(x, i, before = 2, after = -1), slide_index_dbl(x, i, mean, .before = 2, .after = -1))

  expect_identical(slide_index_mean(x, i, before = -1, after = 2, complete = TRUE), slide_index_dbl(x, i, mean, .before = -1, .after = 2, .complete = TRUE))
  expect_identical(slide_index_mean(x, i, before = 2, after = -1, complete = TRUE), slide_index_dbl(x, i, mean, .before = 2, .after = -1, .complete = TRUE))
})

test_that("`Inf` before/after works", {
  x <- 1:4 + 0
  i <- c(1, 2, 4, 5)

  expect_identical(slide_index_mean(x, i, before = Inf), slide_index_dbl(x, i, mean, .before = Inf))
  expect_identical(slide_index_mean(x, i, after = Inf), slide_index_dbl(x, i, mean, .after = Inf))
})

test_that("NA / NaN results are correct", {
  x <- c(rep(1, 10), rep(NA, 10), 1:4)
  y <- c(rep(NA, 10), rep(NaN, 10), 1:4)
  i <- seq_along(x)

  expect_identical(
    slide_index_mean(x, i, before = 3),
    slide_index_dbl(x, i, mean, .before = 3)
  )
  expect_identical(
    slide_index_mean(y, i, before = 3),
    slide_index_dbl(y, i, mean, .before = 3)
  )
  # The NA / NaN ordering is platform dependent
  # expect_identical(
  #   slide_index_mean(rev(y), i, before = 3),
  #   slide_index_dbl(rev(y), i, mean, .before = 3)
  # )
})

test_that("`na_rm = TRUE` works", {
  x <- NA
  y <- c(1, NA, 2, 3)

  expect_identical(slide_index_mean(x, 1, na_rm = TRUE), NaN)
  expect_identical(slide_index_mean(y, 1:4, na_rm = TRUE, before = 1), c(1, 1, 2, 2.5))
})

test_that("Inf and -Inf results are correct", {
  x <- c(1, Inf, -Inf, 1)
  expect_identical(slide_index_mean(x, 1:4, before = 1), c(1, Inf, NaN, -Inf))
})

# ------------------------------------------------------------------------------
# slide_index_min()

test_that("integer before works", {
  x <- 1:4 + 0
  i <- c(1, 2, 4, 5)

  expect_identical(slide_index_min(x, i, before = 1), slide_index_dbl(x, i, min, .before = 1))
  expect_identical(slide_index_min(x, i, before = 2), slide_index_dbl(x, i, min, .before = 2))
})

test_that("integer after works", {
  x <- 1:4 + 0
  i <- c(1, 2, 4, 5)

  expect_identical(slide_index_min(x, i, after = 1), slide_index_dbl(x, i, min, .after = 1))
  expect_identical(slide_index_min(x, i, after = 2), slide_index_dbl(x, i, min, .after = 2))
})

test_that("negative before/after works", {
  x <- 1:4 + 0
  i <- c(1, 2, 4, 5)

  expect_identical(slide_index_min(x, i, before = -1, after = 2), c(2, 3, 4, Inf))
  expect_identical(slide_index_min(x, i, before = 2, after = -1), c(Inf, 1, 2, 3))

  expect_identical(slide_index_min(x, i, before = -1, after = 2, complete = TRUE), slide_index_dbl(x, i, min, .before = -1, .after = 2, .complete = TRUE))
  expect_identical(slide_index_min(x, i, before = 2, after = -1, complete = TRUE), slide_index_dbl(x, i, min, .before = 2, .after = -1, .complete = TRUE))
})

test_that("`Inf` before/after works", {
  x <- 1:4 + 0
  i <- c(1, 2, 4, 5)

  expect_identical(slide_index_min(x, i, before = Inf), slide_index_dbl(x, i, min, .before = Inf))
  expect_identical(slide_index_min(x, i, after = Inf), slide_index_dbl(x, i, min, .after = Inf))
})

test_that("NA / NaN results are correct", {
  x <- c(rep(1, 10), rep(NA, 10), 1:4)
  y <- c(rep(NA, 10), rep(NaN, 10), 1:4)
  i <- seq_along(x)

  expect_identical(
    slide_index_min(x, i, before = 3),
    slide_index_dbl(x, i, min, .before = 3)
  )
  expect_identical(
    slide_index_min(y, i, before = 3),
    slide_index_dbl(y, i, min, .before = 3)
  )
  expect_identical(
    slide_index_min(rev(y), i, before = 3),
    slide_index_dbl(rev(y), i, min, .before = 3)
  )
})

test_that("`na_rm = TRUE` works", {
  x <- NA
  y <- c(1, NA, 2, 3)

  expect_identical(slide_index_min(x, 1, na_rm = TRUE), Inf)
  expect_identical(slide_index_min(y, 1:4, na_rm = TRUE, before = 1), c(1, 1, 2, 2))
})

test_that("Inf and -Inf results are correct", {
  x <- c(1, Inf, -Inf, 1)
  expect_identical(slide_index_min(x, 1:4, before = 1), c(1, 1, -Inf, -Inf))
})

# ------------------------------------------------------------------------------
# slide_index_max()

test_that("integer before works", {
  x <- 1:4 + 0
  i <- c(1, 2, 4, 5)

  expect_identical(slide_index_max(x, i, before = 1), slide_index_dbl(x, i, max, .before = 1))
  expect_identical(slide_index_max(x, i, before = 2), slide_index_dbl(x, i, max, .before = 2))
})

test_that("integer after works", {
  x <- 1:4 + 0
  i <- c(1, 2, 4, 5)

  expect_identical(slide_index_max(x, i, after = 1), slide_index_dbl(x, i, max, .after = 1))
  expect_identical(slide_index_max(x, i, after = 2), slide_index_dbl(x, i, max, .after = 2))
})

test_that("negative before/after works", {
  x <- 1:4 + 0
  i <- c(1, 2, 4, 5)

  expect_identical(slide_index_max(x, i, before = -1, after = 2), c(2, 3, 4, -Inf))
  expect_identical(slide_index_max(x, i, before = 2, after = -1), c(-Inf, 1, 2, 3))

  expect_identical(slide_index_max(x, i, before = -1, after = 2, complete = TRUE), slide_index_dbl(x, i, max, .before = -1, .after = 2, .complete = TRUE))
  expect_identical(slide_index_max(x, i, before = 2, after = -1, complete = TRUE), slide_index_dbl(x, i, max, .before = 2, .after = -1, .complete = TRUE))
})

test_that("`Inf` before/after works", {
  x <- 1:4 + 0
  i <- c(1, 2, 4, 5)

  expect_identical(slide_index_max(x, i, before = Inf), slide_index_dbl(x, i, max, .before = Inf))
  expect_identical(slide_index_max(x, i, after = Inf), slide_index_dbl(x, i, max, .after = Inf))
})

test_that("NA / NaN results are correct", {
  x <- c(rep(1, 10), rep(NA, 10), 1:4)
  y <- c(rep(NA, 10), rep(NaN, 10), 1:4)
  i <- seq_along(x)

  expect_identical(
    slide_index_max(x, i, before = 3),
    slide_index_dbl(x, i, max, .before = 3)
  )
  expect_identical(
    slide_index_max(y, i, before = 3),
    slide_index_dbl(y, i, max, .before = 3)
  )
  expect_identical(
    slide_index_max(rev(y), i, before = 3),
    slide_index_dbl(rev(y), i, max, .before = 3)
  )
})

test_that("`na_rm = TRUE` works", {
  x <- NA
  y <- c(1, NA, 2, 3)

  expect_identical(slide_index_max(x, 1, na_rm = TRUE), -Inf)
  expect_identical(slide_index_max(y, 1:4, na_rm = TRUE, before = 1), c(1, 1, 2, 3))
})

test_that("Inf and -Inf results are correct", {
  x <- c(1, Inf, -Inf, 1)
  expect_identical(slide_index_max(x, 1:4, before = 1), c(1, Inf, Inf, 1))
})

# ------------------------------------------------------------------------------
# slide_index_all()

test_that("integer before works", {
  x <- c(TRUE, FALSE, TRUE, TRUE)
  i <- c(1, 2, 4, 5)

  expect_identical(slide_index_all(x, i, before = 1), slide_index_lgl(x, i, all, .before = 1))
  expect_identical(slide_index_all(x, i, before = 2), slide_index_lgl(x, i, all, .before = 2))
})

test_that("integer after works", {
  x <- c(TRUE, FALSE, TRUE, TRUE)
  i <- c(1, 2, 4, 5)

  expect_identical(slide_index_all(x, i, after = 1), slide_index_lgl(x, i, all, .after = 1))
  expect_identical(slide_index_all(x, i, after = 2), slide_index_lgl(x, i, all, .after = 2))
})

test_that("negative before/after works", {
  x <- c(TRUE, FALSE, TRUE, TRUE)
  i <- c(1, 2, 4, 5)

  expect_identical(slide_index_all(x, i, before = -1, after = 2), slide_index_lgl(x, i, all, .before = -1, .after = 2))
  expect_identical(slide_index_all(x, i, before = 2, after = -1), slide_index_lgl(x, i, all, .before = 2, .after = -1))

  expect_identical(slide_index_all(x, i, before = -1, after = 2, complete = TRUE), slide_index_lgl(x, i, all, .before = -1, .after = 2, .complete = TRUE))
  expect_identical(slide_index_all(x, i, before = 2, after = -1, complete = TRUE), slide_index_lgl(x, i, all, .before = 2, .after = -1, .complete = TRUE))
})

test_that("`Inf` before/after works", {
  x <- c(TRUE, FALSE, TRUE, TRUE)
  i <- c(1, 2, 4, 5)

  expect_identical(slide_index_all(x, i, before = Inf), slide_index_lgl(x, i, all, .before = Inf))
  expect_identical(slide_index_all(x, i, after = Inf), slide_index_lgl(x, i, all, .after = Inf))
})

test_that("NA / NaN results are correct", {
  x <- c(rep(TRUE, 10), rep(NA, 10), c(TRUE, TRUE, FALSE, TRUE))
  i <- seq_along(x)

  expect_identical(
    slide_index_all(x, i, before = 3),
    slide_index_lgl(x, i, all, .before = 3)
  )
})

test_that("FALSE dominates NAs, matching all()", {
  i <- c(1, 2, 3)

  x <- c(NA, FALSE, FALSE)
  expect_identical(slide_index_all(x, i, before = 2), c(NA, FALSE, FALSE))
  expect_identical(slide_index_all(x, i, before = 2), slide_index_lgl(x, i, all, .before = 2))

  x <- c(FALSE, NA, FALSE)
  expect_identical(slide_index_all(x, i, before = 2), c(FALSE, FALSE, FALSE))
  expect_identical(slide_index_all(x, i, before = 2), slide_index_lgl(x, i, all, .before = 2))

  x <- c(FALSE, FALSE, NA)
  expect_identical(slide_index_all(x, i, before = 2), c(FALSE, FALSE, FALSE))
  expect_identical(slide_index_all(x, i, before = 2), slide_index_lgl(x, i, all, .before = 2))
})

test_that("`na_rm = TRUE` works", {
  x <- NA
  i <- 1L
  expect_identical(slide_index_all(x, i, na_rm = TRUE), TRUE)

  y <- c(TRUE, NA, FALSE, NA, TRUE)
  i <- seq_along(y)
  expect_identical(slide_index_all(y, i, na_rm = TRUE, before = 1), slide_index_lgl(y, i, all, na.rm = TRUE, .before = 1))
})

test_that("works when the window is completely OOB", {
  x <- c(TRUE, FALSE, NA)
  i <- seq_along(x)

  expect_identical(slide_index_all(x, i, before = 4, after = -4), c(TRUE, TRUE, TRUE))
  expect_identical(slide_index_all(x, i, before = 4, after = -4), slide_index_lgl(x, i, all, .before = 4, .after = -4))
})

test_that("input must be castable to logical", {
  expect_snapshot({
    (expect_error(slide_index_all(1:5, 1:5), class = "vctrs_error_cast_lossy"))
  })
})

# ------------------------------------------------------------------------------
# slide_index_any()

test_that("integer before works", {
  x <- c(FALSE, TRUE, FALSE, FALSE)
  i <- c(1, 2, 4, 5)

  expect_identical(slide_index_any(x, i, before = 1), slide_index_lgl(x, i, any, .before = 1))
  expect_identical(slide_index_any(x, i, before = 2), slide_index_lgl(x, i, any, .before = 2))
})

test_that("integer after works", {
  x <- c(FALSE, TRUE, FALSE, FALSE)
  i <- c(1, 2, 4, 5)

  expect_identical(slide_index_any(x, i, after = 1), slide_index_lgl(x, i, any, .after = 1))
  expect_identical(slide_index_any(x, i, after = 2), slide_index_lgl(x, i, any, .after = 2))
})

test_that("negative before/after works", {
  x <- c(FALSE, TRUE, FALSE, FALSE)
  i <- c(1, 2, 4, 5)

  expect_identical(slide_index_any(x, i, before = -1, after = 2), slide_index_lgl(x, i, any, .before = -1, .after = 2))
  expect_identical(slide_index_any(x, i, before = 2, after = -1), slide_index_lgl(x, i, any, .before = 2, .after = -1))

  expect_identical(slide_index_any(x, i, before = -1, after = 2, complete = TRUE), slide_index_lgl(x, i, any, .before = -1, .after = 2, .complete = TRUE))
  expect_identical(slide_index_any(x, i, before = 2, after = -1, complete = TRUE), slide_index_lgl(x, i, any, .before = 2, .after = -1, .complete = TRUE))
})

test_that("`Inf` before/after works", {
  x <- c(FALSE, TRUE, FALSE, FALSE)
  i <- c(1, 2, 4, 5)

  expect_identical(slide_index_any(x, i, before = Inf), slide_index_lgl(x, i, any, .before = Inf))
  expect_identical(slide_index_any(x, i, after = Inf), slide_index_lgl(x, i, any, .after = Inf))
})

test_that("NA results are correct", {
  x <- c(rep(FALSE, 10), rep(NA, 10), c(FALSE, FALSE, TRUE, FALSE))
  i <- seq_along(x)

  expect_identical(
    slide_index_any(x, i, before = 3),
    slide_index_lgl(x, i, any, .before = 3)
  )
})

test_that("TRUE dominates NAs, matching any()", {
  i <- c(1, 2, 3)

  x <- c(NA, TRUE, TRUE)
  expect_identical(slide_index_any(x, i, before = 2), c(NA, TRUE, TRUE))
  expect_identical(slide_index_any(x, i, before = 2), slide_index_lgl(x, i, any, .before = 2))

  x <- c(TRUE, NA, TRUE)
  expect_identical(slide_index_any(x, i, before = 2), c(TRUE, TRUE, TRUE))
  expect_identical(slide_index_any(x, i, before = 2), slide_index_lgl(x, i, any, .before = 2))

  x <- c(TRUE, TRUE, NA)
  expect_identical(slide_index_any(x, i, before = 2), c(TRUE, TRUE, TRUE))
  expect_identical(slide_index_any(x, i, before = 2), slide_index_lgl(x, i, any, .before = 2))
})

test_that("`na_rm = TRUE` works", {
  x <- NA
  i <- 1L
  expect_identical(slide_index_any(x, i, na_rm = TRUE), FALSE)

  y <- c(TRUE, NA, FALSE, NA, TRUE)
  i <- seq_along(y)
  expect_identical(slide_index_any(y, i, na_rm = TRUE, before = 1), slide_index_lgl(y, i, any, na.rm = TRUE, .before = 1))
})

test_that("works when the window is completely OOB", {
  x <- c(TRUE, FALSE, NA)
  i <- seq_along(x)

  expect_identical(slide_index_any(x, i, before = 4, after = -4), c(FALSE, FALSE, FALSE))
  expect_identical(slide_index_any(x, i, before = 4, after = -4), slide_index_lgl(x, i, any, .before = 4, .after = -4))
})

test_that("input must be castable to logical", {
  expect_snapshot({
    (expect_error(slide_index_any(1:5, 1:5), class = "vctrs_error_cast_lossy"))
  })
})

# ------------------------------------------------------------------------------
# Misc

test_that("works with size 0 input", {
  expect_identical(slide_index_sum(integer(), integer()), double())
  expect_identical(slide_index_sum(integer(), integer(), before = 5, after = 1), double())
})

test_that("x and i must be the same size", {
  expect_snapshot({
    (expect_error(slide_index_sum(1, 1:3), class = "slider_error_index_incompatible_size"))
  })
})

test_that("names are kept (even on casting)", {
  expect_named(slide_index_sum(c(x = 1, y = 2), 1:2, before = 1), c("x", "y"))
  expect_named(slide_index_sum(c(x = 1L, y = 2L), 1:2, before = 1), c("x", "y"))
})

test_that("can cast integer and logical input", {
  expect_identical(slide_index_sum(1:5, 1:5, before = 1), slide_index_sum(1:5 + 0, 1:5, before = 1))
  expect_identical(slide_index_sum(c(TRUE, FALSE, TRUE), 1:3, before = 1), slide_index_sum(c(1, 0, 1), 1:3, before = 1))
})

test_that("types that can't be cast to numeric are not supported", {
  expect_snapshot({
    (expect_error(slide_index_sum("x", 1), class = "vctrs_error_incompatible_type"))
  })
})

test_that("arrays of dimensionality 1 are supported", {
  expect_identical(
    slide_index_sum(array(1:5), 1:5, before = 1),
    slide_index_sum(1:5, 1:5, before = 1)
  )
})

test_that("arrays of dimensionality >1 are not supported", {
  expect_snapshot({
    (expect_error(slide_index_sum(array(1:4, dim = c(2, 2)), 1:2, before = 1), class = "vctrs_error_incompatible_type"))
  })
})

test_that("works when the window is completely OOB", {
  expect_identical(
    slide_index_sum(1:3, 1:3, before = 4, after = -4),
    c(0, 0, 0)
  )
})

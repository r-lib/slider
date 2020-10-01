# ------------------------------------------------------------------------------
# slide_sum()

test_that("matches base R", {
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

test_that("matches base R", {
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

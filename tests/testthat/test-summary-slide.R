# ------------------------------------------------------------------------------
# slide_sum()

test_that("matches base R", {
  x <- rep(1/7, 10)
  expect_identical(sum(x), slide_sum2(x, Inf)[[length(x)]])
})

# ------------------------------------------------------------------------------
# slide_mean()

test_that("matches base R", {
  x <- c(1/7, 1/7, 1/3)
  expect_identical(mean(x), slide_mean2(x, Inf)[[length(x)]])
})

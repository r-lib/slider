test_that("block works as expected with year blocks", {
  i <- as.Date("2019-01-01") + c(-2:2, 31)

  expect_equal(block(i, i, period = "year"), list(i[1:2], i[3:6]))
})

test_that("block works as expected with month blocks", {
  i <- as.Date("2019-01-01") + c(-2:2, 31)

  expect_equal(block(i, i, period = "month"), list(i[1:2], i[3:5], i[6]))
})

test_that("`x` must be a vector", {
  expect_error(block(as.name("x"), new_date(0)), class = "vctrs_error_scalar_type")
})

test_that("works with empty input", {
  x <- numeric()
  i <- structure(numeric(), class = "Date")

  expect_equal(block(x, i, "year"), list())
})

test_that("`i` can not have `NA` values", {
  expect_error(block(1:2, new_date(c(0, NA_real_))), class = "slider_error_index_cannot_be_na")
})

test_that("type of `i` is validated", {
  expect_error(block(1, 1), class = "slider_error_index_incompatible_type")
})

test_that("length of `i` must be identical to `x`", {
  expect_error(block(c(1, 2), new_date(0)), class = "slider_error_index_incompatible_size")
})

test_that("`i` must be ascending", {
  expect_error(block(c(1, 2, 3), new_date(c(2, 1, 0))), class = "slider_error_index_must_be_ascending")
})

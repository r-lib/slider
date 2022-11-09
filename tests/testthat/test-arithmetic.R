test_that("`slider_plus(x, y)` uses `x + y` as a fallback", {
  x <- 1:3
  y <- 2L
  expect_identical(slider_plus(x, y), x + y)

  x <- as.Date("2019-01-01") + 0:2
  y <- 2L
  expect_identical(slider_plus(x, y), x + y)

  x <- as.POSIXct(c("2019-01-01", "2019-01-02"), tz = "UTC")
  y <- as.difftime(2L, units = "mins")
  expect_identical(slider_plus(x, y), x + y)
})

test_that("`slider_minus(x, y)` uses `x - y` as a fallback", {
  x <- 1:3
  y <- 2L
  expect_identical(slider_minus(x, y), x - y)

  x <- as.Date("2019-01-01") + 0:2
  y <- 2L
  expect_identical(slider_minus(x, y), x - y)

  x <- as.POSIXct(c("2019-01-01", "2019-01-02"), tz = "UTC")
  y <- as.difftime(2L, units = "mins")
  expect_identical(slider_minus(x, y), x - y)
})

test_that("can register package methods for <self> and <double>", {
  x <- new_slider_test_class(c(1, 2, 3))

  # Fallback on integer arithmetic
  expect_identical(slider_plus(x, 2L), new_slider_test_class(c(3, 4, 5)))
  expect_identical(slider_minus(x, 2L), new_slider_test_class(c(-1, 0, 1)))

  # Registered method for double arithmetic that multiplies `y` by `2` first
  expect_identical(slider_plus(x, 2), new_slider_test_class(c(5, 6, 7)))
  expect_identical(slider_minus(x, 2), new_slider_test_class(c(-3, -2, -1)))
})

test_that("`slide_index()` uses the methods", {
  i_base <- c(1, 2, 3, 4, 5, 6, 7, 8)
  i_custom <- new_slider_test_class(i_base)
  x <- seq_along(i_base)

  # `.before` gets multiplied by `2` before subtraction from `i_custom` in
  # `slider_minus.slider_test_class.double()`
  before <- 2
  expect_identical(
    slide_index(x, i_custom, identity, .before = before),
    slide_index(x, i_base, identity, .before = before * 2)
  )

  # `.after` gets multiplied by `2` before addition to `i_custom` in
  # `slider_plus.slider_test_class.double()`
  after <- 2
  expect_identical(
    slide_index(x, i_custom, identity, .after = after),
    slide_index(x, i_base, identity, .after = after * 2)
  )
})

test_that("can register global methods for <self> and <self>", {
  local_methods(
    slider_plus.slider_foobar.slider_foobar = function(x, y) {
      foobar((x + y) * 2L)
    },
    slider_minus.slider_foobar.slider_foobar = function(x, y) {
      foobar((x - y) * 2L)
    }
  )

  x <- foobar(1:3)
  y <- foobar(2L)

  expect_identical(slider_plus(x, y), foobar(c(6L, 8L, 10L)))
  expect_identical(slider_minus(x, y), foobar(c(-2L, 0L, 2L)))
})

test_that("can register global methods for <self> and <double>", {
  local_methods(
    slider_plus.slider_foobar.double = function(x, y) {
      foobar((x + y) * 3)
    },
    slider_minus.slider_foobar.double = function(x, y) {
      foobar((x - y) * 3)
    }
  )

  x <- foobar(1:3)
  y <- 2

  expect_identical(slider_plus(x, y), foobar(c(9, 12, 15)))
  expect_identical(slider_minus(x, y), foobar(c(-3, 0, 3)))
})

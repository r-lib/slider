test_that("basic call works", {
  expect_equal(
    slide_period(1, as.Date("2019-01-01"), "year", identity),
    list(1)
  )
})

test_that("`.x` must be a vector", {
  expect_error(slide_period(call("fn")), class = "vctrs_error_scalar_type")
})

test_that(".x must be the same size as .i", {
  expect_error(slide_period(1, new_date(1:2), "year", identity), class = "slider_error_index_incompatible_size")
})

test_that(".i must be ascending", {
  expect_error(slide_period(1:2, new_date(2:1), "year", identity), class = "slider_error_index_must_be_ascending")
})

test_that("empty input returns a list, but after the index size check", {
  expect_equal(slide_period(integer(), new_date(), "year", ~.x), list())
  expect_error(slide_period(integer(), new_date(0), "year", ~.x), class = "slider_error_index_incompatible_size")
})

test_that(".i must not contain NA values", {
  expect_error(slide_period(1:2, new_date(c(1, NA)), "year", identity), class = "slider_error_index_cannot_be_na")
  expect_error(slide_period(1:2, new_date(c(NA, 1)), "year", identity), class = "slider_error_index_cannot_be_na")
})

# ------------------------------------------------------------------------------
# .before

test_that("`.before` works", {
  expect_equal(
    slide_period(1:2, new_date(c(30, 31)), "month", identity, .before = 1),
    list(1, c(1, 2))
  )
})

test_that("`.before` skips over irregular period gaps", {
  i <- as.Date(c("2019-01-01", "2019-02-01", "2019-04-01"))

  expect_equal(
    slide_period(1:3, i, "month", identity, .before = 1),
    list(1, 1:2, 3)
  )

  expect_equal(
    slide_period(1:3, i, "month", identity, .before = 2),
    list(1, 1:2, 2:3)
  )
})

test_that("`.before` set to Inf works", {
  i <- as.Date(c("2019-01-01", "2019-02-01", "2019-04-01"))

  expect_equal(
    slide_period(1:3, i, "month", identity, .before = Inf),
    list(1, 1:2, 1:3)
  )
})

test_that("can use negative `.before`", {
  i <- as.Date(c("2019-01-01", "2019-02-01", "2019-04-01"))

  expect_equal(
    slide_period(1:3, i, "month", identity, .before = -1, .after = 1),
    list(2, integer(), integer())
  )
})

test_that("`.before` range cannot be after `.after` range", {
  i <- as.Date(c("2019-01-01", "2019-02-01", "2019-04-01"))

  expect_error(
    slide_period(1:3, i, "month", identity, .before = -1),
    "start of the range is after"
  )
})

test_that("`.before` cannot be NA", {
  expect_error(
    slide_period(1, new_date(0), "year", identity, .before = NA_integer_),
    "`.before` cannot be `NA`"
  )
})

test_that("`.before` cannot be -Inf", {
  expect_error(
    slide_period(1, new_date(0), "year", identity, .before = -Inf),
    class = "vctrs_error_cast_lossy"
  )
})

test_that(".before must be size 1", {
  expect_error(
    slide_period(1, new_date(0), "year", identity, .before = c(1L, 2L)),
    class = "vctrs_error_assert_size"
  )
})

test_that("error if .before is NULL", {
  expect_error(
    slide_period(1, new_date(0), "year", identity, .before = NULL),
    class = "vctrs_error_scalar_type"
  )
})

# ------------------------------------------------------------------------------
# .after

test_that("`.after` works", {
  expect_equal(
    slide_period(1:2, new_date(c(30, 31)), "month", identity, .after = 1),
    list(1:2, 2)
  )
})

test_that("`.after` skips over irregular period gaps", {
  i <- as.Date(c("2019-01-01", "2019-02-01", "2019-04-01"))

  expect_equal(
    slide_period(1:3, i, "month", identity, .after = 1),
    list(1:2, 2, 3)
  )

  expect_equal(
    slide_period(1:3, i, "month", identity, .after = 2),
    list(1:2, 2:3, 3)
  )
})

test_that("`.after` set to Inf works", {
  i <- as.Date(c("2019-01-01", "2019-02-01", "2019-04-01"))

  expect_equal(
    slide_period(1:3, i, "month", identity, .after = Inf),
    list(1:3, 2:3, 3)
  )
})

test_that("can use negative `.after`", {
  i <- as.Date(c("2019-01-01", "2019-02-01", "2019-04-01"))

  expect_equal(
    slide_period(1:3, i, "month", identity, .after = -1, .before = 1),
    list(integer(), 1, integer())
  )
})

test_that("`.after` range cannot be before `.before` range", {
  i <- as.Date(c("2019-01-01", "2019-02-01", "2019-04-01"))

  expect_error(
    slide_period(1:3, i, "month", identity, .after = -1),
    "start of the range is after"
  )
})

test_that("`.after` cannot be NA", {
  expect_error(
    slide_period(1, new_date(0), "year", identity, .after = NA_integer_),
    "`.after` cannot be `NA`"
  )
})

test_that("`.after` cannot be -Inf", {
  expect_error(
    slide_period(1, new_date(0), "year", identity, .after = -Inf),
    class = "vctrs_error_cast_lossy"
  )
})

test_that(".after must be size 1", {
  expect_error(
    slide_period(1, new_date(0), "year", identity, .after = c(1L, 2L)),
    class = "vctrs_error_assert_size"
  )
})

test_that("error if .after is NULL", {
  expect_error(
    slide_period(1, new_date(0), "year", identity, .after = NULL),
    class = "vctrs_error_scalar_type"
  )
})


# ------------------------------------------------------------------------------
# .complete

test_that("`.complete` works", {
  expect_equal(
    slide_period(1:2, new_date(c(30, 31)), "month", identity, .before = 1, .complete = TRUE),
    list(NULL, 1:2)
  )

  expect_equal(
    slide_period(1:2, new_date(c(30, 31)), "month", identity, .after = 1, .complete = TRUE),
    list(1:2, NULL)
  )
})

test_that(paste0(
            "proof that we need to be careful about slicing `starts` and `stops` ",
            "when `.complete = TRUE` if we are completely OOB"
          ), {

  expect_equal(
    slide_period(1:3, new_date(c(0, 2, 3)), "day", identity, .before = 4, .after = -4, .complete = TRUE),
    list(NULL, NULL, NULL)
  )
})

test_that("works when the window is between values and `.complete = TRUE`", {
  expect_equal(
    slide_period(1:3, new_date(c(0, 2, 3)), "day", identity, .before = 1, .after = -1, .complete = TRUE),
    list(NULL, integer(), 2)
  )
})

test_that("`.complete` cannot be NA", {
  expect_error(
    slide_period(1, new_date(0), "year", identity, .complete = NA),
    "`.complete` cannot be `NA`"
  )
})

test_that(".complete must be size 1", {
  expect_error(
    slide_period(1, new_date(0), "year", identity, .complete = c(TRUE, FALSE)),
    class = "vctrs_error_assert_size"
  )
})

test_that("error if .complete is NULL", {
  expect_error(
    slide_period(1, new_date(0), "year", identity, .complete = NULL),
    class = "vctrs_error_scalar_type"
  )
})

# ------------------------------------------------------------------------------
# misc

test_that("being completely OOB returns 0-slices of `x`", {
  expect_equal(
    slide_period(1:3, new_date(c(0, 2, 3)), "day", identity, .before = 4, .after = -4),
    list(integer(), integer(), integer())
  )
})

test_that("having a window completely between values returns 0-slices of `x`", {
  expect_equal(
    slide_period(1:3, new_date(c(0, 2, 3)), "day", identity, .before = 1, .after = -1),
    list(integer(), integer(), 2)
  )
})


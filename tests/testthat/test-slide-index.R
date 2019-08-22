test_that("trivial case works", {
  expect_equal(
    slide_index(1:2, 1:2, ~.x),
    list(1L, 2L)
  )
})

test_that("defaults work with `.i`", {
  i <- new_date(c(0, 1, 2, 3))
  x <- 1:4

  expect_equal(
    slide_index(x, i, identity),
    list(
      1L,
      2L,
      3L,
      4L
    )
  )
})

test_that(".x must be the same size as .i", {
  expect_error(slide_index(1, 1:2, identity), "must be the same")
})

test_that(".i must be ascending", {
  expect_error(slide_index(1:2, 2:1, identity), "`.i`ndex must be in ascending order")
})

test_that("empty input returns a list, but after the index size check", {
  expect_equal(slide_index(integer(), integer(), ~.x), list())
  expect_error(slide_index(integer(), 1, ~.x), "must be the same")
})

test_that(".i must not contain NA values", {
  expect_error(slide_index(1:2, c(1, NA), identity), "found at location[(]s[)]: 2")
  expect_error(slide_index(1:2, c(NA, 1), identity), "found at location[(]s[)]: 1")
})

# ------------------------------------------------------------------------------
# .before - integer

test_that("can use integer .before on a Date index", {
  i <- new_date(c(0, 1, 2, 3))
  x <- 1:4

  expect_equal(
    slide_index(x, i, identity, .before = 1L),
    list(
      1L,
      1:2,
      2:3,
      3:4
    )
  )
})

test_that("can use integer .before on a POSIXct index", {
  i <- new_datetime(c(0, 1, 2, 3))
  x <- 1:4

  expect_equal(
    slide_index(x, i, identity, .before = 1L),
    list(
      1L,
      1:2,
      2:3,
      3:4
    )
  )
})

test_that("using .before on an irregular date index works", {
  i <- new_date(c(0, 2, 3, 4))
  x <- 1:4

  expect_equal(
    slide_index(x, i, identity, .before = 1L),
    list(
      1L,
      2L,
      2:3,
      3:4
    )
  )

  expect_equal(
    slide_index(x, i, identity, .before = 2L),
    list(
      1L,
      1:2,
      2:3,
      2:4
    )
  )
})

test_that(".before must be size 1", {
  expect_error(
    slide_index(1, 1, identity, .before = c(1L, 2L)),
    class = "vctrs_error_assert_size"
  )
})

test_that("error if .before is NULL", {
  expect_error(
    slide_index(1, 1, identity, .before = NULL),
    class = "vctrs_error_scalar_type"
  )
})

# ------------------------------------------------------------------------------
# .before - negative

test_that("can use a negative .before with a date index", {
  i <- new_datetime(c(0, 1, 2, 3))
  x <- 1:4

  expect_equal(
    slide_index(x, i, identity, .before = -1L, .after = 1L),
    list(
      2L,
      3L,
      4L,
      NULL
    )
  )

  expect_equal(
    slide_index(x, i, identity, .before = -1L, .after = 2L),
    list(
      2:3,
      3:4,
      4L,
      NULL
    )
  )
})

test_that("can use a negative .before with an irregular date index", {
  i <- new_datetime(c(0, 1, 1, 3))
  x <- 1:4

  expect_equal(
    slide_index(x, i, identity, .before = -1L, .after = 2L),
    list(
      2:3,
      4L,
      4L,
      NULL
    )
  )
})

test_that("can select no elements when using a negative .before", {
  i <- new_datetime(c(0, 1, 1, 3))
  x <- 1:4

  expect_equal(
    slide_index(x, i, identity, .before = -1L, .after = 1L),
    list(
      2:3,
      integer(),
      integer(),
      NULL
    )
  )
})

test_that("negative .before errors if its absolute value is past .after", {
  i <- new_date(c(0, 1, 2, 3))
  x <- i

  expect_error(
    slide_index(x, i, identity, .before = -1, .after = 0),
    "the start of the range is after the end of the range at location[(]s[)]: 1, 2, 3, 4"
  )
})

# ------------------------------------------------------------------------------
# .before - numeric

test_that("ranges use an inclusive .before bound", {
  i <- c(1000, 3000, 5000.000001)
  x <- seq_along(i)

  # Should include 1000 in the second slot of the output
  expect_equal(
    slide_index(x, i, identity, .before = 2000),
    list(
      1L,
      1:2,
      3
    )
  )
})

test_that("can define ranges based on an irregular numeric index", {
  i <- c(1000, 2000, 4000, 8000, 10000, 11000)
  x <- seq_along(i)

  expect_equal(
    slide_index(x, i, identity, .before = 2000),
    list(
      1L,
      1:2,
      2:3,
      4L,
      4:5,
      5:6
    )
  )
})

# ------------------------------------------------------------------------------
# .before - lubridate - Durations

test_that("can use hour Durations with Dates", {
  i <- new_date(c(0, 1, 2, 3))
  x <- seq_along(i)

  expect_equal(
    slide_index(x, i, identity, .before = lubridate::dhours(1)),
    list(
      1L,
      2L,
      3L,
      4L
    )
  )

  expect_equal(
    slide_index(x, i, identity, .before = lubridate::dhours(24)),
    slide_index(x, i, identity, .before = 1L)
  )
})

test_that("can use day Durations with Dates", {
  i <- new_date(c(0, 1, 2, 3))
  x <- seq_along(i)

  expect_equal(
    slide_index(x, i, identity, .before = lubridate::ddays(1)),
    slide_index(x, i, identity, .before = 1L)
  )

  expect_equal(
    slide_index(x, i, identity, .before = lubridate::ddays(2)),
    slide_index(x, i, identity, .before = 2L)
  )
})

test_that("can use week Durations with Dates", {
  i <- new_date(c(0, 6, 7, 8))
  x <- seq_along(i)
  before <- lubridate::dweeks(1)

  # Current element of i, + everything since i - dweeks(1), inclusive!
  expect_equal(
    slide_index(x, i, identity, .before = before),
    list(
      1L,
      1:2,
      1:3,
      2:4
    )
  )
})

test_that("can use year Durations with Dates", {
  i <- new_date(c(0, 365, 365 * 2))
  x <- seq_along(i)
  before <- lubridate::dyears(1)

  expect_equal(
    slide_index(x, i, identity, .before = before),
    list(
      1L,
      1:2,
      2:3
    )
  )
})

test_that("can use negative Durations with Dates", {
  i <- new_date(c(0, 1, 2, 3))
  x <- seq_along(i)

  expect_equal(
    slide_index(x, i, identity, .before = -lubridate::ddays(1), .after = lubridate::ddays(1)),
    list(
      2L,
      3L,
      4L,
      NULL
    )
  )

  expect_equal(
    slide_index(x, i, identity, .before = -lubridate::ddays(1), .after = lubridate::ddays(2)),
    list(
      2:3,
      3:4,
      4L,
      NULL
    )
  )
})

test_that("errors if negative .before Duration is further than .after", {
  i <- new_date(c(0, 1, 2, 3))
  x <- seq_along(i)

  expect_error(
    slide_index(x, i, identity, .before = -lubridate::ddays(1), .after = 0),
    "the start of the range is after the end of the range at location[(]s[)]: 1, 2, 3, 4"
  )
})

test_that("can use millisecond Durations with POSIXct", {
  i <- new_datetime(c(0, 0.001, 0.002, 0.003))
  x <- seq_along(i)

  expect_equal(
    slide_index(x, i, identity, .before = lubridate::dmilliseconds(1)),
    list(
      1L,
      1:2,
      2:3,
      3:4
    )
  )
})

test_that("can use second Durations with POSIXct", {
  i <- new_datetime(c(0, 1, 2, 3))
  x <- seq_along(i)

  expect_equal(
    slide_index(x, i, identity, .before = lubridate::dseconds(1)),
    list(
      1L,
      1:2,
      2:3,
      3:4
    )
  )
})

test_that("can use day Durations with POSIXct", {
  i <- lubridate::as_datetime(new_date(c(0, 1, 2, 3)))
  x <- seq_along(i)

  expect_equal(
    slide_index(x, i, identity, .before = lubridate::ddays(1)),
    list(
      1L,
      1:2,
      2:3,
      3:4
    )
  )
})

# ------------------------------------------------------------------------------
# .before - lubridate - Periods

test_that("can use hour Periods with Dates", {
  i <- new_date(c(0, 1, 2, 3))
  x <- seq_along(i)

  expect_equal(
    slide_index(x, i, identity, .before = lubridate::hours(1)),
    list(
      1L,
      2L,
      3L,
      4L
    )
  )

  expect_equal(
    slide_index(x, i, identity, .before = lubridate::hours(24)),
    slide_index(x, i, identity, .before = 1L)
  )
})

test_that("can use day Periods with Dates", {
  i <- new_date(c(0, 1, 2, 3))
  x <- seq_along(i)

  expect_equal(
    slide_index(x, i, identity, .before = lubridate::days(1)),
    slide_index(x, i, identity, .before = 1L)
  )

  expect_equal(
    slide_index(x, i, identity, .before = lubridate::days(2)),
    slide_index(x, i, identity, .before = 2L)
  )
})

test_that("can use week Periods with Dates", {
  i <- new_date(c(0, 6, 7, 8))
  x <- seq_along(i)
  before <- lubridate::weeks(1)

  # Current element of i, + everything since i - weeks(1), inclusive!
  # So it includes the data point at 1 week prior, and today
  expect_equal(
    slide_index(x, i, identity, .before = before),
    list(
      1L,
      1:2,
      1:3,
      2:4
    )
  )

  # If you want to avoid that 1 week prior data point, bump it back
  # to 1 week - 1 second
  before <- lubridate::weeks(1) - lubridate::seconds(1)

  expect_equal(
    slide_index(x, i, identity, .before = before),
    list(
      1L,
      1:2,
      2:3,
      2:4
    )
  )
})

test_that("can generally use (tricky!) month Periods with Dates", {
  requireNamespace("lubridate", quietly = TRUE)

  i <- new_date(0) + months(0:3)
  x <- seq_along(i)

  expect_equal(
    slide_index(x, i, identity, .before = months(1)),
    list(
      1L,
      1:2,
      2:3,
      3:4
    )
  )

  # Problematic when dates don't exist! Nothing we can do to help!
  # General solution is to use ~ .x %m-% months(1) instead
  i <- as.Date("2019-03-30") + months(0:3)

  expect_error(
    slide_index(x, i, identity, .before = months(1)),
    "`.before` cannot have `NA` values"
  )
})

# ------------------------------------------------------------------------------
# .before - lubridate - Leap Years / DST

test_that("can use year Durations/Periods with Dates and leap years", {
  # 2008 = leap year
  i <- as.Date(c("2008-01-01", "2009-01-01", "2010-01-01"))
  x <- seq_along(i)
  before <- lubridate::dyears(1)

  # There are MORE than ~52.14 weeks in a leap year
  # So subtracting 1 dyears() from 2009-01-01 should NOT include 2008-01-01
  expect_equal(
    slide_index(x, i, identity, .before = before),
    list(
      1L,
      2L,
      2:3
    )
  )

  # Using Period objects results in "expected" behavior as it is calendar
  # time and not clock time
  before <- lubridate::years(1)

  expect_equal(
    slide_index(x, i, identity, .before = before),
    list(
      1L,
      1:2,
      2:3
    )
  )
})

# ------------------------------------------------------------------------------
# .before - nanotime

# TODO - vec_compare() can't handle nanotime objects
# could provide a vec_proxy_compare
# vctrs:::vec_proxy_compare.integer64(nanotime::nanotime(c(1, 2, 3, 6, 4)))

# test_that("can use nanotime resolution", {
#   i <- nanotime::nanotime(1:5)
#   x <- seq_along(i)
#
#   expect_equal(
#     slide_index(x, i, identity, .before = 1L),
#     list(
#       1L,
#       1:2,
#       2:3,
#       3:4,
#       4:5
#     )
#   )
#
#   expect_equal(
#     slide_index(x, i, identity, .before = 2L),
#     list(
#       1L,
#       1:2,
#       1:3,
#       2:4,
#       3:5
#     )
#   )
# })

# ------------------------------------------------------------------------------
# .before - function

test_that("can use a lambda function in .before", {
  i <- 1:5
  x <- i

  expect_equal(
    slide_index(x, i, identity, .before = ~.x),
    slide_index(x, i, identity, .before = 0L)
  )

  expect_equal(
    slide_index(x, i, identity, .before = ~.x - 2),
    slide_index(x, i, identity, .before = 2L)
  )

  expect_equal(
    slide_index(x, i, identity, .before = ~ifelse(.x == 4L, .x - 1L, .x - 2L)),
    list(
      1L,
      1:2,
      1:3,
      3:4,
      3:5
    )
  )
})

test_that("can use a function in .before", {
  i <- 1:5
  x <- i

  fn <- function(x) x - 2L

  expect_equal(
    slide_index(x, i, identity, .before = fn),
    slide_index(x, i, identity, .before = 2L)
  )
})

test_that("can use a function in .before that looks forward (like negative .before)", {
  i <- 1:5
  x <- i

  fn <- function(x) x + 1L

  expect_equal(
    slide_index(x, i, identity, .before = fn, .after = 2L),
    slide_index(x, i, identity, .before = -1L, .after = 2L)
  )
})

test_that("errors if look forward function looks past .after value", {
  i <- 1:5
  x <- i

  fn <- function(x) x + 2L

  expect_error(
    slide_index(x, i, identity, .before = fn, .after = 1L),
    "the start of the range is after the end of the range at location[(]s[)]: 1, 2, 3, 4, 5"
  )
})

test_that("range generated by .before function should be in ascending order", {
  i <- 1:5
  x <- i

  fn <- function(x) {
    ifelse(x == 2L, x - 3L, x - 1L)
  }

  expect_error(
    slide_index(x, i, identity, .before = fn, .after = 1L),
    "`.before` must be in ascending order"
  )
})

test_that("range generated by .before function should have same size as unique .i", {
  i <- c(1, 1, 2, 2, 3)
  x <- i

  expect_error(
    slide_index(x, i, ~.x, .before = ~1),
    "`.before` has size 1"
  )

  expect_error(
    slide_index(x, i, ~.x, .before = ~1),
    "unique values of `.i`, 3"
  )
})

# ------------------------------------------------------------------------------
# .before - function - lubridate

test_that("can use `%m-%` and `add_with_rollback()` to solve month rollback issues", {
  requireNamespace("lubridate", quietly = TRUE)
  `%m-%` <- lubridate::`%m-%`

  i <- vec_c(as.Date("2019-02-27") + 0:3, as.Date("2019-03-27") + 0:5)
  x <- seq_along(i)

  # 3/27 rollback to 2/27
  # 3/28 rollback to 2/28
  # 3/29 rollback to 2/28
  # 3/30 rollback to 2/28
  # 3/31 rollback to 2/28
  # 4/01 rollback to 3/01
  expect_equal(
    slide_index(x, i, identity, .before = ~.x %m-% months(1)),
    list(
      1L,
      1:2,
      1:3,
      1:4,
      1:5,
      2:6,
      2:7,
      2:8,
      2:9,
      3:10
    )
  )

  # 3/27 rollback to 2/27
  # 3/28 rollback to 2/28
  # 3/29 rollback to 2/28 then forward to 3/01
  # 3/30 rollback to 2/28 then forward to 3/01
  # 3/31 rollback to 2/28 then forward to 3/01
  # 4/01 rollback to 3/01
  expect_equal(
    slide_index(
      x, i, identity,
      .before = ~lubridate::add_with_rollback(.x, -months(1), roll_to_first = TRUE)
    ),
    list(
      1L,
      1:2,
      1:3,
      1:4,
      1:5,
      2:6,
      3:7,
      3:8,
      3:9,
      3:10
    )
  )
})

# ------------------------------------------------------------------------------
# .after - integer

test_that("can use integer .after on a Date index", {
  i <- new_date(c(0, 1, 2, 3))
  x <- 1:4

  expect_equal(
    slide_index(x, i, identity, .after = 1L),
    list(
      1:2,
      2:3,
      3:4,
      4L
    )
  )
})

test_that("can use integer .after on a POSIXct index", {
  i <- new_datetime(c(0, 1, 2, 3))
  x <- 1:4

  expect_equal(
    slide_index(x, i, identity, .after = 1L),
    list(
      1:2,
      2:3,
      3:4,
      4L
    )
  )
})

test_that("using .after on an irregular date index works", {
  i <- new_date(c(0, 2, 3, 4))
  x <- 1:4

  expect_equal(
    slide_index(x, i, identity, .after = 1L),
    list(
      1L,
      2:3,
      3:4,
      4L
    )
  )

  expect_equal(
    slide_index(x, i, identity, .after = 2L),
    list(
      1:2,
      2:4,
      3:4,
      4L
    )
  )
})

test_that(".after must be size 1", {
  expect_error(
    slide_index(1, 1, identity, .after = c(1L, 2L)),
    class = "vctrs_error_assert_size"
  )
})

test_that("error if .after is NULL", {
  expect_error(
    slide_index(1, 1, identity, .after = NULL),
    class = "vctrs_error_scalar_type"
  )
})

# ------------------------------------------------------------------------------
# .after - negative

test_that("can use a negative .after with integer index", {
  i <- 1:5
  x <- i

  expect_equal(
    slide_index(x, i, identity, .before = 2L, .after = -1L),
    list(
      NULL,
      1L,
      1:2,
      2:3,
      3:4
    )
  )
})

test_that("can use a negative .after with a date index", {
  i <- new_datetime(c(0, 1, 2, 3))
  x <- 1:4

  expect_equal(
    slide_index(x, i, identity, .after = -1L, .before = 1L),
    list(
      NULL,
      1L,
      2L,
      3L
    )
  )

  expect_equal(
    slide_index(x, i, identity, .after = -1L, .before = 2L),
    list(
      NULL,
      1L,
      1:2,
      2:3
    )
  )
})

test_that("can use a negative .after with an irregular date index", {
  i <- new_datetime(c(0, 1, 1, 3))
  x <- 1:4

  expect_equal(
    slide_index(x, i, identity, .after = -1L, .before = 2L),
    list(
      NULL,
      1L,
      1L,
      2:3
    )
  )
})

test_that("can select no elements when using a negative .after", {
  i <- new_datetime(c(0, 1, 1, 3))
  x <- 1:4

  expect_equal(
    slide_index(x, i, identity, .after = -1L, .before = 1L),
    list(
      NULL,
      1,
      1,
      integer()
    )
  )
})

test_that("negative .after errors if its absolute value is past .before", {
  i <- new_date(c(0, 1, 2, 3))
  x <- i

  expect_error(
    slide_index(x, i, identity, .after = -1, .before = 0),
    "the start of the range is after the end of the range at location[(]s[)]: 1, 2, 3, 4"
  )
})

# ------------------------------------------------------------------------------
# .after - numeric

test_that("ranges use an inclusive .after bound", {
  i <- c(1000, 3000, 5000.000001)
  x <- seq_along(i)

  # Should include 1000 in the second slot of the output
  expect_equal(
    slide_index(x, i, identity, .after = 2000),
    list(
      1:2,
      2L,
      3L
    )
  )
})

test_that("can define ranges based on an irregular numeric index", {
  i <- c(1000, 2000, 4000, 8000, 10000, 11000)
  x <- seq_along(i)

  expect_equal(
    slide_index(x, i, identity, .after = 2000),
    list(
      1:2,
      2:3,
      3L,
      4:5,
      5:6,
      6L
    )
  )
})

# ------------------------------------------------------------------------------
# .after - lubridate - Periods

test_that("can use hour Periods with Dates", {
  i <- new_date(c(0, 1, 2, 3))
  x <- seq_along(i)

  expect_equal(
    slide_index(x, i, identity, .after = lubridate::hours(1)),
    list(
      1L,
      2L,
      3L,
      4L
    )
  )

  expect_equal(
    slide_index(x, i, identity, .after = lubridate::hours(24)),
    slide_index(x, i, identity, .after = 1L)
  )
})

test_that("can use day Periods with Dates", {
  i <- new_date(c(0, 1, 2, 3))
  x <- seq_along(i)

  expect_equal(
    slide_index(x, i, identity, .after = lubridate::days(1)),
    slide_index(x, i, identity, .after = 1L)
  )

  expect_equal(
    slide_index(x, i, identity, .after = lubridate::days(2)),
    slide_index(x, i, identity, .after = 2L)
  )
})

test_that("can use week Periods with Dates", {
  i <- new_date(c(0, 6, 7, 8))
  x <- seq_along(i)
  after <- lubridate::weeks(1)

  # Current element of i, + everything since i + weeks(1), inclusive!
  # So it includes the data point at 1 week prior, and today
  expect_equal(
    slide_index(x, i, identity, .after = after),
    list(
      1:3,
      2:4,
      3:4,
      4L
    )
  )

  # If you want to avoid that 1 week prior data point, bump it back
  # to 1 week - 1 second
  after <- lubridate::weeks(1) - lubridate::seconds(1)

  expect_equal(
    slide_index(x, i, identity, .after = after),
    list(
      1:2,
      2:4,
      3:4,
      4L
    )
  )
})

test_that("can generally use (tricky!) month Periods with Dates", {
  requireNamespace("lubridate", quietly = TRUE)

  i <- new_date(0) + months(0:3)
  x <- seq_along(i)

  expect_equal(
    slide_index(x, i, identity, .after = months(1)),
    list(
      1:2,
      2:3,
      3:4,
      4L
    )
  )

  # Problematic when dates don't exist! Nothing we can do to help!
  # General solution is to use ~ .x %m-% months(1) instead
  i <- as.Date("2019-01-30") + months(-3:0)

  expect_error(
    slide_index(x, i, identity, .after = months(1)),
    "`.after` cannot have `NA` values"
  )
})


# ------------------------------------------------------------------------------
# .after - lubridate - Leap Years / DST

test_that("can use Durations/Periods to handle daylight savings differently", {
  i <- lubridate::ymd_hms("2009-03-08 01:59:59", tz = "America/Chicago")
  i <- i + lubridate::days(0:1)
  i <- vec_c(i, i[2] + lubridate::hours(1))
  x <- seq_along(i)

  # When 1 days() is added to the boundary, it keeps the same hour value
  # because the calendar time should be equal everywhere except in the day slot
  # This means the 3rd data point is not included.
  expect_equal(
    slide_index(x, i, identity, .after = lubridate::days(1)),
    list(
      1:2,
      2:3,
      3L
    )
  )

  # When 1 ddays() is added to the boundary, it adds a fixed number of seconds
  # to the boundary, which, after adjusting the time zone, ends up 1 day +
  # 1 hour forward past the boundary.
  # This means the 3rd data point gets included.
  expect_equal(
    slide_index(x, i, identity, .after = lubridate::ddays(1)),
    list(
      1:3,
      2:3,
      3L
    )
  )
})

# ------------------------------------------------------------------------------
# .after - function

test_that("can use a lambda function in .after", {
  i <- 1:5
  x <- i

  expect_equal(
    slide_index(x, i, identity, .after = ~.x),
    slide_index(x, i, identity, .after = 0L)
  )

  expect_equal(
    slide_index(x, i, identity, .after = ~.x + 2),
    slide_index(x, i, identity, .after = 2L)
  )

  expect_equal(
    slide_index(x, i, identity, .after = ~ifelse(.x == 4L, .x + 1L, .x + 2L)),
    list(
      1:3,
      2:4,
      3:5,
      4:5,
      5L
    )
  )
})

test_that("can use a function in .after", {
  i <- 1:5
  x <- i

  fn <- function(x) x + 2L

  expect_equal(
    slide_index(x, i, identity, .after = fn),
    slide_index(x, i, identity, .after = 2L)
  )
})

test_that("can use a function in .after that looks backwards (like negative .after)", {
  i <- 1:5
  x <- i

  fn <- function(x) x - 1L

  expect_equal(
    slide_index(x, i, identity, .after = fn, .before = 2L),
    slide_index(x, i, identity, .after = -1L, .before = 2L)
  )
})

test_that("errors if look forward function looks past .after value", {
  i <- 1:5
  x <- i

  fn <- function(x) x - 2L

  expect_error(
    slide_index(x, i, identity, .after = fn, .before = 1L),
    "the start of the range is after the end of the range at location[(]s[)]: 1, 2, 3, 4, 5"
  )
})

test_that("range generated by .after function should be in ascending order", {
  i <- 1:5
  x <- i

  fn <- function(x) {
    ifelse(x == 2L, x + 3L, x + 1L)
  }

  expect_error(
    slide_index(x, i, identity, .after = fn, .before = 1L),
    "`.after` must be in ascending order"
  )
})

test_that("range generated by .after function should have same size as unique .i", {
  i <- c(1, 1, 2, 2, 3)
  x <- i

  expect_error(
    slide_index(x, i, ~.x, .after = ~1),
    "`.after` has size 1"
  )

  expect_error(
    slide_index(x, i, ~.x, .after = ~1),
    "unique values of `.i`, 3"
  )
})

# ------------------------------------------------------------------------------
# .complete

test_that("can match slide() usage of .complete", {
  x <- 1:5

  expect_equal(
    slide_index(x, x, identity, .before = 1, .complete = TRUE),
    slide(x, identity, .before = 1, .complete = TRUE)
  )

  expect_equal(
    slide_index(x, x, identity, .before = 1, .after = 2L, .complete = TRUE),
    slide(x, identity, .before = 1, .after = 2L, .complete = TRUE)
  )
})

test_that("can filter for .complete date ranges", {
  i <- new_date(c(0, 1, 2, 2, 3))
  x <- seq_along(i)

  expect_equal(
    slide_index(x, i, identity, .before = 1, .complete = TRUE),
    list(
      NULL,
      1:2,
      2:4,
      2:4,
      3:5
    )
  )
})

test_that(".complete only ensures that there is at least 1 value before that could have been used", {
  i <- new_date(c(0, 2, 3, 4, 5))
  x <- seq_along(i)

  # i.e., even though element 2 doesn't have a "complete" window of size 2,
  # it theoretically _could_ have because 1970-01-03 - 1 day = 1970-01-02
  # which is above the boundary value of 1970-01-01, and that is what matters
  # when computing completeness
  expect_equal(
    slide_index(x, i, identity, .before = 1, .complete = TRUE),
    list(
      NULL,
      2L,
      2:3,
      3:4,
      4:5
    )
  )

  # same idea here:
  # even though element 1 doesn't have a "complete" window of size 2,
  # it theoretically _could_ have because 1970-01-01 + 1 day = 1970-01-02
  # which is below the boundary value of 1970-01-06, and that is what matters
  # when computing completeness
  expect_equal(
    slide_index(x, i, identity, .after = 1, .complete = TRUE),
    list(
      1L,
      2:3,
      3:4,
      4:5,
      NULL
    )
  )
})

test_that("cannot use an invalid .complete value", {
  expect_error(
    slide_index(1, 1, identity, .complete = "hi"),
    class = "vctrs_error_cast_lossy"
  )
})

# ------------------------------------------------------------------------------
# unbounded()

test_that("can use unbounded .before", {
  i <- new_date(c(0, 1, 2, 3, 4))
  x <- seq_along(i)

  expect_equal(
    slide_index(x, i, identity, .before = unbounded()),
    list(
      1L,
      1:2,
      1:3,
      1:4,
      1:5
    )
  )
})

test_that("can use unbounded .before with positive .after", {
  i <- new_date(c(0, 1, 2, 3, 4))
  x <- seq_along(i)

  expect_equal(
    slide_index(x, i, identity, .before = unbounded(), .after = 1),
    list(
      1:2,
      1:3,
      1:4,
      1:5,
      1:5
    )
  )
})

test_that("can use unbounded .before with negative .after", {
  i <- new_date(c(0, 1, 2, 3, 4))
  x <- seq_along(i)

  expect_equal(
    slide_index(x, i, identity, .before = unbounded(), .after = -1),
    list(
      NULL,
      1L,
      1:2,
      1:3,
      1:4
    )
  )
})

test_that("can use unbounded .before with lubridate .after", {
  i <- new_date(c(0, 1, 2, 3, 4))
  x <- seq_along(i)

  expect_equal(
    slide_index(x, i, identity, .before = unbounded(), .after = lubridate::days(2)),
    list(
      1:3,
      1:4,
      1:5,
      1:5,
      1:5
    )
  )
})

# ------------------------------------------------------------------------------
# type / size relaxed-ness

test_that("slide_index() doesn't require `size = 1`", {
  expect_equal(
    slide_index(1:2, 1:2, ~c(.x, 1)),
    list(
      c(1L, 1L),
      c(2L, 1L)
    )
  )
})

test_that("`slide_index()` doesn't require a common inner type", {
  expect_equal(
    slide_index(1:2, 1:2, ~if (.x == 1L) {1} else {"hi"}),
    list(1, "hi")
  )
})

# ------------------------------------------------------------------------------
# input names

test_that("input names are retained with atomics", {
  names <- letters[1:5]
  x <- set_names(1:5, names)
  i <- vec_seq_along(x)
  expect_equal(names(slide_index(x, i, ~.x)), names)
})

test_that("input names are retained from proxied objects", {
  names <- letters[1:5]
  x <- as.POSIXlt(new_datetime(0:4 + 0))
  x <- set_names(x, names)
  i <- vec_seq_along(x)
  expect_equal(names(slide_index(x, i, ~.x)), names)
})

test_that("row names are not extracted from data frames", {
  x <- data.frame(x = 1:5, row.names = letters[1:5])
  i <- vec_seq_along(x)
  expect_equal(names(slide_index(x, i, ~.x)), NULL)
})

test_that("row names are extracted from arrays", {
  x <- array(1:4, c(2, 2), dimnames = list(c("r1", "r2"), c("c1", "c2")))
  i <- vec_seq_along(x)
  expect_equal(names(slide_index(x, i, ~.x)), c("r1", "r2"))
})

test_that("names are retained on inner sliced object", {
  names <- letters[1:5]
  x <- set_names(1:5, names)
  i <- vec_seq_along(x)
  exp <- set_names(as.list(names), names)
  expect_equal(slide_index(x, i, ~names(.x)), exp)

  x <- data.frame(x = 1:5, row.names = letters[1:5])
  i <- vec_seq_along(x)
  expect_equal(slide_index(x, i, ~rownames(.x)), as.list(rownames(x)))

  names <- c("r1", "r2")
  x <- array(1:4, c(2, 2), dimnames = list(names, c("c1", "c2")))
  i <- vec_seq_along(x)
  exp <- set_names(as.list(names), names)
  expect_equal(slide_index(x, i, ~rownames(.x)), exp)
})

# ------------------------------------------------------------------------------
# .i types

test_that("can technically use a logical index", {
  expect_equal(
    slide_index(1:3, c(FALSE, FALSE, TRUE), ~.x, .before = ~.x, .after = ~.x),
    list(
      1:2,
      1:2,
      3L
    )
  )

  expect_equal(
    slide_index(1:3, c(FALSE, FALSE, TRUE), ~.x, .before = 1L),
    list(
      1:2,
      1:2,
      1:3
    )
  )
})

test_that("can technically use a character index", {
  expect_equal(
    slide_index(1:3, c("a", "b", "b"), ~.x, .before = ~.x, .after = ~.x),
    list(
      1L,
      2:3,
      2:3
    )
  )
})

test_that("can use a data frame index", {
  expect_equal(
    slide_index(1:5, data.frame(x = c(1, 1, 2, 3, 4)), ~.x),
    list(
      1:2,
      1:2,
      3L,
      4L,
      5L
    )
  )
})

test_that("can order by two vectors using a data frame and lambda", {
  i <- data.frame(
    date1 = new_date(c(0, 3, 4, 5)),
    date2 = new_date(c(0, 1, 2, 4))
  )

  before <- data.frame(date1 = 2, date2 = 1)

  # NOTE - This is a bit tricky. It always tries to determine the comparison
  # order using the first column that it comes across. If the values are equal,
  # only then will it look to the second column

  expect_equal(
    slide_index(i, i, ~.x, .before = ~.x - vec_recycle(before, vec_size(.x)), .after = ~.x),
    list(
      # At row 1, subtracting makes no difference
      # Return row 1
      vec_slice(i, 1L),

      # "1970-01-04" - 2 days = "1970-01-02"
      # "1970-01-02" > "1970-01-01". Done.
      # Return row 2
      vec_slice(i, 2L),

      # "1970-01-05" - 2 days = "1970-01-03"
      # "1970-01-03" < "1970-01-04" so use row 2
      # "1970-01-03" > "1970-01-01" so don't use row 1
      # Return row 2 and 3
      vec_slice(i, 2:3),

      # "1970-01-06" - 2 days = "1970-01-04"
      # "1970-01-04" < "1970-01-05" so use row 3
      # "1970-01-04" = "1970-01-04" so look to column 2
      # "1970-01-05" - 1 day = "1970-01-04" (col 2)
      # "1970-01-04" > "1970-01-02" so don't use row 2
      # Return row 3 and 4
      vec_slice(i, 3:4)
    )
  )
})

# ------------------------------------------------------------------------------

test_that("repeated index values are grouped with the same values", {
  i <- c(1, 1, 1, 2, 2, 3, 4, 4, 5)
  x <- seq_along(i)

  expect_equal(
    slide_index(x, i, identity),
    as.list(vec_slice(vec_split(x, i)$val, i))
  )
})

test_that("repeated date index values are grouped with the same values", {
  i <- new_date(c(0, 0, 1))
  x <- seq_along(i)

  expect_equal(
    slide_index(x, i, identity),
    list(
      1:2,
      1:2,
      3L
    )
  )

  expect_equal(
    slide_index(x, i, identity, .before = 1L),
    list(
      1:2,
      1:2,
      1:3
    )
  )
})

test_that("can have an irregular index where the window is completely within two index values", {
  expect_equal(
    slide_index(1:7, c(10, 11, 13, 17, 18, 19, 20), ~.x, .before = 3, .after = -2),
    list(
      NULL,
      NULL,
      1:2,
      integer(),
      integer(),
      4L,
      4:5
    )
  )
})

test_that("can select 0 values if before/after are completely out of range", {
  expect_equal(
    slide_index(1:5, 1:5, identity, .before = 10, .after = -10),
    rep(list(NULL), 5)
  )
})

test_that("indexing by vec_seq_along(.x) is the same as slide()", {
  expect_equal(
    slide(1:5, ~.x),
    slide_index(1:5, 1:5, ~.x)
  )

  expect_equal(
    slide(1:5, ~.x, .before = 1),
    slide_index(1:5, 1:5, ~.x, .before = 1)
  )
})

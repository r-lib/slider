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

test_that(".before must be size 1 if .after is not NULL", {
  expect_error(
    slide_index(1, 1, identity, .before = c(1L, 2L)),
    class = "vctrs_error_assert_size"
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

# TODO - What is the correct behavior with `-` changes the type?
# Here it changes from Date -> POSIXct. This still works because
# you can compare Date and POSIXct with > or < at the R level,
# but they will need to be the same type at the C level. Do we
# just convert to the new common type? We can't convert to the
# type of `.i` because the POSIXct promoted type is the "correct"
# new type
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

  # Current element of i, + everything since i - weeks(1), inclusive!
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
    "`NA` value detected in the index range generated by `.before`."
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

test_that("can use nanotime resolution", {
  i <- nanotime::nanotime(1:5)
  x <- seq_along(i)

  expect_equal(
    slide_index(x, i, identity, .before = 1L),
    list(
      1L,
      1:2,
      2:3,
      3:4,
      4:5
    )
  )

  expect_equal(
    slide_index(x, i, identity, .before = 2L),
    list(
      1L,
      1:2,
      1:3,
      2:4,
      3:5
    )
  )
})

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
# .before - list (.after = NULL)

test_that("can use a negative .after by providing 2 .before values", {
  i <- 1:5
  x <- i

  expect_equal(
    slide_index(x, i, identity, .before = c(0, 0), .after = NULL),
    slide_index(x, i, identity, .before = 0, .after = 0)
  )

  expect_equal(
    slide_index(x, i, identity, .before = c(2, 1), .after = NULL),
    list(
      NULL,
      1L,
      1:2,
      2:3,
      3:4
    )
  )
})

test_that("can provide a function in the list for .before", {
  i <- 1:5
  x <- i

  expect_equal(
    slide_index(x, i, identity, .before = c(2, 1), .after = NULL),
    slide_index(x, i, identity, .before = c(~.x - 2, 1), .after = NULL)
  )

  fn <- function(x) x - 2L

  expect_equal(
    slide_index(x, i, identity, .before = c(2, 1), .after = NULL),
    slide_index(x, i, identity, .before = c(fn, 1), .after = NULL)
  )
})

test_that("error if 2 .before values are provided and .after is not NULL", {
  expect_error(
    slide_index(1, 1, identity, .before = list(1, 2), .after = 0),
    class = "vctrs_error_assert_size"
  )
})

test_that("error if .after is NULL and .before hasn't provided 2 values", {
  expect_error(
    slide_index(1, 1, identity, .before = 1, .after = NULL),
    "`.after` is `NULL`, `.before`"
  )
})

test_that("error if .after is NULL and .before is a single formula/function", {
  expect_error(
    slide_index(1, 1, identity, .before = ~.x, .after = NULL),
    "`.after` is `NULL`, `.before` must have type list"
  )

  expect_error(
    slide_index(1, 1, identity, .before = function(x) x, .after = NULL),
    "`.after` is `NULL`, `.before` must have type list"
  )
})

test_that("error if .after is NULL and the first .before value is < the second", {
  expect_error(
    slide_index(1, 1, identity, .before = c(1, 2), .after = NULL)
  )
})

# ------------------------------------------------------------------------------
# .after - lubridate - Leap Years / DST

test_that("can use Durations/Periods to handle daylight savings differently", {
  i <- lubridate::ymd_hms("2009-03-08 01:59:59", tz = "America/Chicago")
  i <- i + lubridate::days(0:1)
  i <- c(i, i[2] + lubridate::hours(1))
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
# .after - list (.after = NULL)

test_that("can use a negative .before by providing 2 .after values", {
  i <- 1:5
  x <- i

  expect_equal(
    slide_index(x, i, identity, .after = c(0, 0), .before = NULL),
    slide_index(x, i, identity, .after = 0, .before = 0)
  )

  expect_equal(
    slide_index(x, i, identity, .after = c(-1, 0), .before = NULL),
    slide_index(x, i, identity, .after = 0, .before = 1)
  )

  expect_equal(
    slide_index(x, i, identity, .after = c(1, 2), .before = NULL),
    list(
      2:3,
      3:4,
      4:5,
      5L,
      NULL
    )
  )
})

test_that("can provide a function in the list for .after", {
  i <- 1:5
  x <- i

  expect_equal(
    slide_index(x, i, identity, .after = c(1, 2), .before = NULL),
    slide_index(x, i, identity, .after = c(~.x + 1, 2), .before = NULL)
  )

  fn <- function(x) x + 2L

  expect_equal(
    slide_index(x, i, identity, .after = c(1, 2), .before = NULL),
    slide_index(x, i, identity, .after = c(1, fn), .before = NULL)
  )
})

test_that("error if 2 .before values are provided and .after is not NULL", {
  expect_error(
    slide_index(1, 1, identity, .before = list(1, 2), .after = 0),
    class = "vctrs_error_assert_size"
  )
})

test_that("error if .after is NULL and .before hasn't provided 2 values", {
  expect_error(
    slide_index(1, 1, identity, .before = 1, .after = NULL),
    "`.after` is `NULL`, `.before`"
  )
})

test_that("error if .after is NULL and .before is a single formula/function", {
  expect_error(
    slide_index(1, 1, identity, .before = ~.x, .after = NULL),
    "`.after` is `NULL`, `.before` must have type list"
  )

  expect_error(
    slide_index(1, 1, identity, .before = function(x) x, .after = NULL),
    "`.after` is `NULL`, `.before` must have type list"
  )
})

test_that("error if .after is NULL and the first .before value is < the second", {
  expect_error(
    slide_index(1, 1, identity, .before = c(1, 2), .after = NULL)
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

# ------------------------------------------------------------------------------

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

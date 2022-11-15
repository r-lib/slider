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
  expect_snapshot({
    (expect_error(slide_index(1, 1:2, identity), class = "slider_error_index_incompatible_size"))
  })
})

test_that(".i must be ascending", {
  expect_snapshot({
    (expect_error(slide_index(1:2, 2:1, identity), class = "slider_error_index_must_be_ascending"))
  })
})

test_that("empty input returns a list, but after the index size check", {
  expect_equal(slide_index(integer(), integer(), ~.x), list())
  expect_snapshot({
    (expect_error(slide_index(integer(), 1, ~.x), class = "slider_error_index_incompatible_size"))
  })
})

test_that(".i must not contain NA values", {
  expect_error(slide_index(1:2, c(1, NA), identity), class = "slider_error_index_cannot_be_na")
  expect_snapshot({
    (expect_error(slide_index(1:2, c(NA, 1), identity), class = "slider_error_index_cannot_be_na"))
  })
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
  expect_snapshot({
    (expect_error(
      slide_index(1, 1, identity, .before = c(1L, 2L)),
      class = "vctrs_error_assert_size"
    ))
  })
})

test_that("error if .before is NULL", {
  expect_snapshot({
    (expect_error(
      slide_index(1, 1, identity, .before = NULL),
      class = "vctrs_error_scalar_type"
    ))
  })
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
      integer()
    )
  )

  expect_equal(
    slide_index(x, i, identity, .before = -1L, .after = 2L),
    list(
      2:3,
      3:4,
      4L,
      integer()
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
      integer()
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
      integer()
    )
  )
})

test_that("negative .before errors if its absolute value is past .after", {
  i <- new_date(c(0, 1, 2, 3))
  x <- i

  expect_snapshot(error = TRUE, {
    slide_index(x, i, identity, .before = -1, .after = 0)
  })
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

test_that("can use hour Durations with POSIXct", {
  i <- lubridate::as_datetime(new_date(c(0, 1, 2, 3)))
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
    slide_index(x, i, identity, .before = 24 * 60 * 60)
  )
})

test_that("can use day Durations with POSIXct", {
  i <- lubridate::as_datetime(new_date(c(0, 1, 2, 3)))
  x <- seq_along(i)

  expect_equal(
    slide_index(x, i, identity, .before = lubridate::ddays(1)),
    slide_index(x, i, identity, .before = 24 * 60 * 60)
  )

  expect_equal(
    slide_index(x, i, identity, .before = lubridate::ddays(2)),
    slide_index(x, i, identity, .before = 2 * 24 * 60 * 60)
  )
})

test_that("can use week Durations with POSIXct", {
  i <- lubridate::as_datetime(new_date(c(0, 6, 7, 8)))
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

test_that("can use year Durations with POSIXct", {
  i <- lubridate::as_datetime(new_date(c(0, 365, 365 * 2)))
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

test_that("can use negative Durations with POSIXct", {
  i <- lubridate::as_datetime(new_date(c(0, 1, 2, 3)))
  x <- seq_along(i)

  expect_equal(
    slide_index(x, i, identity, .before = -lubridate::ddays(1), .after = lubridate::ddays(1)),
    list(
      2L,
      3L,
      4L,
      integer()
    )
  )

  expect_equal(
    slide_index(x, i, identity, .before = -lubridate::ddays(1), .after = lubridate::ddays(2)),
    list(
      2:3,
      3:4,
      4L,
      integer()
    )
  )
})

test_that("errors if negative .before Duration is further than .after", {
  i <- lubridate::as_datetime(new_date(c(0, 1, 2, 3)))
  x <- seq_along(i)

  expect_snapshot(error = TRUE, {
    slide_index(x, i, identity, .before = -lubridate::ddays(1), .after = 0)
  })
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

# ------------------------------------------------------------------------------
# .before - lubridate - Periods

test_that("can use hour Periods with POSIXct", {
  i <- lubridate::as_datetime(new_date(c(0, 1, 2, 3)))
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
    list(
      1L,
      1:2,
      2:3,
      3:4
    )
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
  i <- lubridate::as_datetime(i)
  before <- lubridate::seconds(604799)

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

  expect_snapshot({
    (expect_error(
      slide_index(x, i, identity, .before = months(1)),
      class = "slider_error_generated_endpoints_cannot_be_na"
    ))
  })
})

# ------------------------------------------------------------------------------
# .before - lubridate - Leap Years / DST

test_that("can use year Durations/Periods with Dates and leap years", {
  # 2008 = leap year
  i <- as.POSIXct(c("2008-01-01", "2009-01-01", "2010-01-01"), "UTC")
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
  expect_snapshot({
    (expect_error(
      slide_index(1, 1, identity, .after = c(1L, 2L)),
      class = "vctrs_error_assert_size"
    ))
  })
})

test_that("error if .after is NULL", {
  expect_snapshot({
    (expect_error(
      slide_index(1, 1, identity, .after = NULL),
      class = "vctrs_error_scalar_type"
    ))
  })
})

# ------------------------------------------------------------------------------
# .after - negative

test_that("can use a negative .after with integer index", {
  i <- 1:5
  x <- i

  expect_equal(
    slide_index(x, i, identity, .before = 2L, .after = -1L),
    list(
      integer(),
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
      integer(),
      1L,
      2L,
      3L
    )
  )

  expect_equal(
    slide_index(x, i, identity, .after = -1L, .before = 2L),
    list(
      integer(),
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
      integer(),
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
      integer(),
      1,
      1,
      integer()
    )
  )
})

test_that("negative .after errors if its absolute value is past .before", {
  i <- new_date(c(0, 1, 2, 3))
  x <- i

  expect_snapshot(error = TRUE, {
    slide_index(x, i, identity, .after = -1, .before = 0)
  })
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

test_that("can use hour Periods with POSIXct", {
  i <- lubridate::as_datetime(new_date(c(0, 1, 2, 3)))
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
    list(
      1:2,
      2:3,
      3:4,
      4L
    )
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
  i <- lubridate::as_datetime(i)
  after <- lubridate::seconds(604799)

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

  expect_snapshot({
    (expect_error(
      slide_index(x, i, identity, .after = months(1)),
      class = "slider_error_generated_endpoints_cannot_be_na"
    ))
  })
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
# .before / .after - non-vctrs types

test_that(".i/.before/.after can be non-vctrs types (#182)", {
  local_c_foobar()

  i <- foobar(1:4)
  x <- seq_along(i)

  expect_identical(
    slide_index(x, i, identity, .before = 2L),
    list(
      1L,
      1:2,
      1:3,
      2:4
    )
  )
  expect_identical(
    slide_index(x, i, identity, .before = foobar(2L)),
    list(
      1L,
      1:2,
      1:3,
      2:4
    )
  )

  expect_identical(
    slide_index(x, i, identity, .after = 2L),
    list(
      1:3,
      2:4,
      3:4,
      4L
    )
  )
  expect_identical(
    slide_index(x, i, identity, .after = foobar(2L)),
    list(
      1:3,
      2:4,
      3:4,
      4L
    )
  )
})

# ------------------------------------------------------------------------------
# .before / .after - function

test_that(".before/.after - can use a function", {
  x <- 1:5

  expect_identical(
    slide_index(x, x, identity, .before = function(.x) .x - 2),
    slide_index(x, x, identity, .before = 2)
  )
  expect_identical(
    slide_index(x, x, identity, .before = ~.x - 2),
    slide_index(x, x, identity, .before = 2)
  )
  expect_identical(
    slide_index(x, x, identity, .after = function(.x) .x + 2),
    slide_index(x, x, identity, .after = 2)
  )
  expect_identical(
    slide_index(x, x, identity, .after = ~.x + 2),
    slide_index(x, x, identity, .after = 2)
  )
})

test_that(".before/.after - using a function can help with lubridate `+ months(1)` invalid date issues", {
  x <- as.Date(c("2019-01-31", "2019-02-28", "2019-03-31"))

  expect_identical(
    slide_index(x, x, identity, .before = ~lubridate::add_with_rollback(.x, months(-1))),
    list(x[1], x[1:2], x[2:3])
  )
  expect_identical(
    slide_index(x, x, identity, .after = ~lubridate::add_with_rollback(.x, months(1))),
    list(x[1:2], x[2], x[3])
  )
})

test_that(".before/.after - generated endpoints must be in weakly ascending order", {
  x <- c(1, 2)

  expect_snapshot({
    (expect_error(
      slide_index(x, x, identity, .before = ~.x - c(2, 4)),
      class = "slider_error_generated_endpoints_must_be_ascending"
    ))
    (expect_error(
      slide_index(x, x, identity, .after = ~.x + c(4, 2)),
      class = "slider_error_generated_endpoints_must_be_ascending"
    ))
  })
})

test_that(".before/.after - generated endpoints must maintain .before <= .after ordering", {
  expect_snapshot({
    (expect_error(
      slide_index(1:2, 1:2, identity, .before = ~.x + 1, .after = 0)
    ))
    (expect_error(
      slide_index(1:2, 1:2, identity, .before = 0, .after = ~.x - 1)
    ))
  })
})

test_that(".before/.after - generated endpoints can't be NA", {
  expect_snapshot({
    (expect_error(
      slide_index(1:2, 1:2, identity, .before = ~rep(NA_integer_, length(.x)))
    ))
    (expect_error(
      slide_index(1:2, 1:2, identity, .after = ~rep(NA_integer_, length(.x)))
    ))
  })
})

test_that(".before/.after - generated endpoints shouldn't rely on original `.i` length", {
  # Duplicates (peers) will be removed
  x <- c(1, 1)
  adjust <- c(2, 3)

  expect_snapshot({
    (expect_error(
      slide_index(x, x, identity, .before = ~.x - adjust),
      class = "slider_error_generated_endpoints_incompatible_size"
    ))
    (expect_error(
      slide_index(x, x, identity, .after = ~.x + adjust),
      class = "slider_error_generated_endpoints_incompatible_size"
    ))
  })
})

test_that(".before/.after - function must have 1 argument", {
  # Base R errors
  expect_error(slide_index(1, 1, identity, .before = function(x, y) x + y))
  expect_error(slide_index(1, 1, identity, .before = ~.x + .y))
  expect_error(slide_index(1, 1, identity, .after = function(x, y) x + y))
  expect_error(slide_index(1, 1, identity, .after = ~.x + .y))
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
  expect_snapshot({
    (expect_error(
      slide_index(1, 1, identity, .complete = "hi"),
      class = "vctrs_error_incompatible_type"
    ))
  })
})

# ------------------------------------------------------------------------------
# unbounded

test_that("can use unbounded .before", {
  i <- new_date(c(0, 1, 2, 3, 4))
  x <- seq_along(i)

  expect_equal(
    slide_index(x, i, identity, .before = Inf),
    list(
      1L,
      1:2,
      1:3,
      1:4,
      1:5
    )
  )
})

test_that("can use unbounded .after", {
  i <- new_date(c(0, 1, 2, 3, 4))
  x <- seq_along(i)

  expect_equal(
    slide_index(x, i, identity, .after = Inf),
    list(
      1:5,
      2:5,
      3:5,
      4:5,
      5L
    )
  )
})

test_that("can use unbounded .before with positive .after", {
  i <- new_date(c(0, 1, 2, 3, 4))
  x <- seq_along(i)

  expect_equal(
    slide_index(x, i, identity, .before = Inf, .after = 1),
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
    slide_index(x, i, identity, .before = Inf, .after = -1),
    list(
      integer(),
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
    slide_index(x, i, identity, .before = Inf, .after = lubridate::days(2)),
    list(
      1:3,
      1:4,
      1:5,
      1:5,
      1:5
    )
  )
})

test_that("can be doubly unbounded", {
  i <- new_date(c(0, 1, 2, 3, 4))
  x <- seq_along(i)

  expect_equal(
    slide_index(x, i, identity, .before = Inf, .after = Inf),
    list(
      1:5,
      1:5,
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

test_that("row names are extracted from data frames", {
  x <- data.frame(x = 1:5, row.names = letters[1:5])
  i <- vec_seq_along(x)
  expect_equal(names(slide_index(x, i, ~.x)), letters[1:5])
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

  names <- letters[1:5]
  x <- data.frame(x = 1:5, row.names = names)
  i <- vec_seq_along(x)
  expect <- set_names(as.list(names), names)
  expect_equal(slide_index(x, i, ~rownames(.x)), expect)

  names <- c("r1", "r2")
  x <- array(1:4, c(2, 2), dimnames = list(names, c("c1", "c2")))
  i <- vec_seq_along(x)
  exp <- set_names(as.list(names), names)
  expect_equal(slide_index(x, i, ~rownames(.x)), exp)
})

# ------------------------------------------------------------------------------
# .i types

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

test_that("`.i - .before` must be castable to `.i`", {
  i <- 1L

  expect_snapshot({
    (expect_error(
      slide_index(1, i, identity, .before = 1.5),
      class = "vctrs_error_cast_lossy"
    ))
  })
})

test_that("`.i + .after` must be castable to `.i`", {
  i <- 1L

  expect_snapshot({
    (expect_error(
      slide_index(1, i, identity, .after = 1.5),
      class = "vctrs_error_cast_lossy"
    ))
  })
})

# ------------------------------------------------------------------------------
# misc

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
      integer(),
      integer(),
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
    rep(list(integer()), 5)
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

test_that("lambdas are equivalent to functions (#10)", {
  expect_equal(
    slide_index(1:10, 1:10, sum, .before = 3),
    slide_index(1:10, 1:10, ~sum(.x), .before = 3)
  )
})

test_that("slide_index() forces arguments in the same way as base R / map()", {
  f_slide <- slide_index(1:2, 1:2, function(i) function(x) x + i)
  f_base <- lapply(1:2, function(i) function(x) x + i)

  expect_equal(f_slide[[1]](0), f_base[[1]](0))
  expect_equal(f_slide[[2]](0), f_base[[2]](0))
})

test_that("stress test that we don't stack overflow (#34)", {
  expect_error(slide_index(1:1e6, 1:1e6, ~.x), NA)
})

test_that(paste0(
            "proof that the `stops_pos < starts_pos` check is required for ",
            "cases where we have an irregular series and ",
            "the window is completely between values"
          ), {

  expect_equal(
    slide_index(1:3, c(1, 3, 4), identity, .before = -1, .after = 1),
    list(integer(), 3, integer())
  )
})

test_that("`error_call` and `.error_call` args aren't swallowed", {
  fn <- function(x, error_call) {
    abort("hi", call = error_call)
  }
  fn_dot <- function(x, .error_call) {
    abort("hi", call = .error_call)
  }

  expect_snapshot(error = TRUE, {
    slide_index(1, 1, fn, error_call = call("foo"))
  })
  expect_snapshot(error = TRUE, {
    slide_index(1, 1, fn_dot, .error_call = call("foo"))
  })
})

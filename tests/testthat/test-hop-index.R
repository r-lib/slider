test_that("trivial case works", {
  expect_equal(
    hop_index(1:2, 1:2, 1:2, 1:2, ~.x),
    list(1L, 2L)
  )
})

test_that("can work with with Date `.i`", {
  i <- new_date(c(0, 1, 2, 3))
  x <- 1:4

  expect_equal(
    hop_index(x, i, i, i, identity),
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
    (expect_error(hop_index(1, 1:2, 1, 1, identity), class = "slider_error_index_incompatible_size"))
  })
})

test_that(".i must be ascending", {
  expect_snapshot({
    (expect_error(hop_index(1:2, 2:1, 1:2, 1:2, identity), class = "slider_error_index_must_be_ascending"))
  })
})

test_that(".starts must be ascending", {
  expect_snapshot({
    (expect_error(hop_index(1:2, 1:2, 2:1, 1:2, identity), class = "slider_error_endpoints_must_be_ascending"))
  })
})

test_that(".stops must be ascending", {
  expect_snapshot({
    (expect_error(hop_index(1:2, 1:2, 1:2, 2:1, identity), class = "slider_error_endpoints_must_be_ascending"))
  })
})

test_that("empty input returns a list, but after the index size check", {
  expect_equal(hop_index(integer(), integer(), integer(), integer(), ~.x), list())
  expect_error(hop_index(integer(), 1, integer(), integer(), ~.x), class = "slider_error_index_incompatible_size")
})

test_that("empty `.x` and `.i`, but size `n > 0` `.starts` and `.stops` returns size `n` ptype", {
  expect_equal(hop_index(integer(), integer(), 1:2, 2:3, ~.x), list(integer(), integer()))
})

test_that("empty `.x` and `.i`, but size `n > 0` `.starts` and `.stops`: sizes and types are checked first", {
  expect_snapshot({
    (expect_error(hop_index(integer(), integer(), 1:3, 1:2, ~.x), class = "vctrs_error_incompatible_size"))
    (expect_error(hop_index(integer(), integer(), 1, "x", ~.x), class = "vctrs_error_incompatible_type"))
  })
})

test_that(".i must not contain NA values", {
  expect_snapshot({
    (expect_error(hop_index(1:2, c(1, NA), 1:2, 1:2, identity), class = "slider_error_index_cannot_be_na"))
    (expect_error(hop_index(1:2, c(NA, 1), 1:2, 1:2, identity), class = "slider_error_index_cannot_be_na"))
  })
})

test_that(".starts must not contain NA values", {
  expect_snapshot({
    (expect_error(hop_index(1:2, 1:2, c(1, NA), 1:2, identity), class = "slider_error_endpoints_cannot_be_na"))
    (expect_error(hop_index(1:2, 1:2, c(NA, 1), 1:2, identity), class = "slider_error_endpoints_cannot_be_na"))
  })
})

test_that(".stops must not contain NA values", {
  expect_snapshot({
    (expect_error(hop_index(1:2, 1:2, 1:2, c(1, NA), identity), class = "slider_error_endpoints_cannot_be_na"))
    (expect_error(hop_index(1:2, 1:2, 1:2, c(NA, 1), identity), class = "slider_error_endpoints_cannot_be_na"))
  })
})

test_that("recycling is used for .starts/.stops", {
  expect_equal(
    hop_index(1:2, 1:2, 1, 1:2, ~.x),
    list(
      1L,
      1:2
    )
  )

  expect_equal(
    hop_index(1:2, 1:2, 1:2, 2, ~.x),
    list(
      1:2,
      2L
    )
  )

  expect_snapshot({
    (expect_error(hop_index(1:2, 1:2, 1:2, 1:3, ~.x), class = "vctrs_error_incompatible_size"))
  })
})

test_that("0 length .starts/.stops are allowed", {
  expect_equal(hop_index(1, 1, integer(), integer(), ~.x), list())
})

test_that(".starts and .stops are cast to .i", {
  i <- new_date(c(0, 1))
  starts <- c("x", "y")
  stops <- i

  expect_snapshot({
    (expect_error(
      hop_index(1:2, i, starts, stops, ~.x),
      class = "vctrs_error_incompatible_type"
    ))
  })
})

test_that("output size is the common size of .starts/.stops", {
  expect_equal(
    hop_index(1:5, 1:5, 1, 2, ~.x),
    list(1:2)
  )

  expect_equal(
    hop_index(1:2, 1:2, c(1, 1, 2), c(1, 2, 2), ~.x),
    list(1L, 1:2, 2L)
  )
})

test_that("out of bounds .starts/.stops result in NULLs", {
  expect_equal(
    hop_index(1:2, 1:2, 3, 4, ~.x),
    list(integer())
  )

  expect_equal(
    hop_index(1:2, 1:2, c(3, 4), c(4, 6), ~.x),
    list(integer(), integer())
  )

  expect_equal(
    hop_index(1:2, 1:2, c(-1, 4), c(0, 6), ~.x),
    list(integer(), integer())
  )

  expect_equal(
    hop_index(1:2, 1:2, c(-1, 1, 4), c(0, 2, 6), ~.x),
    list(integer(), 1:2, integer())
  )
})

test_that("indexing into gaps in an irregular .i results in 0 size .x values", {
  expect_equal(
    hop_index(1:4, c(1, 2, 5, 6), 3, 4, ~.x),
    list(integer())
  )

  expect_equal(
    hop_index(1:4, c(1, 2, 5, 6), c(3, 3, 3), c(3, 4, 5), ~.x),
    list(integer(), integer(), 3)
  )
})

test_that("duplicated .starts/.stops pairs are allowed", {
  expect_equal(
    hop_index(1:4, 1:4, c(1, 2, 2), c(2, 2, 2), ~.x),
    list(
      1:2,
      2L,
      2L
    )
  )
})

# ------------------------------------------------------------------------------
# nonexistant dates with lubridate::months()

test_that("can use `%m-%` and `add_with_rollback()` to solve month rollback issues", {
  requireNamespace("lubridate", quietly = TRUE)
  `%m-%` <- lubridate::`%m-%`

  i <- vec_c(as.Date("2019-02-27") + 0:3, as.Date("2019-03-27") + 0:5)
  x <- seq_along(i)

  starts <- i %m-% months(1)
  stops <- i

  # 3/27 rollback to 2/27
  # 3/28 rollback to 2/28
  # 3/29 rollback to 2/28
  # 3/30 rollback to 2/28
  # 3/31 rollback to 2/28
  # 4/01 rollback to 3/01
  expect_equal(
    hop_index(x, i, starts, stops, identity),
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

  starts <- lubridate::add_with_rollback(i, -months(1), roll_to_first = TRUE)
  stops <- i

  # 3/27 rollback to 2/27
  # 3/28 rollback to 2/28
  # 3/29 rollback to 2/28 then forward to 3/01
  # 3/30 rollback to 2/28 then forward to 3/01
  # 3/31 rollback to 2/28 then forward to 3/01
  # 4/01 rollback to 3/01
  expect_equal(
    hop_index(x, i, starts, stops, identity),
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
# data frame indices

test_that("can order by two vectors using a data frame", {
  i <- data.frame(
    date1 = new_date(c(0, 3, 4, 5)),
    date2 = new_date(c(0, 1, 2, 4))
  )

  before <- data.frame(date1 = 2, date2 = 1)
  starts <- i - vec_recycle(before, vec_size(i))

  stops <- i

  # NOTE - This is a bit tricky. It always tries to determine the comparison
  # order using the first column that it comes across. If the values are equal,
  # only then will it look to the second column

  expect_equal(
    hop_index(i, i, starts, stops, ~.x),
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

test_that("can use a data frame index where the first column breaks ties (#133)", {
  i <- vec_c(
    data.frame(year = 2019, month = c(4, 5, 5, 6, 7, 8)),
    data.frame(year = 2020, month = 1:4)
  )
  starts <- data.frame(year = 2019, month = 5:6)
  stops <- data.frame(year = 2020, month = 2:3)

  expect_identical(
    hop_index(i, i, starts, stops, identity),
    list(
      vec_slice(i, 2:8),
      vec_slice(i, 4:9)
    )
  )
})

test_that("can select no rows when using a data frame index", {
  i <- data.frame(year = 2020, month = 2)
  starts <- data.frame(year = 2020, month = 3)
  stops <- data.frame(year = 2020, month = 4)

  expect_identical(
    hop_index(i, i, starts, stops, identity),
    list(vec_slice(i, NULL))
  )
})

# ------------------------------------------------------------------------------
# input names

test_that("names exist on inner sliced elements", {
  names <- letters[1:5]
  x <- set_names(1:5, names)
  exp <- as.list(names)
  expect_equal(hop_index(x, 1:5, 1:5, 1:5, ~names(.x)), exp)
})

test_that("names are never placed on the output", {
  x <- set_names(1:5, letters[1:5])
  expect_null(names(hop_index(x, 1:5, 1:5, 1:5, ~.x)))
})

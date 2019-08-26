test_that("trivial case works", {
  expect_equal(
    slide_between(1:2, 1:2, 1:2, 1:2, ~.x),
    list(1L, 2L)
  )
})

test_that("can work with with Date `.i`", {
  i <- new_date(c(0, 1, 2, 3))
  x <- 1:4

  expect_equal(
    slide_between(x, i, i, i, identity),
    list(
      1L,
      2L,
      3L,
      4L
    )
  )
})

test_that(".x must be the same size as .i", {
  expect_error(slide_between(1, 1:2, 1, 1, identity), "must be the same")
})

test_that(".i must be ascending", {
  expect_error(slide_between(1:2, 2:1, 1:2, 1:2, identity), "`.i`ndex must be in ascending order")
})

test_that(".starts must be ascending", {
  expect_error(slide_between(1:2, 1:2, 2:1, 1:2, identity), "`.starts` must be in ascending order")
})

test_that(".stops must be ascending", {
  expect_error(slide_between(1:2, 1:2, 1:2, 2:1, identity), "`.stops` must be in ascending order")
})

test_that("empty input returns a list, but after the index size check", {
  expect_equal(slide_between(integer(), integer(), integer(), integer(), ~.x), list())
  expect_error(slide_between(integer(), 1, integer(), integer(), ~.x), "must be the same")
})

test_that(".i must not contain NA values", {
  expect_error(slide_between(1:2, c(1, NA), 1:2, 1:2, identity), "found at location[(]s[)]: 2")
  expect_error(slide_between(1:2, c(NA, 1), 1:2, 1:2, identity), "found at location[(]s[)]: 1")
})

test_that(".starts must not contain NA values", {
  expect_error(slide_between(1:2, 1:2, c(1, NA), 1:2, identity), "found at location[(]s[)]: 2")
  expect_error(slide_between(1:2, 1:2, c(NA, 1), 1:2, identity), "found at location[(]s[)]: 1")
})

test_that(".stops must not contain NA values", {
  expect_error(slide_between(1:2, 1:2, 1:2, c(1, NA), identity), "found at location[(]s[)]: 2")
  expect_error(slide_between(1:2, 1:2, 1:2, c(NA, 1), identity), "found at location[(]s[)]: 1")
})

test_that("recycling is used for .starts/.stops", {
  expect_equal(
    slide_between(1:2, 1:2, 1, 1:2, ~.x),
    list(
      1L,
      1:2
    )
  )

  expect_equal(
    slide_between(1:2, 1:2, 1:2, 2, ~.x),
    list(
      1:2,
      2L
    )
  )

  expect_error(slide_between(1:2, 1:2, 1:2, 1:3, ~.x), class = "vctrs_error_incompatible_size")
})

test_that("0 length .starts/.stops are allowed", {
  expect_equal(slide_between(1, 1, integer(), integer(), ~.x), list())
})

test_that("common type is found among .i/.starts/.stops", {
  i <- new_date(c(0, 1))
  start <- vec_cast(i[1], new_datetime(0))
  stop <- i[2]

  expect_equal(
    slide_between(1:2, i, start, stop, ~.x),
    list(
      1:2
    )
  )
})

test_that("output size is the common size of .starts/.stops", {
  expect_equal(
    slide_between(1:5, 1:5, 1, 2, ~.x),
    list(1:2)
  )

  expect_equal(
    slide_between(1:2, 1:2, c(1, 1, 2), c(1, 2, 2), ~.x),
    list(1L, 1:2, 2L)
  )
})

test_that("out of bounds .starts/.stops result in NULLs", {
  expect_equal(
    slide_between(1:2, 1:2, 3, 4, ~.x),
    list(NULL)
  )

  expect_equal(
    slide_between(1:2, 1:2, c(3, 4), c(4, 6), ~.x),
    list(NULL, NULL)
  )

  expect_equal(
    slide_between(1:2, 1:2, c(-1, 4), c(0, 6), ~.x),
    list(NULL, NULL)
  )

  expect_equal(
    slide_between(1:2, 1:2, c(-1, 1, 4), c(0, 2, 6), ~.x),
    list(NULL, 1:2, NULL)
  )
})

test_that("indexing into gaps in an irregular .i results in 0 size .x values", {
  expect_equal(
    slide_between(1:4, c(1, 2, 5, 6), 3, 4, ~.x),
    list(integer())
  )

  expect_equal(
    slide_between(1:4, c(1, 2, 5, 6), c(3, 3, 3), c(3, 4, 5), ~.x),
    list(integer(), integer(), 3)
  )
})

test_that("duplicated .starts/.stops pairs are allowed", {
  expect_equal(
    slide_between(1:4, 1:4, c(1, 2, 2), c(2, 2, 2), ~.x),
    list(
      1:2,
      2L,
      2L
    )
  )
})

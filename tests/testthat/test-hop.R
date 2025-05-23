test_that("trivial case works", {
  expect_equal(
    hop(1:2, 1:2, 1:2, ~.x),
    list(1L, 2L)
  )
})

test_that(".starts and .stops don't have to be ascending", {
  expect_equal(hop(1:5, c(2, 1), c(3, 2), identity), list(2:3, 1:2))
})

test_that(".starts must be before .stops", {
  expect_snapshot({
    (expect_error(hop(1:5, c(2, 3, 1), c(1, 1, 2), identity)))
    (expect_error(hop(1:5, c(2, 3, 1), c(1, 1, 2), identity)))
  })
})

test_that("empty input returns a list", {
  expect_equal(hop(integer(), integer(), integer(), ~.x), list())
})

test_that("empty `.x`, but size `n > 0` `.starts` and `.stops` returns size `n` empty ptype", {
  expect_equal(hop(integer(), 1:2, 2:3, ~.x), list(integer(), integer()))
})

test_that("empty `.x`, but size `n > 0` `.starts` and `.stops`: sizes and types are checked first", {
  expect_snapshot({
    (expect_error(
      hop(integer(), 1:3, 1:2, ~.x),
      class = "vctrs_error_incompatible_size"
    ))
    (expect_error(
      hop(integer(), 1, "x", ~.x),
      class = "vctrs_error_subscript_type"
    ))
  })
})

test_that(".starts must not contain NA values", {
  expect_snapshot({
    (expect_error(
      hop(1:2, c(1, NA), 1:2, identity),
      class = "slider_error_endpoints_cannot_be_na"
    ))
    (expect_error(
      hop(1:2, c(NA, 1), 1:2, identity),
      class = "slider_error_endpoints_cannot_be_na"
    ))
  })
})

test_that(".stops must not contain NA values", {
  expect_snapshot({
    (expect_error(
      hop(1:2, 1:2, c(1, NA), identity),
      class = "slider_error_endpoints_cannot_be_na"
    ))
    (expect_error(
      hop(1:2, 1:2, c(NA, 1), identity),
      class = "slider_error_endpoints_cannot_be_na"
    ))
  })
})

test_that("recycling is used for .starts/.stops", {
  expect_equal(
    hop(1:2, 1, 1:2, ~.x),
    list(
      1L,
      1:2
    )
  )

  expect_equal(
    hop(1:2, 1:2, 2, ~.x),
    list(
      1:2,
      2L
    )
  )

  expect_snapshot({
    expect_error(
      hop(1:2, 1:2, 1:3, ~.x),
      class = "vctrs_error_incompatible_size"
    )
  })
})

test_that("0 length .starts/.stops are allowed", {
  expect_equal(hop(1, integer(), integer(), ~.x), list())
})

test_that("output size is the common size of .starts/.stops", {
  expect_equal(
    hop(1:5, 1, 2, ~.x),
    list(1:2)
  )

  expect_equal(
    hop(1:2, c(1, 1, 2), c(1, 2, 2), ~.x),
    list(1L, 1:2, 2L)
  )
})

test_that("out of bounds .starts/.stops result in size-0 slices", {
  expect_equal(
    hop(1:2, 3, 4, ~.x),
    list(integer())
  )

  expect_equal(
    hop(1:2, c(3, 4), c(4, 6), ~.x),
    list(integer(), integer())
  )
})

test_that("negative / 0 out of bounds .starts/.stops result in size-0 slices", {
  expect_equal(
    hop(1:2, c(-1, 4), c(0, 6), ~.x),
    list(integer(), integer())
  )

  expect_equal(
    hop(1:2, c(-1, 1, 4), c(0, 2, 6), ~.x),
    list(integer(), 1:2, integer())
  )
})

test_that("duplicated .starts/.stops pairs are allowed", {
  expect_equal(
    hop(1:4, c(1, 2, 2), c(2, 2, 2), ~.x),
    list(
      1:2,
      2L,
      2L
    )
  )
})

test_that("`.starts` and `.stops` must be integerish", {
  expect_snapshot({
    (expect_error(
      hop(1, "x", 1, identity),
      class = "vctrs_error_subscript_type"
    ))
    (expect_error(
      hop(1, 1, "x", identity),
      class = "vctrs_error_subscript_type"
    ))
  })
})

test_that("`error_call` and `.error_call` args aren't swallowed", {
  fn <- function(x, error_call) {
    abort("hi", call = error_call)
  }
  fn_dot <- function(x, .error_call) {
    abort("hi", call = .error_call)
  }

  expect_snapshot(error = TRUE, {
    hop(1, 1, 1, fn, error_call = call("foo"))
  })
  expect_snapshot(error = TRUE, {
    hop(1, 1, 1, fn_dot, .error_call = call("foo"))
  })
})

# ------------------------------------------------------------------------------
# input names

test_that("names exist on inner sliced elements", {
  names <- letters[1:5]
  x <- set_names(1:5, names)
  exp <- as.list(names)
  expect_equal(hop(x, 1:5, 1:5, ~ names(.x)), exp)
})

test_that("names are never placed on the output", {
  x <- set_names(1:5, letters[1:5])
  expect_null(names(hop(x, 1:5, 1:5, ~.x)))
})

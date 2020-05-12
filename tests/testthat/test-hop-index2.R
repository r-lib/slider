test_that("empty input returns a list, but after the index size check", {
  expect_equal(
    hop_index2(
      .x = integer(),
      .y = integer(),
      .i = integer(),
      .starts = integer(),
      .stops = integer(),
      .f = ~.x
    ),
    list()
  )

  expect_equal(
    hop_index2(
      .x = integer(),
      .y = 1,
      .i = integer(),
      .starts = integer(),
      .stops = integer(),
      .f = ~.x
    ),
    list()
  )

  expect_equal(
    hop_index2(
      .x = 1,
      .y = integer(),
      .i = integer(),
      .starts = integer(),
      .stops = integer(),
      .f = ~.x
    ),
    list()
  )

  expect_error(
    hop_index2(
      .x = integer(),
      .y = integer(),
      .i = 1,
      .starts = integer(),
      .stops = integer(),
      .f = ~.x
    ),
    class = "slider_error_index_incompatible_size"
  )
})

test_that("empty `.x` and `.y` and `.i`, but size `n > 0` `.starts` and `.stops` returns size `n` empty ptype", {
  expect_equal(hop_index2(integer(), integer(), integer(), 1:2, 2:3, ~.x), list(integer(), integer()))
})

test_that("empty `.x` and `.y` and `.i`, but size `n > 0` `.starts` and `.stops`: sizes and types are checked first", {
  expect_error(hop_index2(integer(), integer(), integer(), 1:3, 1:2, ~.x), class = "vctrs_error_incompatible_size")
  expect_error(hop_index2(integer(), integer(), integer(), 1, "x", ~.x), class = "vctrs_error_incompatible_type")
})

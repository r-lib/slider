test_that("empty input returns a list, but after the index size check", {
  expect_equal(
    slide_between2(
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
    slide_between2(
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
    slide_between2(
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
    slide_between2(
      .x = integer(),
      .y = integer(),
      .i = 1,
      .starts = integer(),
      .stops = integer(),
      .f = ~.x
    ),
    "must be the same"
  )
})

test_that("empty `.x` and `.y` and `.i`, but size `n > 0` `.starts` and `.stops` returns size `n` empty ptype", {
  expect_equal(slide_between2(integer(), integer(), integer(), 1:2, 2:3, ~.x), list(NULL, NULL))
})

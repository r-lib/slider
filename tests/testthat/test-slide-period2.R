test_that("empty input returns a list, but after the index size check", {
  expect_equal(
    slide_period2(
      .x = integer(),
      .y = integer(),
      .i = structure(numeric(), class = "Date"),
      .period = "day",
      .f = ~.x
    ),
    list()
  )

  expect_equal(
    slide_period2(
      .x = integer(),
      .y = 1,
      .i = structure(numeric(), class = "Date"),
      .period = "day",
      .f = ~.x
    ),
    list()
  )

  expect_equal(
    slide_period2(
      .x = 1,
      .y = integer(),
      .i = structure(numeric(), class = "Date"),
      .period = "day",
      .f = ~.x
    ),
    list()
  )

  expect_snapshot({
    (expect_error(
      slide_period2(
        .x = integer(),
        .y = integer(),
        .i = structure(0, class = "Date"),
        .period = "day",
        .f = ~.x
      ),
      class = "slider_error_index_incompatible_size"
    ))
  })
})

test_that("empty input works with `.complete = TRUE` (#111)", {
  expect_equal(
    slide_period2(
      integer(),
      integer(),
      new_date(),
      "year",
      ~.x,
      .complete = TRUE
    ),
    list()
  )
})

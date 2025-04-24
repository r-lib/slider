test_that("empty input returns a list, but after the index size check", {
  i <- new_date()

  expect_equal(pslide_period(list(integer(), integer()), i, "day", ~.x), list())
  expect_equal(pslide_period(list(integer(), 1), i, "day", ~.x), list())
  expect_equal(pslide_period(list(1, integer()), i, "day", ~.x), list())

  i <- new_date(0)

  expect_snapshot({
    (expect_error(
      pslide_period(list(integer(), integer()), i, "day", ~.x),
      class = "slider_error_index_incompatible_size"
    ))
  })
})

test_that("completely empty input returns a list", {
  expect_equal(pslide_period(list(), new_date(), "day", ~.x), list())
})

test_that("empty input works with `.complete = TRUE` (#111)", {
  expect_equal(
    pslide_period(
      list(integer(), integer()),
      new_date(),
      "year",
      ~.x,
      .complete = TRUE
    ),
    list()
  )
})

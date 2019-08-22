test_that("empty input returns a list, but after the index size check", {
  expect_equal(slide_index2(integer(), integer(), integer(), ~.x), list())
  expect_equal(slide_index2(integer(), 1, integer(), ~.x), list())
  expect_equal(slide_index2(1, integer(), integer(), ~.x), list())

  expect_error(slide_index2(integer(), integer(), 1, ~.x), "must be the same")
})

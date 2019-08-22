test_that("empty input returns a list, but after the index size check", {
  expect_equal(pslide_index(list(integer(), integer()), integer(), ~.x), list())
  expect_equal(pslide_index(list(integer(), 1), integer(), ~.x), list())
  expect_equal(pslide_index(list(1, integer()), integer(), ~.x), list())

  expect_error(pslide_index(list(integer(), integer()), 1, ~.x), "must be the same")
})

test_that("completely empty input returns a list", {
  expect_equal(pslide_index(list(), integer(), ~.x), list())
})

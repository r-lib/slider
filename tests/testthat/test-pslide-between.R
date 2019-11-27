test_that("empty input returns a list, but after the index size check", {
  expect_equal(pslide_between(list(integer(), integer()), integer(), integer(), integer(), ~.x), list())
  expect_equal(pslide_between(list(integer(), 1), integer(), integer(), integer(), ~.x), list())
  expect_equal(pslide_between(list(1, integer()), integer(), integer(), integer(), ~.x), list())

  expect_error(pslide_between(list(integer(), integer()), 1, integer(), integer(), ~.x), "must be the same")
})

test_that("completely empty input returns a list", {
  expect_equal(pslide_between(list(), integer(), integer(), integer(), ~.x), list())
})

test_that("empty `.l` and `.i`, but size `n > 0` `.starts` and `.stops` returns size `n` empty ptype", {
  expect_equal(pslide_between(list(), integer(), 1:2, 2:3, ~.x), list(NULL, NULL))
})

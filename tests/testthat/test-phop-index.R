test_that("empty input returns a list, but after the index size check", {
  expect_equal(phop_index(list(integer(), integer()), integer(), integer(), integer(), ~.x), list())
  expect_equal(phop_index(list(integer(), 1), integer(), integer(), integer(), ~.x), list())
  expect_equal(phop_index(list(1, integer()), integer(), integer(), integer(), ~.x), list())

  expect_error(phop_index(list(integer(), integer()), 1, integer(), integer(), ~.x), class = "slider_error_index_incompatible_size")
})

test_that("completely empty input returns a list", {
  expect_equal(phop_index(list(), integer(), integer(), integer(), ~.x), list())
})

test_that("empty `.l` and `.i`, but size `n > 0` `.starts` and `.stops` returns size `n` empty ptype", {
  expect_equal(phop_index(list(), integer(), 1:2, 2:3, ~.x), list(NULL, NULL))
})

test_that("empty `.l` and `.i`, but size `n > 0` `.starts` and `.stops`: sizes and types are checked first", {
  expect_error(phop_index(list(), integer(), 1:3, 1:2, ~.x), class = "vctrs_error_incompatible_size")
  expect_error(phop_index(list(), integer(), 1, "x", ~.x), class = "vctrs_error_incompatible_type")
})

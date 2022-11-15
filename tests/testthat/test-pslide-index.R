test_that("empty input returns a list, but after the index size check", {
  expect_equal(pslide_index(list(integer(), integer()), integer(), ~.x), list())
  expect_equal(pslide_index(list(integer(), 1), integer(), ~.x), list())
  expect_equal(pslide_index(list(1, integer()), integer(), ~.x), list())

  expect_snapshot({
    (expect_error(pslide_index(list(integer(), integer()), 1, ~.x), class = "slider_error_index_incompatible_size"))
  })
})

test_that("completely empty input returns a list", {
  expect_equal(pslide_index(list(), integer(), ~.x), list())
})

test_that("pslide_index() forces arguments in the same way as base R / pmap()", {
  f_slide <- pslide_index(list(1:2, 1:2, 1:2), 1:2, function(i, j, k) function(x) x + i + j + k)
  f_base <- mapply(function(i, j, k) function(x) x + i + j + k, 1:2, 1:2, 1:2)

  expect_equal(f_slide[[1]](0), f_base[[1]](0))
  expect_equal(f_slide[[2]](0), f_base[[2]](0))
})

test_that("empty input returns a list, but after the index size check", {
  expect_equal(slide_index2(integer(), integer(), integer(), ~.x), list())
  expect_equal(slide_index2(integer(), 1, integer(), ~.x), list())
  expect_equal(slide_index2(1, integer(), integer(), ~.x), list())

  expect_snapshot({
    (expect_error(
      slide_index2(integer(), integer(), 1, ~.x),
      class = "slider_error_index_incompatible_size"
    ))
  })
})

test_that("slide_index2() forces arguments in the same way as base R / map2()", {
  f_slide <- slide_index2(1:2, 1:2, 1:2, function(i, j) function(x) x + i + j)
  f_base <- mapply(function(i, j) function(x) x + i + j, 1:2, 1:2)

  expect_equal(f_slide[[1]](0), f_base[[1]](0))
  expect_equal(f_slide[[2]](0), f_base[[2]](0))
})

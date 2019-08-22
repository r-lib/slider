test_that("can create an unbounded() object", {
  expect_is(
    unbounded(),
    "slide_box_unbounded"
  )
})

test_that("prints <unbounded>", {
  expect_equal(capture.output(unbounded()), "<unbounded>")
})

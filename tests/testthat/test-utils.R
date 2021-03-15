test_that("`is_unbounded()` returns `FALSE` for classed objects", {
  # We only care about checking if an exact literal `Inf` is supplied.
  # We don't want to call `is.infinite()` on any object, as vctrs classes
  # don't support `is.infinite()` by default and error if `is.infinite()`
  # is called.
  x <- structure(Inf, class = "foobar")
  expect_false(is_unbounded(x))
})

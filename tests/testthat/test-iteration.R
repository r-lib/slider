test_that("can retrieve the iteration", {
  expect_identical(slide_int(6:10, ~slider_iteration()), 1:5)
})

test_that("can compute the correct iteration in nested slide calls", {
  x <- slide(1:3, ~list(slider_iteration(), slide_int(1:4, ~slider_iteration())))
  y <- slide(1:3, ~list(slide_int(1:4, ~slider_iteration()), slider_iteration()))

  x_expect <- list(
    list(1L, 1:4),
    list(2L, 1:4),
    list(3L, 1:4)
  )

  y_expect <- list(
    list(1:4, 1L),
    list(1:4, 2L),
    list(1:4, 3L)
  )

  expect_identical(x, x_expect)
  expect_identical(y, y_expect)
})

test_that("iteration is restored on jumps", {
  x <- slide(
    1:3,
    ~list(
      try(slide_int(1:3, ~ stop("foo")), silent = TRUE),
      slider_iteration()
    )
  )

  expect_identical(x[[1]][[2]], 1L)
  expect_identical(x[[2]][[2]], 2L)
  expect_identical(x[[3]][[2]], 3L)
})

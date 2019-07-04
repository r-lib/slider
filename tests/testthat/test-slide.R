test_that("default settings is the same as map()", {
  expect_equal(slide(1:5, identity), as.list(1:5))
})

# ------------------------------------------------------------------------------
# .size

test_that("can slide over a window", {
  expect_equal(
    slide(1:7, identity, .size = 2),
    list(
      NULL,
      c(1, 2),
      c(2, 3),
      c(3, 4),
      c(4, 5),
      c(5, 6),
      c(6, 7)
    )
  )

  expect_equal(
    slide(1:7, identity, .size = 3),
    list(
      NULL,
      NULL,
      c(1, 2, 3),
      c(2, 3, 4),
      c(3, 4, 5),
      c(4, 5, 6),
      c(5, 6, 7)
    )
  )
})

# ------------------------------------------------------------------------------
# .step

test_that("can step to skip over function calls", {
  expect_equal(
    slide(1:7, identity, .step = 2),
    list(
      1,
      NULL,
      3,
      NULL,
      5,
      NULL,
      7
    )
  )

  expect_equal(
    slide(1:7, identity, .step = 3),
    list(
      1,
      NULL,
      NULL,
      4,
      NULL,
      NULL,
      7
    )
  )
})

# ------------------------------------------------------------------------------
# .align

test_that("can align right", {
  expect_equal(
    slide(1:7, identity, .size = 4, .align = "right"),
    list(
      NULL,
      NULL,
      NULL,
      1:4,
      2:5,
      3:6,
      4:7
    )
  )
})

test_that("can align left", {
  expect_equal(
    slide(1:7, identity, .size = 4, .align = "left"),
    list(
      1:4,
      2:5,
      3:6,
      4:7,
      NULL,
      NULL,
      NULL
    )
  )
})

test_that("can align center-left with even .size", {
  expect_equal(
    slide(1:7, identity, .size = 4, .align = "center-left"),
    list(
      NULL,
      1:4,
      2:5,
      3:6,
      4:7,
      NULL,
      NULL
    )
  )
})

test_that("can align center-right with even .size", {
  expect_equal(
    slide(1:7, identity, .size = 4, .align = "center-right"),
    list(
      NULL,
      NULL,
      1:4,
      2:5,
      3:6,
      4:7,
      NULL
    )
  )
})

test_that("align center with even size falls back to center-left", {
  expect_equal(
    slide(1:7, identity, .size = 4, .align = "center"),
    slide(1:7, identity, .size = 4, .align = "center-left")
  )
})

test_that("align center-left with odd size is equivalent to align center", {
  expect_equal(
    slide(1:7, identity, .size = 3, .align = "center-left"),
    slide(1:7, identity, .size = 3, .align = "center")
  )
})

test_that("align center-right with odd size is equivalent to align center", {
  expect_equal(
    slide(1:7, identity, .size = 3, .align = "center-right"),
    slide(1:7, identity, .size = 3, .align = "center")
  )
})

# ------------------------------------------------------------------------------
# .partial

test_that(".partial doesn't change the result if not required", {
  expect_equal(
    slide(1:7, identity, .partial = TRUE),
    slide(1:7, identity)
  )

  expect_equal(
    slide(1:7, identity, .partial = TRUE, .step = 2L),
    slide(1:7, identity, .step = 2L)
  )
})

test_that(".partial is meaningful with align left + size > 1", {
  expect_equal(
    slide(1:5, identity, .partial = TRUE, .size = 2L, .align = "left"),
    list(
      1:2,
      2:3,
      3:4,
      4:5,
      5
    )
  )
})

test_that(".partial is meaningful with align center + size > 1", {
  expect_equal(
    slide(1:5, identity, .partial = TRUE, .size = 3L, .align = "center"),
    list(
      NULL,
      1:3,
      2:4,
      3:5,
      4:5
    )
  )
})

# ------------------------------------------------------------------------------
# .dir

test_that(".dir backward is equivalent to .dir forward if .size = 1", {
  expect_equal(
    slide(1:5, identity, .dir = "backward"),
    slide(1:5, identity)
  )

  expect_equal(
    slide(1:5, identity, .dir = "backward", .step = 2),
    slide(1:5, identity, .step = 2)
  )
})

test_that(".dir with only altered size is equivalent to reversed forward .dir", {
  expect_equal(
    slide(1:5, identity, .dir = "backward", .size = 2),
    lapply(slide(1:5, identity, .size = 2), rev)
  )

  expect_equal(
    slide(1:5, identity, .dir = "backward", .size = 3),
    lapply(slide(1:5, identity, .size = 3), rev)
  )
})

test_that(".dir doesn't reverse the idea of align", {
  expect_equal(
    slide(1:5, identity, .size = 2, .step = 2, .dir = "backward"),
    list(
      NULL,
      NULL,
      3:2,
      NULL,
      5:4
    )
  )

  expect_equal(
    slide(1:5, identity, .size = 2, .step = 2, .dir = "backward", .align = "left"),
    list(
      NULL,
      3:2,
      NULL,
      5:4,
      NULL
    )
  )
})

test_that(".dir backwards + .partial is meaningful with align right + size > 1", {
  expect_equal(
    slide(1:5, identity, .partial = TRUE, .size = 2L, .dir = "backward"),
    list(
      1,
      2:1,
      3:2,
      4:3,
      5:4
    )
  )
})

# ------------------------------------------------------------------------------
# data frames

test_that("slide() is a rowwise iterator", {
  x <- data.frame(x = 1:3, y = 2:4)

  expect_equal(
    slide(x, identity),
    list(
      vec_slice(x, 1),
      vec_slice(x, 2),
      vec_slice(x, 3)
    )
  )

  expect_equal(
    slide(x, identity, .size = 2L),
    list(
      NULL,
      vec_slice(x, 1:2),
      vec_slice(x, 2:3)
    )
  )
})

# ------------------------------------------------------------------------------
# complex combinations

test_that(".partial is only activated when an endpoint lands inside the output vector", {
  # here, partial is not active because the final step would place the endpoint at position -1
  expect_equal(
    slide(1:7, identity, .partial = TRUE, .size = 3L, .dir = "backward", .align = "center-left", .step = 2L),
    list(
      NULL,
      3:1,
      NULL,
      5:3,
      NULL,
      7:5,
      NULL
    )
  )

  # now partial is activated because the last point lands at 0
  expect_equal(
    slide(1:8, identity, .partial = TRUE, .size = 3L, .dir = "backward", .align = "center-left", .step = 2L),
    list(
      2:1,
      NULL,
      4:2,
      NULL,
      6:4,
      NULL,
      8:6,
      NULL
    )
  )
})

# ------------------------------------------------------------------------------
# validation

test_that("cannot use invalid .size", {
  expect_error(slide(1, identity, .size = -1), "at least 1, not -1")
  expect_error(slide(1, identity, .size = 0), "at least 1, not 0")

  expect_error(slide(1, identity, .size = c(1, 2)), class = "vctrs_error_assert_size")
  expect_error(slide(1, identity, .size = "x"), class = "vctrs_error_cast_lossy")
})

test_that("cannot use invalid .step", {
  expect_error(slide(1, identity, .step = -1), "at least 1, not -1")
  expect_error(slide(1, identity, .step = 0), "at least 1, not 0")

  expect_error(slide(1, identity, .step = c(1, 2)), class = "vctrs_error_assert_size")
  expect_error(slide(1, identity, .step = "x"), class = "vctrs_error_cast_lossy")
})

test_that("cannot use invalid .align", {
  expect_error(slide(1, identity, .align = "stuff"), "must be one of")
  expect_error(slide(1, identity, .align = "ri"), 'Did you mean "right"?')

  expect_error(slide(1, identity, .align = c("right", "left")), class = "vctrs_error_assert_size")

  expect_error(slide(1, identity, .align = 1), class = "vctrs_error_assert_ptype")
})

test_that("cannot use invalid .dir", {
  expect_error(slide(1, identity, .dir = "stuff"), "must be one of")
  expect_error(slide(1, identity, .dir = "for"), 'Did you mean "forward"?')

  expect_error(slide(1, identity, .dir = c("forward", "backward")), class = "vctrs_error_assert_size")

  expect_error(slide(1, identity, .dir = 1), class = "vctrs_error_assert_ptype")
})

test_that("cannot use invalid .partial", {
  expect_error(slide(1, identity, .partial = c(TRUE, TRUE)), class = "vctrs_error_assert_size")
  expect_error(slide(1, identity, .partial = 1), class = "vctrs_error_assert_ptype")
})

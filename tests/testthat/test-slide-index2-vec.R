# ------------------------------------------------------------------------------
# suffix tests

test_that("slide_index2_int() works", {
  expect_equal(slide_index2_int(1L, 1L, 1, ~.x), 1L)
})

test_that("slide_index2_int() can coerce", {
  expect_equal(slide_index2_int(1, 1, 1, ~.x), 1L)
})

test_that("slide_index2_dbl() works", {
  expect_equal(slide_index2_dbl(1, 1, 1, ~.x), 1)
})

test_that("slide_index2_dbl() can coerce", {
  expect_equal(slide_index2_dbl(1L, 1, 1, ~.x), 1)
})

test_that("slide_index2_chr() works", {
  expect_equal(slide_index2_chr("x", 1, 1, ~.x), "x")
})

test_that("slide_index2_chr() can coerce", {
  expect_equal(slide_index2_chr(1, 1, 1, ~.x), "1")
})

test_that("slide_index2_lgl() works", {
  expect_equal(slide_index2_lgl(TRUE, 1, 1, ~.x), TRUE)
})

test_that("slide_index2_lgl() can coerce", {
  expect_equal(slide_index2_lgl(1, 1, 1, ~.x), TRUE)
})

test_that("slide_index2_raw() works", {
  expect_equal(slide_index2_raw(raw(1), 1, 1, ~.x), raw(1))
})

# ------------------------------------------------------------------------------
# data frame suffix tests

test_that("slide_index2_dfr() works", {
  expect_equal(
    slide_index2_dfr(1:2, 1:2, 1:2, ~.x, .before = 1),
    slide_dfr(1:2, ~.x, .before = 1)
  )

  x <- 1:2
  expect_equal(
    slide_index2_dfr(x, x, 1:2, ~data.frame(x = .x), .before = 1),
    slide_dfr(x, ~data.frame(x = .x), .before = 1)
  )
})

test_that("slide_index2_dfc() works", {
  expect_equal(
    slide_index2_dfc(1:2, 1:2, 1:2, ~.x, .before = 1),
    slide_dfc(1:2, ~.x, .before = 1)
  )

  x <- 1:2
  expect_equal(
    slide_index2_dfc(x, x, 1:2, ~data.frame(x = .x), .before = 1),
    slide_dfc(x, ~data.frame(x = .x), .before = 1)
  )
})

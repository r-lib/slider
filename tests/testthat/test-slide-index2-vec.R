# ------------------------------------------------------------------------------
# slide_index2_*()

test_that("slide_index2_*() works", {
  expect_identical(slide_index2_vec(1L, 1L, 1, ~ .x + .y), 2L)
  expect_identical(slide_index2_int(1L, 1L, 1, ~ .x + .y), 2L)
})

test_that("slide_index2_*() retains names of x", {
  expect_identical(
    slide_index2_vec(c(x = 1L), c(y = 1L), 1, ~ .x + .y),
    c(x = 2L)
  )
  expect_identical(
    slide_index2_int(c(x = 1L), c(y = 1L), 1, ~ .x + .y),
    c(x = 2L)
  )
})

test_that("slide_index2_vec() can simplify automatically", {
  expect_identical(slide_index2_vec(1, 2, 1, ~ .x + .y, .ptype = NULL), 3)
})

test_that("slide_index2_*() errors if it can't simplify", {
  fn <- function(x, y) {
    if (x == 1L) {
      1
    } else {
      "hi"
    }
  }
  expect_snapshot({
    (expect_error(
      slide_index2_vec(1:2, 1:2, 1:2, fn, .ptype = NULL),
      class = "vctrs_error_incompatible_type"
    ))
  })
  expect_snapshot({
    (expect_error(
      slide_index2_int(1:2, 1:2, 1:2, fn),
      class = "vctrs_error_incompatible_type"
    ))
  })
})

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

test_that("slide_index2_chr() cannot coerce", {
  expect_snapshot({
    (expect_error(
      slide_index2_chr(1, 1, 1, ~.x),
      class = "vctrs_error_incompatible_type"
    ))
  })
})

test_that("slide_index2_lgl() works", {
  expect_equal(slide_index2_lgl(TRUE, 1, 1, ~.x), TRUE)
})

test_that("slide_index2_lgl() can coerce", {
  expect_equal(slide_index2_lgl(1, 1, 1, ~.x), TRUE)
})

# ------------------------------------------------------------------------------
# data frame suffix tests

test_that("slide_index2_dfr() works", {
  expect_identical(
    slide_index2_dfr(
      1:2,
      1:2,
      1:2,
      ~ new_data_frame(list(x = list(.x), y = list(.y))),
      .before = 1
    ),
    data_frame(
      x = list(1L, 1:2),
      y = list(1L, 1:2)
    )
  )
})

test_that("pslide_index_dfc() works", {
  x <- 1:2

  fn <- function(x, y) {
    if (length(x) == 1) {
      data.frame(x1 = x, y1 = y)
    } else {
      data.frame(x2 = x, y2 = y)
    }
  }

  expect_identical(
    slide_index2_dfc(
      1:2,
      1:2,
      1:2,
      fn,
      .before = 1
    ),
    data.frame(
      x1 = c(1L, 1L),
      y1 = c(1L, 1L),
      x2 = 1:2,
      y2 = 1:2
    )
  )
})

# ------------------------------------------------------------------------------
# .ptype

test_that("`.ptype = NULL` is size stable (#78)", {
  expect_length(
    slide_index2_vec(1:4, 1:4, 1:4, ~1, .before = 1, .complete = TRUE),
    4
  )
})

test_that("size 0 inputs returns .ptype", {
  expect_identical(
    slide_index2_vec(integer(), integer(), integer(), ~.x, .ptype = NULL),
    NULL
  )
  expect_identical(
    slide_index2_vec(integer(), integer(), integer(), ~.x, .ptype = double()),
    double()
  )
})

test_that("`slide_index2_vec()` falls back to `c()` method as required", {
  local_c_foobar()

  expect_identical(
    slide_index2_vec(1:3, 1:3, 1:3, ~ foobar(.x), .ptype = foobar(integer())),
    foobar(1:3)
  )
  expect_condition(
    slide_index2_vec(1:3, 1:3, 1:3, ~ foobar(.x), .ptype = foobar(integer())),
    class = "slider_c_foobar"
  )

  expect_identical(slide_index2_vec(1:3, 1:3, 1:3, ~ foobar(.x)), foobar(1:3))
  expect_condition(
    slide_index2_vec(1:3, 1:3, 1:3, ~ foobar(.x)),
    class = "slider_c_foobar"
  )
})

# ------------------------------------------------------------------------------
# .complete

test_that(".complete produces typed `NA` values", {
  expect_identical(
    slide_index2_int(1:3, 1:3, 1:3, ~1L, .before = 1, .complete = TRUE),
    c(NA, 1L, 1L)
  )
  expect_identical(
    slide_index2_dbl(1:3, 1:3, 1:3, ~1, .before = 1, .complete = TRUE),
    c(NA, 1, 1)
  )
  expect_identical(
    slide_index2_chr(1:3, 1:3, 1:3, ~"1", .before = 1, .complete = TRUE),
    c(NA, "1", "1")
  )
  expect_identical(
    slide_index2_vec(1:3, 1:3, 1:3, ~1, .before = 1, .complete = TRUE),
    c(NA, 1, 1)
  )
  expect_identical(
    slide_index2_vec(
      1:3,
      1:3,
      1:3,
      ~1,
      .before = 1,
      .complete = TRUE,
      .ptype = integer()
    ),
    c(NA, 1L, 1L)
  )
})

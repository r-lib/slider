# ------------------------------------------------------------------------------
# pslide_*()

test_that("pslide_*() works", {
  expect_identical(pslide_vec(list(1L, 1L), ~ .x + .y), 2L)
  expect_identical(pslide_int(list(1L, 1L), ~ .x + .y), 2L)
})

test_that("pslide_*() retains names of first input", {
  expect_identical(pslide_vec(list(c(x = 1L), c(y = 1L)), ~ .x + .y), c(x = 2L))
  expect_identical(pslide_int(list(c(x = 1L), c(y = 1L)), ~ .x + .y), c(x = 2L))
})

test_that("pslide_vec() can simplify automatically", {
  expect_identical(pslide_vec(list(1, 2), ~ .x + .y, .ptype = NULL), 3)
})

test_that("pslide_vec() errors if it can't simplify", {
  fn <- function(x, y) {
    if (x == 1L) {
      1
    } else {
      "hi"
    }
  }
  expect_snapshot({
    (expect_error(
      pslide_vec(list(1:2, 1:2), fn, .ptype = NULL),
      class = "vctrs_error_incompatible_type"
    ))
  })
})

test_that("pslide_*() errors if it can't cast", {
  fn <- function(x, y) {
    if (x == 1L) {
      1
    } else {
      "hi"
    }
  }
  expect_snapshot({
    (expect_error(
      pslide_int(list(1:2, 1:2), fn),
      class = "vctrs_error_incompatible_type"
    ))
  })
})

# ------------------------------------------------------------------------------
# suffix tests

test_that("pslide_int() works", {
  expect_identical(pslide_int(list(1L, 1L), ~ .x + .y), 2L)
})

test_that("pslide_int() can coerce", {
  expect_identical(pslide_int(list(1, 1), ~ .x + .y), 2L)
})

test_that("pslide_dbl() works", {
  expect_identical(pslide_dbl(list(1, 1), ~.x), 1)
})

test_that("pslide_dbl() can coerce", {
  expect_identical(pslide_dbl(list(1L, 1L), ~ .x + .y), 2)
})

test_that("pslide_chr() works", {
  expect_identical(pslide_chr(list("x", 1), ~.x), "x")
})

test_that("pslide_chr() cannot coerce", {
  expect_snapshot({
    (expect_error(
      pslide_chr(list(1, 1), ~ .x + .y),
      class = "vctrs_error_incompatible_type"
    ))
  })
})

test_that("pslide_lgl() works", {
  expect_identical(pslide_lgl(list(TRUE, 1), ~.x), TRUE)
})

test_that("pslide_lgl() can coerce", {
  expect_identical(pslide_lgl(list(1, 0), ~ .x + .y), TRUE)
})

# ------------------------------------------------------------------------------
# data frame suffix tests

test_that("pslide_dfr() works", {
  expect_identical(
    pslide_dfr(
      list(1:2, 1:2),
      ~ new_data_frame(list(x = list(.x), y = list(.y))),
      .before = 1
    ),
    data_frame(
      x = list(1L, 1:2),
      y = list(1L, 1:2)
    )
  )
})

test_that("pslide_dfc() works", {
  x <- 1:2

  fn <- function(x, y) {
    if (length(x) == 1) {
      data.frame(x1 = x, y1 = y)
    } else {
      data.frame(x2 = x, y2 = y)
    }
  }

  expect_identical(
    pslide_dfc(list(x, x), fn, .before = 1),
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
  expect_length(pslide_vec(list(1:4, 1:4), ~.x, .step = 2), 4)
  expect_length(
    pslide_vec(list(1:4, 1:4), ~1, .before = 1, .complete = TRUE),
    4
  )
})

test_that("`pslide_vec()` falls back to `c()` method as required", {
  local_c_foobar()

  expect_identical(
    pslide_vec(list(1:3, 1:3), ~ foobar(.x), .ptype = foobar(integer())),
    foobar(1:3)
  )
  expect_condition(
    pslide_vec(list(1:3, 1:3), ~ foobar(.x), .ptype = foobar(integer())),
    class = "slider_c_foobar"
  )

  expect_identical(pslide_vec(list(1:3, 1:3), ~ foobar(.x)), foobar(1:3))
  expect_condition(
    pslide_vec(list(1:3, 1:3), ~ foobar(.x)),
    class = "slider_c_foobar"
  )
})

# ------------------------------------------------------------------------------
# .step

test_that(".step produces typed `NA` values", {
  expect_identical(pslide_int(list(1:3, 1:3), ~.x, .step = 2), c(1L, NA, 3L))
  expect_identical(pslide_dbl(list(1:3, 1:3), ~.x, .step = 2), c(1, NA, 3))
  expect_identical(
    pslide_chr(list(c("a", "b", "c"), 1:3), ~.x, .step = 2),
    c("a", NA, "c")
  )
  expect_identical(pslide_vec(list(1:3, 1:3), ~.x, .step = 2), c(1L, NA, 3L))
  expect_identical(
    pslide_vec(list(1:3, 1:3), ~.x, .step = 2, .ptype = integer()),
    c(1L, NA, 3L)
  )
})

# ------------------------------------------------------------------------------
# .complete

test_that(".complete produces typed `NA` values", {
  expect_identical(
    pslide_int(list(1:3, 1:3), ~1L, .before = 1, .complete = TRUE),
    c(NA, 1L, 1L)
  )
  expect_identical(
    pslide_dbl(list(1:3, 1:3), ~1, .before = 1, .complete = TRUE),
    c(NA, 1, 1)
  )
  expect_identical(
    pslide_chr(list(1:3, 1:3), ~"1", .before = 1, .complete = TRUE),
    c(NA, "1", "1")
  )
  expect_identical(
    pslide_vec(list(1:3, 1:3), ~1, .before = 1, .complete = TRUE),
    c(NA, 1, 1)
  )
  expect_identical(
    pslide_vec(
      list(1:3, 1:3),
      ~1,
      .before = 1,
      .complete = TRUE,
      .ptype = integer()
    ),
    c(NA, 1L, 1L)
  )
})

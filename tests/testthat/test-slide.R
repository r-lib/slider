test_that("default settings is the same as map()", {
  expect_equal(slide(1:5, identity), as.list(1:5))
})

# ------------------------------------------------------------------------------
# .before / .after

test_that("can use .before for right alignment", {
  expect_equal(
    slide(1:7, identity, .before = 1),
    list(
      NULL,
      1:2,
      2:3,
      3:4,
      4:5,
      5:6,
      6:7
    )
  )

  expect_equal(
    slide(1:7, identity, .before = 2),
    list(
      NULL,
      NULL,
      1:3,
      2:4,
      3:5,
      4:6,
      5:7
    )
  )
})

test_that("can use .after for left alignment", {
  expect_equal(
    slide(1:7, identity, .after = 1),
    list(
      1:2,
      2:3,
      3:4,
      4:5,
      5:6,
      6:7,
      NULL
    )
  )

  expect_equal(
    slide(1:7, identity, .after = 2),
    list(
      1:3,
      2:4,
      3:5,
      4:6,
      5:7,
      NULL,
      NULL
    )
  )
})

test_that("can use .before / .after for center alignment", {
  expect_equal(
    slide(1:7, identity, .before = 1, .after = 1),
    list(
      NULL,
      1:3,
      2:4,
      3:5,
      4:6,
      5:7,
      NULL
    )
  )

  expect_equal(
    slide(1:7, identity, .before = 2, .after = 2),
    list(
      NULL,
      NULL,
      1:5,
      2:6,
      3:7,
      NULL,
      NULL
    )
  )
})

test_that("can use .before / .after for center-left alignment", {
  expect_equal(
    slide(1:7, identity, .before = 2, .after = 1),
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

test_that("can use .before / .after for center-right alignment", {
  expect_equal(
    slide(1:7, identity, .before = 1, .after = 2),
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

# ------------------------------------------------------------------------------
# negative before

test_that("can use a negative before to 'look forward'", {
  expect_equal(
    slide(1:5, identity, .before = -1, .after = 1),
    list(
      2L,
      3L,
      4L,
      5L,
      NULL
    )
  )

  expect_equal(
    slide(1:5, identity, .before = -1, .after = unbounded()),
    list(
      2:5,
      3:5,
      4:5,
      5L,
      NULL
    )
  )
})

test_that("can use negative before with offset", {
  expect_equal(
    slide(1:5, identity, .before = -1, .after = 1, .offset = 1),
    list(
      NULL,
      3L,
      4L,
      5L,
      NULL
    )
  )
})

test_that("error if negative .before's abs() is > .after", {
  expect_error(slide(1:5, identity, .before = -1), "cannot be greater than `.after`.")
})

test_that("both .before and .after cannot be negative", {
  expect_error(slide(1:5, identity, .before = -1, .after = -1), "cannot both be negative.")
})

# ------------------------------------------------------------------------------
# negative after

test_that("can use a negative .after to 'look backward'", {
  expect_equal(
    slide(1:5, identity, .before = 1, .after = -1),
    list(
      NULL,
      1L,
      2L,
      3L,
      4L
    )
  )

  expect_equal(
    slide(1:5, identity, .before = unbounded(), .after = -1),
    list(
      NULL,
      1L,
      1:2,
      1:3,
      1:4
    )
  )
})

test_that("error if negative .after's abs() is > .before", {
  expect_error(slide(1:5, identity, .after = -1), "cannot be greater than `.before`.")
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

test_that(".partial works when the size shrinks over the last iterations", {
  expect_equal(
    slide(1:7, identity, .partial = TRUE, .after = 2L),
    list(
      1:3,
      2:4,
      3:5,
      4:6,
      5:7,
      6:7,
      7
    )
  )
})

test_that(".partial works when doing center alignment", {
  expect_equal(
    slide(1:5, identity, .partial = TRUE, .before = 1, .after = 1),
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
    slide(1:5, identity, .dir = "backward", .before = 1),
    lapply(slide(1:5, identity, .before = 1), rev)
  )

  expect_equal(
    slide(1:5, identity, .dir = "backward", .before = 1, .after = 2),
    lapply(slide(1:5, identity, .before = 1, .after = 2), rev)
  )
})

test_that(".dir keeps intuitive alignment with .before / .after", {
  expect_equal(
    slide(1:5, identity, .before = 1, .step = 2, .dir = "backward"),
    list(
      NULL,
      NULL,
      3:2,
      NULL,
      5:4
    )
  )

  expect_equal(
    slide(1:5, identity, .after = 1, .step = 2, .dir = "backward"),
    list(
      NULL,
      3:2,
      NULL,
      5:4,
      NULL
    )
  )
})

test_that(".dir backwards + .partial is meaningful with `.before > 0`", {
  expect_equal(
    slide(1:5, identity, .partial = TRUE, .before = 2L, .dir = "backward"),
    list(
      1,
      2:1,
      3:1,
      4:2,
      5:3
    )
  )
})

# ------------------------------------------------------------------------------
# .offset

test_that(".offset shifts starting point", {
  expect_equal(
    slide(1:5, identity, .offset = 1),
    list(
      NULL,
      2,
      3,
      4,
      5
    )
  )

  expect_equal(
    slide(1:5, identity, .offset = 4),
    list(
      NULL,
      NULL,
      NULL,
      NULL,
      5
    )
  )

  expect_equal(
    slide(1:5, identity, .offset = 1, .dir = "backward"),
    list(
      1,
      2,
      3,
      4,
      NULL
    )
  )

  expect_equal(
    slide(1:5, identity, .offset = 4, .dir = "backward"),
    list(
      1,
      NULL,
      NULL,
      NULL,
      NULL
    )
  )
})

test_that(".offset with .before/.after cannot imply a location larger than the size of .x", {
  expect_error(
    slide(1:5, identity, .offset = 5),
    "`.offset` and `.after` imply a location [(]6[)] outside the size of `.x` [(]5[)]"
  )

  expect_error(
    slide(1:5, identity, .offset = 2, .after = 3),
    "`.offset` and `.after` imply a location [(]6[)] outside the size of `.x` [(]5[)]"
  )

  expect_error(
    slide(1:5, identity, .offset = 5, .dir = "backward"),
    "`.offset` and `.before` imply a location [(]6[)] outside the size of `.x` [(]5[)]"
  )

  expect_error(
    slide(1:5, identity, .offset = 2, .before = 3, .dir = "backward"),
    "`.offset` and `.before` imply a location [(]6[)] outside the size of `.x` [(]5[)]"
  )
})

test_that(".offset must be at least .before/.after", {
  expect_error(
    slide(1:5, identity, .offset = 3, .before = 4),
    "`.offset` [(]3[)] must be at least as large as `.before` [(]4[)]"
  )

  expect_error(
    slide(1:5, identity, .offset = 3, .after = 4, .dir = "backward"),
    "`.offset` [(]3[)] must be at least as large as `.after` [(]4[)]"
  )
})

# ------------------------------------------------------------------------------
# unbounded()

test_that("can use unbounded() in .before for cumulative sliding", {
  expect_equal(
    slide(1:5, identity, .before = unbounded()),
    list(
      1L,
      1:2,
      1:3,
      1:4,
      1:5
    )
  )
})

test_that("can use unbounded() in .before + set .after", {
  expect_equal(
    slide(1:5, identity, .before = unbounded(), .after = 1L),
    list(
      1:2,
      1:3,
      1:4,
      1:5,
      NULL
    )
  )

  expect_equal(
    slide(1:5, identity, .before = unbounded(), .after = -1L),
    list(
      NULL,
      1L,
      1:2,
      1:3,
      1:4
    )
  )
})

test_that("can use unbounded() in .after for cumulative sliding", {
  expect_equal(
    slide(1:5, identity, .after = unbounded()),
    list(
      1:5,
      2:5,
      3:5,
      4:5,
      5L
    )
  )
})

test_that("can use unbounded() in .after + set .before", {
  expect_equal(
    slide(1:5, identity, .after = unbounded(), .before = 1L),
    list(
      NULL,
      1:5,
      2:5,
      3:5,
      4:5
    )
  )

  expect_equal(
    slide(1:5, identity, .after = unbounded(), .before = -1L),
    list(
      2:5,
      3:5,
      4:5,
      5L,
      NULL
    )
  )
})

test_that("can use unbounded() when going backwards", {
  expect_equal(
    slide(1:5, identity, .before = unbounded(), .dir = "backward"),
    list(
      1L,
      2:1,
      3:1,
      4:1,
      5:1
    )
  )

  expect_equal(
    slide(1:5, identity, .after = unbounded(), .dir = "backward"),
    list(
      5:1,
      5:2,
      5:3,
      5:4,
      5L
    )
  )

  expect_equal(
    slide(1:5, identity, .before = unbounded(), .after = 1L, .dir = "backward"),
    list(
      2:1,
      3:1,
      4:1,
      5:1,
      NULL
    )
  )

  expect_equal(
    slide(1:5, identity, .after = unbounded(), .before = 1L, .dir = "backward"),
    list(
      NULL,
      5:1,
      5:2,
      5:3,
      5:4
    )
  )
})

test_that("can use unbounded() with .offset", {
  expect_equal(
    slide(1:5, identity, .after = unbounded(), .offset = 1L),
    list(
      NULL,
      2:5,
      3:5,
      4:5,
      5L
    )
  )

  expect_equal(
    slide(1:5, identity, .before = unbounded(), .offset = 1L),
    list(
      NULL,
      1:2,
      1:3,
      1:4,
      1:5
    )
  )

  expect_equal(
    slide(1:5, identity, .before = unbounded(), .dir = "backward", .offset = 1L),
    list(
      1L,
      2:1,
      3:1,
      4:1,
      NULL
    )
  )

  expect_equal(
    slide(1:5, identity, .after = unbounded(), .dir = "backward", .offset = 1L),
    list(
      5:1,
      5:2,
      5:3,
      5:4,
      NULL
    )
  )
})

test_that("can be doubly unbounded()", {
  expect_equal(
    slide(1:5, identity, .before = unbounded(), .after = unbounded()),
    list(
      1:5,
      1:5,
      1:5,
      1:5,
      1:5
    )
  )

  expect_equal(
    slide(1:5, identity, .before = unbounded(), .after = unbounded(), .dir = "backward"),
    list(
      5:1,
      5:1,
      5:1,
      5:1,
      5:1
    )
  )

  expect_equal(
    slide(1:5, identity, .before = unbounded(), .after = unbounded(), .offset = 1L),
    list(
      NULL,
      1:5,
      1:5,
      1:5,
      1:5
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
    slide(x, identity, .before = 1L),
    list(
      NULL,
      vec_slice(x, 1:2),
      vec_slice(x, 2:3)
    )
  )
})

# ------------------------------------------------------------------------------
# .ptype

test_that(".ptype is respected", {
  expect_equal(slide(1, ~.x), list(1))
  expect_equal(slide(1, ~.x, .ptype = int()), 1L)
  expect_error(slide(1, ~.x, .ptype = new_date()), class = "vctrs_error_incompatible_type")
})

test_that("`.ptype = NULL` results in 'guessed' .ptype", {
  expect_equal(
    slide(1, ~.x, .ptype = NULL),
    slide(1, ~.x, .ptype = dbl())
  )

  # failure = list()
  expect_equal(
    slide(1:2, ~ifelse(.x == 1L, "hello", 1), .ptype = NULL),
    list("hello", 1)
  )
})

test_that(".ptypes with a vec_proxy() are restored to original type", {
  expect_is(
    slide(Sys.Date() + 1:5, ~.x, .ptype = as.POSIXlt(Sys.Date())),
    "POSIXlt"
  )
})

# ------------------------------------------------------------------------------
# complex combinations

test_that(".partial is only activated when an endpoint lands inside the output vector", {
  # here, partial is not active because the final step would place the endpoint at position -1
  expect_equal(
    slide(1:7, identity, .partial = TRUE, .before = 1L, .after = 1L, .dir = "backward", .step = 2L),
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
    slide(1:8, identity, .partial = TRUE, .before = 1L, .after = 1L, .dir = "backward", .step = 2L),
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

test_that("cannot use invalid .before", {
  expect_error(slide(1, identity, .before = c(1, 2)), class = "vctrs_error_assert_size")
  expect_error(slide(1, identity, .before = "x"), class = "vctrs_error_cast_lossy")
})

test_that("cannot use invalid .after", {
  expect_error(slide(1, identity, .after = c(1, 2)), class = "vctrs_error_assert_size")
  expect_error(slide(1, identity, .after = "x"), class = "vctrs_error_cast_lossy")
})

test_that("cannot use invalid .offset", {
  expect_error(slide(1, identity, .offset = -1), "at least 0, not -1")

  expect_error(slide(1, identity, .offset = c(1, 2)), class = "vctrs_error_assert_size")
  expect_error(slide(1, identity, .offset = "x"), class = "vctrs_error_cast_lossy")
})

test_that("cannot use invalid .step", {
  expect_error(slide(1, identity, .step = -1), "at least 1, not -1")
  expect_error(slide(1, identity, .step = 0), "at least 1, not 0")

  expect_error(slide(1, identity, .step = c(1, 2)), class = "vctrs_error_assert_size")
  expect_error(slide(1, identity, .step = "x"), class = "vctrs_error_cast_lossy")
})

test_that("cannot use invalid .dir", {
  expect_error(slide(1, identity, .dir = "stuff"), "must be one of")
  expect_error(slide(1, identity, .dir = "for"), 'Did you mean "forward"?')

  expect_error(slide(1, identity, .dir = c("forward", "backward")), class = "vctrs_error_assert_size")

  expect_error(slide(1, identity, .dir = 1), "must be a character vector", class = "rlang_error")
})

test_that("cannot use invalid .partial", {
  expect_error(slide(1, identity, .partial = c(TRUE, TRUE)), class = "vctrs_error_assert_size")
  expect_error(slide(1, identity, .partial = 1), class = "vctrs_error_assert_ptype")
})

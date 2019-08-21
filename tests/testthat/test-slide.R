test_that("default settings is the same as map()", {
  expect_equal(slide(1:5, identity), as.list(1:5))
})

# ------------------------------------------------------------------------------
# .before / .after

test_that("can use .before for right alignment", {
  expect_equal(
    slide(1:7, identity, .before = 1),
    list(
      1L,
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
      1L,
      1:2,
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
      7L
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
      6:7,
      7L
    )
  )
})

test_that("can use .before / .after for center alignment", {
  expect_equal(
    slide(1:7, identity, .before = 1, .after = 1),
    list(
      1:2,
      1:3,
      2:4,
      3:5,
      4:6,
      5:7,
      6:7
    )
  )

  expect_equal(
    slide(1:7, identity, .before = 2, .after = 2),
    list(
      1:3,
      1:4,
      1:5,
      2:6,
      3:7,
      4:7,
      5:7
    )
  )
})

test_that("can use .before / .after for center-left alignment", {
  expect_equal(
    slide(1:7, identity, .before = 2, .after = 1),
    list(
      1:2,
      1:3,
      1:4,
      2:5,
      3:6,
      4:7,
      5:7
    )
  )
})

test_that("can use .before / .after for center-right alignment", {
  expect_equal(
    slide(1:7, identity, .before = 1, .after = 2),
    list(
      1:3,
      1:4,
      2:5,
      3:6,
      4:7,
      5:7,
      6:7
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

  expect_equal(
    slide(1:6, identity, .before = 1, .step = 2),
    list(
      1,
      NULL,
      2:3,
      NULL,
      4:5,
      NULL
    )
  )
})

# ------------------------------------------------------------------------------
# .complete

test_that(".complete doesn't change the result if not required", {
  expect_equal(
    slide(1:7, identity, .complete = TRUE),
    slide(1:7, identity)
  )

  expect_equal(
    slide(1:7, identity, .complete = TRUE, .step = 2L),
    slide(1:7, identity, .step = 2L)
  )
})

test_that(".complete works when the size shrinks over the last iterations", {
  expect_equal(
    slide(1:7, identity, .complete = TRUE, .after = 2L),
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

test_that(".complete works when doing center alignment", {
  expect_equal(
    slide(1:5, identity, .complete = TRUE, .before = 1, .after = 1),
    list(
      NULL,
      1:3,
      2:4,
      3:5,
      NULL
    )
  )
})

# TODO - fix failing test
test_that(".complete works with negative .before", {
  expect_error(
    slide(1:5, ~.x, .before = -1, .after = 2, .complete = TRUE)
  )

  # expect_equal(
  #   slide(1:5, ~.x, .before = -1, .after = 2, .complete = TRUE),
  #   list(
  #     2L,
  #     3L,
  #     4L,
  #     5L,
  #     NULL
  #   )
  # )
})

# ------------------------------------------------------------------------------
# .forward

test_that("`.forward == FALSE` is equivalent to `.forward == TRUE` if `.size == 1`", {
  expect_equal(
    slide(1:5, identity, .forward = FALSE),
    slide(1:5, identity)
  )

  expect_equal(
    slide(1:5, identity, .forward = FALSE, .step = 2),
    slide(1:5, identity, .step = 2)
  )
})

test_that("`.forward == FALSE` with only altered size is equivalent to reversed `.forward == TRUE`", {
  expect_equal(
    slide(1:5, identity, .forward = FALSE, .before = 1),
    lapply(slide(1:5, identity, .before = 1), rev)
  )

  expect_equal(
    slide(1:5, identity, .forward = FALSE, .before = 1, .after = 2),
    lapply(slide(1:5, identity, .before = 1, .after = 2), rev)
  )
})

test_that("backwards sliding keeps intuitive alignment with .before / .after", {
  expect_equal(
    slide(1:5, identity, .before = 1, .step = 2, .forward = FALSE),
    list(
      1L,
      NULL,
      3:2,
      NULL,
      5:4
    )
  )

  expect_equal(
    slide(1:5, identity, .after = 1, .step = 2, .forward = FALSE),
    list(
      2:1,
      NULL,
      4:3,
      NULL,
      5L
    )
  )
})

test_that("backwards + .complete is meaningful with `.before > 0`", {
  expect_equal(
    slide(1:5, identity, .complete = TRUE, .before = 2L, .forward = FALSE),
    list(
      NULL,
      NULL,
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
    slide(1:5, identity, .offset = 1, .forward = FALSE),
    list(
      1,
      2,
      3,
      4,
      NULL
    )
  )

  expect_equal(
    slide(1:5, identity, .offset = 4, .forward = FALSE),
    list(
      1,
      NULL,
      NULL,
      NULL,
      NULL
    )
  )
})

test_that(".offset can be smaller than .before/.after", {
  expect_equal(
    slide(1:5, identity, .offset = 3, .before = 4),
    list(
      NULL,
      NULL,
      NULL,
      1:4,
      1:5
    )
  )

  expect_equal(
    slide(1:5, identity, .offset = 3, .after = 4, .forward = FALSE),
    list(
      5:1,
      5:2,
      NULL,
      NULL,
      NULL
    )
  )

  expect_equal(
    slide(1:5, identity, .offset = 1, .before = 3, .after = 3),
    list(
      NULL,
      1:5,
      1:5,
      1:5,
      2:5
    )
  )
})

test_that(".offset can be smaller in magnitude than .after and is adjusted automatically", {
  expect_equal(
    slide(1:5, ~.x, .before = 3, .after = -2, .offset = 1),
    list(
      NULL,
      NULL,
      1L,
      1:2,
      2:3
    )
  )
})

test_that(".offset can be smaller in magnitude than .before and is adjusted automatically", {
  expect_equal(
    slide(1:5, ~.x, .after = 3, .before = -2, .offset = 1, .forward = FALSE),
    list(
      4:3,
      5:4,
      5L,
      NULL,
      NULL
    )
  )
})

test_that(".offset is allowed to be completely outside of .x", {
  expect_equal(
    slide(1:5, ~.x, .offset = 5),
    list(
      NULL,
      NULL,
      NULL,
      NULL,
      NULL
    )
  )

  expect_equal(
    slide(1:5, identity, .offset = 5, .forward = FALSE),
    list(
      NULL,
      NULL,
      NULL,
      NULL,
      NULL
    )
  )
})

test_that(".offset + .before + .complete can result in all NULLs", {
  expect_equal(
    slide(1:5, ~.x, .before = 5, .offset = 3, .complete = TRUE),
    list(
      NULL,
      NULL,
      NULL,
      NULL,
      NULL
    )
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
      1:5
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
      1:5,
      1:5,
      2:5,
      3:5,
      4:5
    )
  )

  expect_equal(
    slide(1:5, identity, .after = unbounded(), .before = 1L, .complete = TRUE),
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
    slide(1:5, identity, .before = unbounded(), .forward = FALSE),
    list(
      1L,
      2:1,
      3:1,
      4:1,
      5:1
    )
  )

  expect_equal(
    slide(1:5, identity, .after = unbounded(), .forward = FALSE),
    list(
      5:1,
      5:2,
      5:3,
      5:4,
      5L
    )
  )

  expect_equal(
    slide(1:5, identity, .before = unbounded(), .after = 1L, .forward = FALSE),
    list(
      2:1,
      3:1,
      4:1,
      5:1,
      5:1
    )
  )

  expect_equal(
    slide(1:5, identity, .before = unbounded(), .after = 1L, .forward = FALSE, .complete = TRUE),
    list(
      2:1,
      3:1,
      4:1,
      5:1,
      NULL
    )
  )

  expect_equal(
    slide(1:5, identity, .after = unbounded(), .before = 1L, .forward = FALSE),
    list(
      5:1,
      5:1,
      5:2,
      5:3,
      5:4
    )
  )

  expect_equal(
    slide(1:5, identity, .after = unbounded(), .before = 1L, .forward = FALSE, .complete = TRUE),
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
    slide(1:5, identity, .before = unbounded(), .forward = FALSE, .offset = 1L),
    list(
      1L,
      2:1,
      3:1,
      4:1,
      NULL
    )
  )

  expect_equal(
    slide(1:5, identity, .after = unbounded(), .forward = FALSE, .offset = 1L),
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
    slide(1:5, identity, .before = unbounded(), .after = unbounded(), .forward = FALSE),
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

  expect_equal(
    slide(1:5, identity, .before = unbounded(), .after = unbounded(), .complete = TRUE),
    list(
      1:5,
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
      vec_slice(x, 1L),
      vec_slice(x, 1:2),
      vec_slice(x, 2:3)
    )
  )

  expect_equal(
    slide(x, identity, .before = 1L, .complete = TRUE),
    list(
      NULL,
      vec_slice(x, 1:2),
      vec_slice(x, 2:3)
    )
  )
})

# ------------------------------------------------------------------------------
# type / size relaxed-ness

test_that("slide() doesn't require `size = 1`", {
  expect_equal(
    slide(1:2, ~c(.x, 1)),
    list(
      c(1L, 1L),
      c(2L, 1L)
    )
  )
})

test_that("`slide()` doesn't require a common inner type", {
  expect_equal(
    slide(1:2, ~if (.x == 1L) {1} else {"hi"}),
    list(1, "hi")
  )
})

# ------------------------------------------------------------------------------
# input names

test_that("input names are retained with atomics", {
  names <- letters[1:5]
  x <- set_names(1:5, names)
  expect_equal(names(slide(x, ~.x)), names)
})

test_that("input names are retained from proxied objects", {
  names <- letters[1:5]
  x <- as.POSIXlt(new_datetime(0:4 + 0))
  x <- set_names(x, names)
  expect_equal(names(slide(x, ~.x)), names)
})

test_that("row names are not extracted from data frames", {
  x <- data.frame(x = 1:5, row.names = letters[1:5])
  expect_equal(names(slide(x, ~.x)), NULL)
})

test_that("row names are extracted from arrays", {
  x <- array(1:4, c(2, 2), dimnames = list(c("r1", "r2"), c("c1", "c2")))
  expect_equal(names(slide(x, ~.x)), c("r1", "r2"))
})

test_that("names are retained on inner sliced object", {
  names <- letters[1:5]
  x <- set_names(1:5, names)
  exp <- set_names(as.list(names), names)
  expect_equal(slide(x, ~names(.x)), exp)

  x <- data.frame(x = 1:5, row.names = letters[1:5])
  expect_equal(slide(x, ~rownames(.x)), as.list(rownames(x)))

  names <- c("r1", "r2")
  x <- array(1:4, c(2, 2), dimnames = list(names, c("c1", "c2")))
  exp <- set_names(as.list(names), names)
  expect_equal(slide(x, ~rownames(.x)), exp)
})

# ------------------------------------------------------------------------------
# validation

test_that("cannot use invalid .before", {
  expect_error(slide(1, identity, .before = c(1, 2)), regexp = "1, not 2")
  expect_error(slide(1, identity, .before = "x"), class = "vctrs_error_cast_lossy")
})

test_that("cannot use invalid .after", {
  expect_error(slide(1, identity, .after = c(1, 2)), regexp = "1, not 2")
  expect_error(slide(1, identity, .after = "x"), class = "vctrs_error_cast_lossy")
})

test_that("cannot use invalid .offset", {
  expect_error(slide(1, identity, .offset = c(1, 2)), regexp = "1, not 2")
  expect_error(slide(1, identity, .offset = "x"), class = "vctrs_error_cast_lossy")
})

test_that("cannot use invalid .step", {
  expect_error(slide(1, identity, .step = -1), "at least 1, not -1")
  expect_error(slide(1, identity, .step = 0), "at least 1, not 0")

  expect_error(slide(1, identity, .step = c(1, 2)), regexp = "1, not 2")
  expect_error(slide(1, identity, .step = "x"), class = "vctrs_error_cast_lossy")
})

test_that("cannot use invalid .forward", {
  expect_error(slide(1, identity, .forward = c(TRUE, TRUE)), regexp = "1, not 2")
  expect_error(slide(1, identity, .forward = "hi"), class = "vctrs_error_cast_lossy")
})

test_that("cannot use invalid .complete", {
  expect_error(slide(1, identity, .complete = c(TRUE, TRUE)), regexp = "1, not 2")
  expect_error(slide(1, identity, .complete = "hi"), class = "vctrs_error_cast_lossy")
})


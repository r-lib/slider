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
      integer()
    )
  )

  expect_equal(
    slide(1:5, identity, .before = -1, .after = Inf),
    list(
      2:5,
      3:5,
      4:5,
      5L,
      integer()
    )
  )
})

test_that("error if negative .before's abs() is > .after", {
  expect_snapshot(error = TRUE, {
    slide(1:5, identity, .before = -1)
  })
})

test_that("both .before and .after cannot be negative", {
  expect_snapshot(error = TRUE, {
    slide(1:5, identity, .before = -1, .after = -1)
  })
})

# ------------------------------------------------------------------------------
# negative after

test_that("can use a negative .after to 'look backward'", {
  expect_equal(
    slide(1:5, identity, .before = 1, .after = -1),
    list(
      integer(),
      1L,
      2L,
      3L,
      4L
    )
  )

  expect_equal(
    slide(1:5, identity, .before = Inf, .after = -1),
    list(
      integer(),
      1L,
      1:2,
      1:3,
      1:4
    )
  )
})

test_that("error if negative .after's abs() is > .before", {
  expect_snapshot(error = TRUE, slide(1:5, identity, .after = -1))
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

test_that(".complete works with negative .before", {
  expect_equal(
    slide(1:5, ~.x, .before = -1, .after = 2, .complete = TRUE),
    list(
      2:3,
      3:4,
      4:5,
      NULL,
      NULL
    )
  )
})

# ------------------------------------------------------------------------------
# unbounded

test_that("can use Inf in .before for cumulative sliding", {
  expect_equal(
    slide(1:5, identity, .before = Inf),
    list(
      1L,
      1:2,
      1:3,
      1:4,
      1:5
    )
  )
})

test_that("can use Inf in .before + set .after", {
  expect_equal(
    slide(1:5, identity, .before = Inf, .after = 1L),
    list(
      1:2,
      1:3,
      1:4,
      1:5,
      1:5
    )
  )

  expect_equal(
    slide(1:5, identity, .before = Inf, .after = -1L),
    list(
      integer(),
      1L,
      1:2,
      1:3,
      1:4
    )
  )
})

test_that("can use Inf in .after for cumulative sliding", {
  expect_equal(
    slide(1:5, identity, .after = Inf),
    list(
      1:5,
      2:5,
      3:5,
      4:5,
      5L
    )
  )
})

test_that("can use Inf in .after + set .before", {
  expect_equal(
    slide(1:5, identity, .after = Inf, .before = 1L),
    list(
      1:5,
      1:5,
      2:5,
      3:5,
      4:5
    )
  )

  expect_equal(
    slide(1:5, identity, .after = Inf, .before = 1L, .complete = TRUE),
    list(
      NULL,
      1:5,
      2:5,
      3:5,
      4:5
    )
  )

  expect_equal(
    slide(1:5, identity, .after = Inf, .before = -1L),
    list(
      2:5,
      3:5,
      4:5,
      5L,
      integer()
    )
  )
})

test_that("can be doubly unbounded", {
  expect_equal(
    slide(1:5, identity, .before = Inf, .after = Inf),
    list(
      1:5,
      1:5,
      1:5,
      1:5,
      1:5
    )
  )

  expect_equal(
    slide(1:5, identity, .before = Inf, .after = Inf, .complete = TRUE),
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
    slide(1:2, ~ c(.x, 1)),
    list(
      c(1L, 1L),
      c(2L, 1L)
    )
  )
})

test_that("`slide()` doesn't require a common inner type", {
  expect_equal(
    slide(
      1:2,
      ~ if (.x == 1L) {
        1
      } else {
        "hi"
      }
    ),
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

test_that("row names are extracted from data frames", {
  x <- data.frame(x = 1:5, row.names = letters[1:5])
  expect_equal(names(slide(x, ~.x)), letters[1:5])
})

test_that("row names are extracted from arrays", {
  x <- array(1:4, c(2, 2), dimnames = list(c("r1", "r2"), c("c1", "c2")))
  expect_equal(names(slide(x, ~.x)), c("r1", "r2"))
})

test_that("names are retained on inner sliced object", {
  names <- letters[1:5]
  x <- set_names(1:5, names)
  exp <- set_names(as.list(names), names)
  expect_equal(slide(x, ~ names(.x)), exp)

  names <- letters[1:5]
  x <- data.frame(x = 1:5, row.names = names)
  expect <- set_names(as.list(names), names)
  expect_equal(slide(x, ~ rownames(.x)), expect)

  names <- c("r1", "r2")
  x <- array(1:4, c(2, 2), dimnames = list(names, c("c1", "c2")))
  exp <- set_names(as.list(names), names)
  expect_equal(slide(x, ~ rownames(.x)), exp)
})

# ------------------------------------------------------------------------------
# validation

test_that("cannot use invalid .before", {
  expect_snapshot(error = TRUE, slide(1, identity, .before = c(1, 2)))
  expect_snapshot({
    (expect_error(
      slide(1, identity, .before = "x"),
      class = "vctrs_error_incompatible_type"
    ))
  })
})

test_that("cannot use invalid .after", {
  expect_snapshot(error = TRUE, slide(1, identity, .after = c(1, 2)))
  expect_snapshot({
    (expect_error(
      slide(1, identity, .after = "x"),
      class = "vctrs_error_incompatible_type"
    ))
  })
})

test_that("cannot use invalid .step", {
  expect_snapshot(error = TRUE, slide(1, identity, .step = -1))
  expect_snapshot(error = TRUE, slide(1, identity, .step = 0))

  expect_snapshot(error = TRUE, slide(1, identity, .step = c(1, 2)))
  expect_snapshot({
    (expect_error(
      slide(1, identity, .step = "x"),
      class = "vctrs_error_incompatible_type"
    ))
  })
})

test_that("cannot use invalid .complete", {
  expect_snapshot(error = TRUE, slide(1, identity, .complete = c(TRUE, TRUE)))
  expect_snapshot({
    (expect_error(
      slide(1, identity, .complete = "hi"),
      class = "vctrs_error_incompatible_type"
    ))
  })
})

# ------------------------------------------------------------------------------
# misc

test_that("slide() forces arguments in the same way as base R / map()", {
  f_slide <- slide(1:2, function(i) function(x) x + i)
  f_base <- lapply(1:2, function(i) function(x) x + i)

  expect_equal(f_slide[[1]](0), f_base[[1]](0))
  expect_equal(f_slide[[2]](0), f_base[[2]](0))
})

test_that(
  paste0(
    "proof that the `window_stop < window_start` check is required for ",
    "cases where the window is completely OOB"
  ),
  {
    expect_equal(
      slide(1:3, identity, .before = 4, .after = -4),
      list(integer(), integer(), integer())
    )
  }
)

test_that("`error_call` and `.error_call` args aren't swallowed", {
  fn <- function(x, error_call) {
    abort("hi", call = error_call)
  }
  fn_dot <- function(x, .error_call) {
    abort("hi", call = .error_call)
  }

  expect_snapshot(error = TRUE, {
    slide(1, fn, error_call = call("foo"))
  })
  expect_snapshot(error = TRUE, {
    slide(1, fn_dot, .error_call = call("foo"))
  })
})

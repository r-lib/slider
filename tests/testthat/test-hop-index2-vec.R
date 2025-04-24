# ------------------------------------------------------------------------------
# hop_index2_vec

test_that("hop_index2_vec() works", {
  expect_identical(hop_index2_vec(1L, 1L, 1, 1, 1, ~ .x + .y), 2L)
})

test_that("hop_index2_vec() doesn't retains names of x (#75)", {
  expect_named(hop_index2_vec(c(x = 1L), c(y = 1L), 1, 1, 1, ~ .x + .y), NULL)
})

test_that("hop_index2_vec() can simplify automatically", {
  expect_identical(hop_index2_vec(1, 2, 1, 1, 1, ~ .x + .y, .ptype = NULL), 3)
})

test_that("hop_index2_vec() errors if it can't simplify", {
  fn <- function(x, y) {
    if (x == 1L) {
      1
    } else {
      "hi"
    }
  }

  expect_snapshot({
    (expect_error(
      hop_index2_vec(1:2, 1:2, 1:2, 1:2, 1:2, fn, .ptype = NULL),
      class = "vctrs_error_incompatible_type"
    ))
  })
})

# ------------------------------------------------------------------------------
# .ptype

test_that("`.ptype = NULL` validates that element lengths are 1", {
  expect_snapshot(error = TRUE, {
    hop_index2_vec(
      1:2,
      1:2,
      1:2,
      1:2,
      1:2,
      ~ if (.x == 1L) {
        1:2
      } else {
        1
      },
      .ptype = NULL
    )
  })
  expect_snapshot(error = TRUE, {
    hop_index2_vec(
      1:2,
      1:2,
      1:2,
      1:2,
      1:2,
      ~ if (.x == 1L) {
        NULL
      } else {
        2
      },
      .ptype = NULL
    )
  })
})

test_that("size 0 `.starts` / `.stops` returns size 0 `.ptype`", {
  expect_identical(
    hop_index2_vec(1:5, 1:5, 1:5, integer(), integer(), ~.x, .ptype = NULL),
    NULL
  )
  expect_identical(
    hop_index2_vec(1:5, 1:5, 1:5, integer(), integer(), ~.x, .ptype = double()),
    double()
  )
})

test_that("`hop_index2_vec()` falls back to `c()` method as required", {
  local_c_foobar()

  expect_identical(
    hop_index2_vec(
      1:3,
      1:3,
      1:3,
      1:3,
      1:3,
      ~ foobar(.x),
      .ptype = foobar(integer())
    ),
    foobar(1:3)
  )
  expect_condition(
    hop_index2_vec(
      1:3,
      1:3,
      1:3,
      1:3,
      1:3,
      ~ foobar(.x),
      .ptype = foobar(integer())
    ),
    class = "slider_c_foobar"
  )

  expect_identical(
    hop_index2_vec(1:3, 1:3, 1:3, 1:3, 1:3, ~ foobar(.x)),
    foobar(1:3)
  )
  expect_condition(
    hop_index2_vec(1:3, 1:3, 1:3, 1:3, 1:3, ~ foobar(.x)),
    class = "slider_c_foobar"
  )
})

# ------------------------------------------------------------------------------
# type / size strict-ness

test_that("size of each `.f` result must be 1", {
  expect_snapshot(error = TRUE, hop_vec(1:2, 1, 1, ~ c(.x, 1)))
})

test_that("inner type is allowed to be different", {
  expect_equal(
    hop_vec(
      1:2,
      1:2,
      1:2,
      ~ if (.x == 1L) {
        list(1)
      } else {
        list("hi")
      },
      .ptype = list()
    ),
    list(1, "hi")
  )
})

test_that("inner type can be restricted with list_of", {
  expect_snapshot({
    (expect_error(
      hop_vec(
        1:2,
        1:2,
        1:2,
        ~ if (.x == 1L) {
          list_of(1)
        } else {
          list_of("hi")
        },
        .ptype = list_of(.ptype = double())
      ),
      class = "vctrs_error_incompatible_type"
    ))
  })
})

# ------------------------------------------------------------------------------
# .ptype

test_that(".ptype is respected", {
  expect_equal(hop_vec(1, 1, 1, ~.x), 1)
  expect_equal(hop_vec(1, 1, 1, ~.x, .ptype = int()), 1L)
  expect_error(
    hop_vec(1, 1, 1, ~ .x + .5, .ptype = integer()),
    class = "vctrs_error_cast_lossy"
  )
})

test_that("`.ptype = NULL` results in 'guessed' .ptype", {
  expect_equal(
    hop_vec(1, 1, 1, ~.x, .ptype = NULL),
    hop_vec(1, 1, 1, ~.x, .ptype = dbl())
  )
})

test_that("`.ptype = NULL` fails if no common type is found", {
  expect_snapshot({
    (expect_error(
      hop_vec(1:2, 1:2, 1:2, ~ ifelse(.x == 1L, "hello", 1), .ptype = NULL),
      class = "vctrs_error_incompatible_type"
    ))
  })
})

test_that("`.ptype = NULL` validates that element lengths are 1", {
  expect_snapshot(error = TRUE, {
    hop_vec(
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
    hop_vec(
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

test_that("`.ptype = NULL` returns `NULL` with size 0 `.x`", {
  expect_equal(
    hop_vec(integer(), integer(), integer(), ~.x, .ptype = NULL),
    NULL
  )
})

test_that(".ptypes with a vec_proxy() are restored to original type", {
  expect_s3_class(
    hop_vec(Sys.Date() + 1:5, 1:5, 1:5, ~.x, .ptype = as.POSIXlt(Sys.Date())),
    "POSIXlt"
  )
})

test_that("can return a matrix and rowwise bind the results together", {
  mat <- matrix(1, ncol = 2)
  expect_equal(
    hop_vec(1:5, 1:5, 1:5, ~mat, .ptype = mat),
    rbind(mat, mat, mat, mat, mat)
  )
})

test_that("`hop_vec()` falls back to `c()` method as required", {
  local_c_foobar()

  expect_identical(
    hop_vec(1:3, 1:3, 1:3, ~ foobar(.x), .ptype = foobar(integer())),
    foobar(1:3)
  )
  expect_condition(
    hop_vec(1:3, 1:3, 1:3, ~ foobar(.x), .ptype = foobar(integer())),
    class = "slider_c_foobar"
  )

  expect_identical(hop_vec(1:3, 1:3, 1:3, ~ foobar(.x)), foobar(1:3))
  expect_condition(
    hop_vec(1:3, 1:3, 1:3, ~ foobar(.x)),
    class = "slider_c_foobar"
  )
})

# ------------------------------------------------------------------------------
# input names

test_that("names exist on inner sliced elements", {
  names <- letters[1:5]
  x <- set_names(1:5, names)
  exp <- as.list(names)
  expect_equal(hop_vec(x, 1:5, 1:5, ~ list(names(.x))), exp)
})

test_that("names are never placed on the output", {
  x <- set_names(1:5, letters[1:5])
  expect_null(names(hop_vec(x, 1:5, 1:5, ~.x)))
  expect_null(names(hop_vec(x, 1:5, 1:5, ~.x, .ptype = int())))
})

# ------------------------------------------------------------------------------
# type / size strict-ness

test_that("size of each `.f` result must be 1", {
  expect_snapshot(error = TRUE, slide_index_vec(1:2, 1:2, ~c(.x, 1)))
  expect_snapshot(error = TRUE, slide_index_dbl(1:2, 1:2, ~c(.x, 1)))
})

test_that("inner type is allowed to be different", {
  expect_equal(
    slide_index_vec(1:2, 1:2, ~if (.x == 1L) {list(1)} else {list("hi")}, .ptype = list()),
    list(1, "hi")
  )
})

test_that("inner type can be restricted with list_of", {
  expect_snapshot({
    (expect_error(
      slide_index_vec(1:2, 1:2, ~if (.x == 1L) {list_of(1)} else {list_of("hi")}, .ptype = list_of(.ptype = double())),
      class = "vctrs_error_incompatible_type"
    ))
  })
})

test_that("type of suffixed versions can be restricted", {
  expect_snapshot({
    (expect_error(
      slide_index_dbl(1:2, 1:2, ~if (.x == 1L) {1} else {"hi"}),
      class = "vctrs_error_incompatible_type"
    ))
  })
})

# ------------------------------------------------------------------------------
# .ptype

test_that(".ptype is respected", {
  expect_equal(slide_index_vec(1, 1, ~.x), 1)
  expect_equal(slide_index_vec(1, 1, ~.x, .ptype = int()), 1L)
  expect_snapshot({
    (expect_error(slide_index_vec(1, 1, ~.x + .5, .ptype = integer()), class = "vctrs_error_cast_lossy"))
  })
})

test_that("`.ptype = NULL` results in 'guessed' .ptype", {
  expect_equal(
    slide_index_vec(1, 1, ~.x, .ptype = NULL),
    slide_index_vec(1, 1, ~.x, .ptype = dbl())
  )
})

test_that("`.ptype = NULL` fails if no common type is found", {
  expect_snapshot({
    (expect_error(
      slide_index_vec(1:2, 1:2, ~ifelse(.x == 1L, "hello", 1), .ptype = NULL),
      class = "vctrs_error_incompatible_type"
    ))
  })
})

test_that("`.ptype = NULL` validates that element lengths are 1", {
  expect_snapshot(error = TRUE, {
    slide_index_vec(1:2, 1:2, ~if(.x == 1L) {1:2} else {1}, .ptype = NULL)
  })
})

test_that("size 0 `.x` returns .ptype", {
  expect_identical(slide_index_vec(integer(), integer(), ~.x, .ptype = NULL), NULL)
  expect_identical(slide_index_vec(integer(), integer(), ~.x, .ptype = double()), double())
})

test_that("`.ptype = NULL` is size stable (#78)", {
  expect_length(slide_index_vec(1:4, 1:4, ~1, .before = 1, .complete = TRUE), 4)
})

test_that(".ptypes with a vec_proxy() are restored to original type", {
  expect_s3_class(
    slide_index_vec(Sys.Date() + 1:5, 1:5, ~.x, .ptype = as.POSIXlt(Sys.Date())),
    "POSIXlt"
  )
})

test_that("can return a matrix and rowwise bind the results together", {
  mat <- matrix(1, ncol = 2)
  expect_equal(
    slide_index_vec(1:5, 1:5, ~mat, .ptype = mat),
    rbind(mat, mat, mat, mat, mat)
  )
})

test_that("`slide_index_vec()` falls back to `c()` method as required", {
  local_c_foobar()

  expect_identical(slide_index_vec(1:3, 1:3, ~foobar(.x), .ptype = foobar(integer())), foobar(1:3))
  expect_condition(slide_index_vec(1:3, 1:3, ~foobar(.x), .ptype = foobar(integer())), class = "slider_c_foobar")

  expect_identical(slide_index_vec(1:3, 1:3, ~foobar(.x)), foobar(1:3))
  expect_condition(slide_index_vec(1:3, 1:3, ~foobar(.x)), class = "slider_c_foobar")
})

# ------------------------------------------------------------------------------
# .complete

test_that(".complete produces typed `NA` values", {
  expect_identical(slide_index_int(1:3, 1:3, ~1L, .before = 1, .complete = TRUE), c(NA, 1L, 1L))
  expect_identical(slide_index_dbl(1:3, 1:3, ~1, .before = 1, .complete = TRUE), c(NA, 1, 1))
  expect_identical(slide_index_chr(1:3, 1:3, ~"1", .before = 1, .complete = TRUE), c(NA, "1", "1"))
  expect_identical(slide_index_vec(1:3, 1:3, ~1, .before = 1, .complete = TRUE), c(NA, 1, 1))
  expect_identical(slide_index_vec(1:3, 1:3, ~1, .before = 1, .complete = TRUE, .ptype = integer()), c(NA, 1L, 1L))
})

# ------------------------------------------------------------------------------
# suffix tests

test_that("slide_index_int() works", {
  expect_equal(slide_index_int(1L, 1L, ~.x), 1L)
})

test_that("slide_index_int() can coerce", {
  expect_equal(slide_index_int(1, 1, ~.x), 1L)
})

test_that("slide_index_dbl() works", {
  expect_equal(slide_index_dbl(1, 1, ~.x), 1)
})

test_that("slide_index_dbl() can coerce", {
  expect_equal(slide_index_dbl(1L, 1, ~.x), 1)
})

test_that("slide_index_chr() works", {
  expect_equal(slide_index_chr("x", 1, ~.x), "x")
})

test_that("slide_index_chr() cannot coerce", {
  expect_snapshot({
    (expect_error(slide_index_chr(1, 1, ~.x), class = "vctrs_error_incompatible_type"))
  })
})

test_that("slide_index_lgl() works", {
  expect_equal(slide_index_lgl(TRUE, 1, ~.x), TRUE)
})

test_that("slide_index_lgl() can coerce", {
  expect_equal(slide_index_lgl(1, 1, ~.x), TRUE)
})

# ------------------------------------------------------------------------------
# data frame suffix tests

test_that("slide_index_dfr() works", {
  expect_identical(
    slide_index_dfr(
      1:2,
      1:2,
      ~new_data_frame(list(x = list(.x))),
      .before = 1
    ),
    data_frame(
      x = list(1L, 1:2)
    )
  )
})

test_that("slide_index_dfc() works", {
  x <- 1:2

  fn <- function(x) {
    if (length(x) == 1) {
      data.frame(x1 = x)
    } else {
      data.frame(x2 = x)
    }
  }

  expect_identical(
    slide_index_dfc(
      1:2,
      1:2,
      fn,
      .before = 1
    ),
    data.frame(
      x1 = c(1L, 1L),
      x2 = 1:2
    )
  )
})

# ------------------------------------------------------------------------------
# recycling

test_that("size 1 results are recycled when there are repeated indices", {
  i <- c(1, 1, 2, 3, 3)
  x <- seq_along(i)

  expect_equal(
    slide_index_dbl(x, i, mean),
    vapply(vec_slice(vec_split(x, i)$val, i), mean, double(1))
  )
})

# ------------------------------------------------------------------------------
# type / size strict-ness

test_that("size of each `.f` result must be 1", {
  expect_error(
    slide_period_vec(1:2, new_date(1:2), "day", ~c(.x, 1)),
    "In iteration 1, the result of `.f` had size 2, not 1"
  )
})

test_that("inner type is allowed to be different", {
  expect_equal(
    slide_period_vec(1:2, new_date(1:2), "day", ~if (.x == 1L) {1} else {"hi"}, .ptype = list()),
    list(1, "hi")
  )
})

test_that("inner type can be restricted with list_of", {
  expect_error(
    slide_period_vec(1:2, new_date(1:2), "day", ~if (.x == 1L) {1} else {"hi"}, .ptype = list_of(.ptype = double())),
    class = "vctrs_error_cast_lossy"
  )
})

# ------------------------------------------------------------------------------
# .ptype

test_that(".ptype is respected", {
  expect_equal(slide_period_vec(1, new_date(0), "day", ~.x), 1)
  expect_equal(slide_period_vec(1, new_date(0), "day", ~.x, .ptype = int()), 1L)
  expect_equal(slide_period_vec(1, new_date(0), "day", ~.x, .ptype = new_date()), as.Date("1970-01-02"))
  expect_error(slide_period_vec(1, new_date(0), "day", ~.x + .5, .ptype = integer()), class = "vctrs_error_cast_lossy")
})

test_that("`.ptype = NULL` results in 'guessed' .ptype", {
  expect_equal(
    slide_period_vec(1, new_date(0), "day", ~.x, .ptype = NULL),
    slide_period_vec(1, new_date(0), "day", ~.x, .ptype = dbl())
  )
})

test_that("`.ptype = NULL` fails if no common type is found", {
  expect_error(
    slide_period_vec(1:2, new_date(0:1), "day", ~ifelse(.x == 1L, "hello", 1), .ptype = NULL),
    class = "vctrs_error_incompatible_type"
  )
})

test_that("`.ptype = NULL` validates that element lengths are 1", {
  expect_error(
    slide_period_vec(1:2, new_date(0:1), "day", ~if(.x == 1L) {1:2} else {1}, .ptype = NULL),
    "In iteration 1, the result of `.f` had size 2, not 1."
  )
})

test_that("`.ptype = NULL` returns `NULL` with size 0 `.x`", {
  expect_equal(slide_period_vec(integer(), new_date(), "day", ~.x, .ptype = NULL), NULL)
})

test_that(".ptypes with a vec_proxy() are restored to original type", {
  expect_is(
    slide_period_vec(Sys.Date() + 1:5, new_date(1:5), "day", ~.x, .ptype = as.POSIXlt(Sys.Date())),
    "POSIXlt"
  )
})

test_that("with `.complete = TRUE`, `.ptype` is used to pad", {
  expect_equal(
    slide_period_dbl(
      1:3, new_date(1:3),
      "day", ~1, .before = 1, .complete = TRUE
    ),
    c(NA, 1, 1)
  )

  expect_equal(
    slide_period_vec(
      1:3, new_date(1:3),
      "day", ~new_date(0), .before = 1, .complete = TRUE, .ptype = new_date()
    ),
    new_date(c(NA, 0, 0))
  )
})

# ------------------------------------------------------------------------------
# suffix tests

test_that("slide_period_int() works", {
  expect_equal(slide_period_int(1L, new_date(0), "day", ~.x), 1L)
})

test_that("slide_period_int() can coerce", {
  expect_equal(slide_period_int(1, new_date(0), "day", ~.x), 1L)
})

test_that("slide_period_dbl() works", {
  expect_equal(slide_period_dbl(1, new_date(0), "day", ~.x), 1)
})

test_that("slide_period_dbl() can coerce", {
  expect_equal(slide_period_dbl(1L, new_date(0), "day", ~.x), 1)
})

test_that("slide_period_chr() works", {
  expect_equal(slide_period_chr("x", new_date(0), "day", ~.x), "x")
})

test_that("slide_period_chr() can coerce", {
  expect_equal(slide_period_chr(1, new_date(0), "day", ~.x), "1")
})

test_that("slide_period_lgl() works", {
  expect_equal(slide_period_lgl(TRUE, new_date(0), "day", ~.x), TRUE)
})

test_that("slide_period_lgl() can coerce", {
  expect_equal(slide_period_lgl(1, new_date(0), "day", ~.x), TRUE)
})

# ------------------------------------------------------------------------------
# data frame suffix tests

test_that("slide_period_dfr() works", {
  expect_equal(
    slide_period_dfr(1:2, new_date(1:2), "day", ~.x, .before = 1),
    slide_dfr(1:2, ~.x, .before = 1)
  )

  x <- 1:2
  expect_equal(
    slide_period_dfr(x, new_date(1:2), "day", ~data.frame(x = .x), .before = 1),
    slide_dfr(x, ~data.frame(x = .x), .before = 1)
  )
})

test_that("slide_period_dfc() works", {
  expect_equal(
    slide_period_dfc(1:2, new_date(1:2), "day", ~.x, .before = 1),
    slide_dfc(1:2, ~.x, .before = 1)
  )

  x <- 1:2
  expect_equal(
    slide_period_dfc(x, new_date(1:2), "day", ~data.frame(x = .x), .before = 1),
    slide_dfc(x, ~data.frame(x = .x), .before = 1)
  )
})

# ------------------------------------------------------------------------------
# failing tests

# TODO - failing test for OBJECT() that doesn't implement a proxy?
# would also need to use `vec_assign_fallback()`

# (need to use `vec_assign_fallback()`, there is a note in slice.c)
test_that("can return a matrix and rowwise bind the results together", {
  mat <- matrix(1, ncol = 2)
  expect_failure({
    expect_error(
      slide_period_vec(1:5, new_date(1:5), "day", ~mat, .ptype = mat),
      NA
    )
  })

  # expect_equal(
  #   slide_period_vec(1:5, 1:5, ~mat, .ptype = mat),
  #   rbind(mat, mat, mat, mat, mat)
  # )
})





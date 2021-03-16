# ------------------------------------------------------------------------------
# type / size strict-ness

test_that("size of each `.f` result must be 1", {
  expect_error(
    pslide_period_vec(list(1:2, 1:2), new_date(c(1, 2)), "day", ~c(.x, .y)),
    "In iteration 1, the result of `.f` had size 2, not 1"
  )
  expect_error(
    pslide_period_int(list(1:2, 1:2), new_date(c(1, 2)), "day", ~c(.x, .y)),
    "In iteration 1, the result of `.f` had size 2, not 1"
  )
})

test_that("inner type is allowed to be different", {
  expect_equal(
    pslide_period_vec(list(1:2, 1:2), new_date(c(1, 2)), "day", ~if (.x == 1L) {list(1)} else {list("hi")}, .ptype = list()),
    list(1, "hi")
  )
})

test_that("inner type can be restricted with list_of", {
  expect_error(
    pslide_period_vec(list(1:2, 1:2), new_date(c(1, 2)), "day", ~if (.x == 1L) {list_of(1)} else {list_of("hi")}, .ptype = list_of(.ptype = double())),
    class = "vctrs_error_incompatible_type"
  )
})

test_that("type can be restricted", {
  expect_error(
    pslide_period_int(list(1:2, 1:2), new_date(c(1, 2)), "day", ~if (.x == 1L) {1L} else {"hi"}),
    class = "vctrs_error_incompatible_type"
  )
})

test_that("empty input works with `.complete = TRUE` (#111)", {
  expect_equal(pslide_period_dbl(list(integer(), integer()), new_date(), "year", ~.x, .complete = TRUE), double())
})

# ------------------------------------------------------------------------------
# .ptype

test_that(".ptype is respected", {
  expect_equal(pslide_period_vec(list(1, 1), new_date(0), "day", ~.x), 1)
  expect_equal(pslide_period_vec(list(1, 1), new_date(0), "day", ~.x, .ptype = int()), 1L)
  expect_error(pslide_period_vec(list(1, 1), new_date(0), "day", ~.x + .5, .ptype = integer()), class = "vctrs_error_cast_lossy")
})

test_that("`.ptype = NULL` results in 'guessed' .ptype", {
  expect_equal(
    pslide_period_vec(list(1, 1), new_date(0), "day", ~.x, .ptype = NULL),
    pslide_period_vec(list(1, 1), new_date(0), "day", ~.x, .ptype = dbl())
  )
})

test_that("`.ptype = NULL` fails if no common type is found", {
  expect_error(
    pslide_period_vec(list(1:2, 1:2), new_date(c(0, 1)), "day", ~ifelse(.x == 1L, "hello", 1), .ptype = NULL),
    class = "vctrs_error_incompatible_type"
  )
})

test_that("`.ptype = NULL` validates that element lengths are 1", {
  expect_error(
    pslide_period_vec(list(1:2, 1:2), new_date(c(0, 1)), "day", ~if(.x == 1L) {1:2} else {1}, .ptype = NULL),
    "In iteration 1, the result of `.f` had size 2, not 1."
  )
  expect_error(
    pslide_period_vec(list(1:2, 1:2), new_date(c(0, 1)), "day", ~if(.x == 1L) {NULL} else {1}, .ptype = NULL),
    "In iteration 1, the result of `.f` had size 0, not 1."
  )
})

test_that("`.ptype = NULL` returns `NULL` with size 0 `.x`", {
  expect_equal(pslide_period_vec(list(integer(), integer()), new_date(), "day", ~.x, .ptype = NULL), NULL)
})

test_that(".ptypes with a vec_proxy() are restored to original type", {
  expect_s3_class(
    pslide_period_vec(list(Sys.Date() + 1:5, 1:5), new_date(c(1, 2, 3, 4, 5)), "day", ~.x, .ptype = as.POSIXlt(Sys.Date())),
    "POSIXlt"
  )
})

test_that("with `.complete = TRUE`, `.ptype` is used to pad", {
  expect_equal(
    pslide_period_dbl(
      list(1:3, 1:3), new_date(c(1, 2, 3)),
      "day", ~1, .before = 1, .complete = TRUE
    ),
    c(NA, 1, 1)
  )
})

test_that("with `.complete = TRUE`, padding is size stable (#93)", {
  expect_equal(
    pslide_period_vec(
      list(1:3, 1:3), new_date(c(1, 2, 3)),
      "day", ~new_date(0), .before = 1, .complete = TRUE, .ptype = new_date()
    ),
    new_date(c(NA, 0, 0))
  )
  expect_equal(
    pslide_period_vec(
      list(1:3, 1:3), new_date(c(1, 2, 3)),
      "day", ~new_date(0), .after = 1, .complete = TRUE, .ptype = new_date()
    ),
    new_date(c(0, 0, NA))
  )
  expect_equal(
    pslide_period_vec(
      list(1:3, 1:3), new_date(c(1, 2, 3)),
      "day", ~new_date(0), .before = 1, .complete = TRUE, .ptype = NULL
    ),
    new_date(c(NA, 0, 0))
  )
})

test_that("can return a matrix and rowwise bind the results together", {
  mat <- matrix(1, ncol = 2)
  expect_equal(
    pslide_period_vec(list(1:5, 1:5), new_date(c(1, 2, 3, 4, 5)), "day", ~mat, .ptype = mat),
    rbind(mat, mat, mat, mat, mat)
  )
})

test_that("`pslide_period_vec()` falls back to `c()` method as required", {
  local_c_foobar()

  expect_identical(pslide_period_vec(list(1:3, 1:3), new_date(c(1, 2, 3)), "day", ~foobar(.x), .ptype = foobar(integer())), foobar(1:3))
  expect_condition(pslide_period_vec(list(1:3, 1:3), new_date(c(1, 2, 3)), "day", ~foobar(.x), .ptype = foobar(integer())), class = "slider_c_foobar")

  expect_identical(pslide_period_vec(list(1:3, 1:3), new_date(c(1, 2, 3)), "day", ~foobar(.x)), foobar(1:3))
  expect_condition(pslide_period_vec(list(1:3, 1:3), new_date(c(1, 2, 3)), "day", ~foobar(.x)), class = "slider_c_foobar")
})

# ------------------------------------------------------------------------------
# suffix tests

test_that("pslide_period_int() works", {
  expect_equal(pslide_period_int(list(1L, 1), new_date(0), "day", ~.x), 1L)
})

test_that("pslide_period_int() can coerce", {
  expect_equal(pslide_period_int(list(1, 1), new_date(0), "day", ~.x), 1L)
})

test_that("pslide_period_dbl() works", {
  expect_equal(pslide_period_dbl(list(1, 1), new_date(0), "day", ~.x), 1)
})

test_that("pslide_period_dbl() can coerce", {
  expect_equal(pslide_period_dbl(list(1L, 1), new_date(0), "day", ~.x), 1)
})

test_that("pslide_period_chr() works", {
  expect_equal(pslide_period_chr(list("x", 1), new_date(0), "day", ~.x), "x")
})

test_that("pslide_period_chr() cannot coerce", {
  expect_error(pslide_period_chr(list(1, 1), new_date(0), "day", ~.x), class = "vctrs_error_incompatible_type")
})

test_that("pslide_period_lgl() works", {
  expect_equal(pslide_period_lgl(list(TRUE, 1), new_date(0), "day", ~.x), TRUE)
})

test_that("pslide_period_lgl() can coerce", {
  expect_equal(pslide_period_lgl(list(1, 1), new_date(0), "day", ~.x), TRUE)
})

# ------------------------------------------------------------------------------
# data frame suffix tests

test_that("pslide_period_dfr() works", {
  expect_identical(
    pslide_period_dfr(
      list(1:2, 1:2),
      new_date(c(1, 2)),
      "day",
      ~new_data_frame(list(x = list(.x))),
      .before = 1
    ),
    slide_dfr(1:2, ~new_data_frame(list(x = list(.x))), .before = 1)
  )
})

test_that("pslide_period_dfc() works", {
  x <- 1:2

  fn <- function(x, y) {
    if (length(x) == 1) {
      data.frame(x1 = x, y1 = y)
    } else {
      data.frame(x2 = x, y2 = y)
    }
  }

  expect_identical(
    pslide_period_dfc(
      list(x, x),
      new_date(c(1, 2)),
      "day",
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

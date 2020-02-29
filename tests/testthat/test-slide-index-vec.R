# ------------------------------------------------------------------------------
# type / size strict-ness

test_that("size of each `.f` result must be 1", {
  expect_error(
    slide_index_vec(1:2, 1:2, ~c(.x, 1)),
    "In iteration 1, the result of `.f` had size 2, not 1"
  )
})

test_that("inner type is allowed to be different", {
  expect_equal(
    slide_index_vec(1:2, 1:2, ~if (.x == 1L) {1} else {"hi"}, .ptype = list()),
    list(1, "hi")
  )
})

test_that("inner type can be restricted with list_of", {
  expect_error(
    slide_index_vec(1:2, 1:2, ~if (.x == 1L) {1} else {"hi"}, .ptype = list_of(.ptype = double())),
    class = "vctrs_error_cast_lossy"
  )
})

# ------------------------------------------------------------------------------
# .ptype

test_that(".ptype is respected", {
  expect_equal(slide_index_vec(1, 1, ~.x), 1)
  expect_equal(slide_index_vec(1, 1, ~.x, .ptype = int()), 1L)
  expect_equal(slide_index_vec(1, 1, ~.x, .ptype = new_date()), as.Date("1970-01-02"))
  expect_error(slide_index_vec(1, 1, ~.x + .5, .ptype = integer()), class = "vctrs_error_cast_lossy")
})

test_that("`.ptype = NULL` results in 'guessed' .ptype", {
  expect_equal(
    slide_index_vec(1, 1, ~.x, .ptype = NULL),
    slide_index_vec(1, 1, ~.x, .ptype = dbl())
  )
})

test_that("`.ptype = NULL` fails if no common type is found", {
  expect_error(
    slide_index_vec(1:2, 1:2, ~ifelse(.x == 1L, "hello", 1), .ptype = NULL),
    class = "vctrs_error_incompatible_type"
  )
})

test_that("`.ptype = NULL` validates that element lengths are 1", {
  expect_error(
    slide_index_vec(1:2, 1:2, ~if(.x == 1L) {1:2} else {1}, .ptype = NULL),
    "In iteration 1, the result of `.f` had size 2, not 1."
  )
})

test_that("`.ptype = NULL` returns `NULL` with size 0 `.x`", {
  expect_equal(slide_index_vec(integer(), integer(), ~.x, .ptype = NULL), NULL)
})

test_that(".ptypes with a vec_proxy() are restored to original type", {
  expect_is(
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

test_that("slide_index_chr() can coerce", {
  expect_equal(slide_index_chr(1, 1, ~.x), "1")
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
  expect_equal(
    slide_index_dfr(1:2, 1:2, ~.x, .before = 1),
    slide_dfr(1:2, ~.x, .before = 1)
  )

  x <- 1:2
  expect_equal(
    slide_index_dfr(x, x, ~data.frame(x = .x), .before = 1),
    slide_dfr(x, ~data.frame(x = .x), .before = 1)
  )
})

test_that("slide_index_dfc() works", {
  expect_equal(
    slide_index_dfc(1:2, 1:2, ~.x, .before = 1),
    slide_dfc(1:2, ~.x, .before = 1)
  )

  x <- 1:2
  expect_equal(
    slide_index_dfc(x, x, ~data.frame(x = .x), .before = 1),
    slide_dfc(x, ~data.frame(x = .x), .before = 1)
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

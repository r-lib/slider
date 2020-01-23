# ------------------------------------------------------------------------------
# type / size strict-ness

test_that("size of each `.f` result must be 1", {
  expect_error(
    slide_vec(1:2, ~c(.x, 1)),
    "In iteration 1, the result of `.f` had size 2, not 1"
  )
})

test_that("inner type is allowed to be different", {
  expect_equal(
    slide_vec(1:2, ~if (.x == 1L) {1} else {"hi"}, .ptype = list()),
    list(1, "hi")
  )
})

test_that("inner type can be restricted with list_of", {
  expect_error(
    slide_vec(1:2, ~if (.x == 1L) {1} else {"hi"}, .ptype = list_of(.ptype = double())),
    class = "vctrs_error_cast_lossy"
  )
})

# ------------------------------------------------------------------------------
# .ptype

test_that(".ptype is respected", {
  expect_equal(slide_vec(1, ~.x), 1)
  expect_equal(slide_vec(1, ~.x, .ptype = int()), 1L)
  expect_equal(slide_vec(1, ~.x, .ptype = new_date()), as.Date("1970-01-02"))
  expect_error(slide_vec(1, ~.x + .5, .ptype = integer()), class = "vctrs_error_cast_lossy")
})

test_that("`.ptype = NULL` results in 'guessed' .ptype", {
  expect_equal(
    slide_vec(1, ~.x, .ptype = NULL),
    slide_vec(1, ~.x, .ptype = dbl())
  )
})

test_that("`.ptype = NULL` fails if no common type is found", {
  expect_error(
    slide_vec(1:2, ~ifelse(.x == 1L, "hello", 1), .ptype = NULL),
    class = "vctrs_error_incompatible_type"
  )
})

test_that("`.ptype = NULL` validates that element lengths are 1", {
  expect_error(
    slide_vec(1:2, ~if(.x == 1L) {1:2} else {1}, .ptype = NULL),
    "In iteration 1, the result of `.f` had size 2, not 1."
  )
})

test_that("`.ptype = NULL` returns `NULL` with size 0 `.x`", {
  expect_equal(slide_vec(integer(), ~.x, .ptype = NULL), NULL)
})

test_that(".ptypes with a vec_proxy() are restored to original type", {
  expect_is(
    slide_vec(Sys.Date() + 1:5, ~.x, .ptype = as.POSIXlt(Sys.Date())),
    "POSIXlt"
  )
})

# ------------------------------------------------------------------------------
# input names

test_that("names exist on inner sliced elements", {
  names <- letters[1:5]
  x <- set_names(1:5, names)
  exp <- set_names(as.list(names), names)
  expect_equal(slide_vec(x, ~list(names(.x))), exp)
})

test_that("names can be placed on atomics", {
  names <- letters[1:5]
  x <- set_names(1:5, names)
  expect_equal(names(slide_vec(x, ~.x)), names)
  expect_equal(names(slide_vec(x, ~.x, .ptype = int())), names)
  expect_equal(names(slide_vec(x, ~.x, .ptype = dbl())), names)
})

test_that("when simplifying, names from `.x` are kept, and new names from `.f` results are dropped", {
  x <- set_names(1, "x")

  expect_identical(
    slide_vec(x, ~c(y = 2), .ptype = NULL),
    c(x = 2)
  )
})

test_that("names are not placed on data frames rownames", {
  names <- letters[1:2]
  x <- set_names(1:2, names)
  out <- slide_vec(x, ~data.frame(x = .x), .ptype = data.frame(x = int()))
  expect_equal(rownames(out), c("1", "2"))
})

test_that("names can be placed on arrays", {
  names <- letters[1:2]
  x <- set_names(1:2, names)
  out <- slide_vec(x, ~array(.x, c(1, 1)), .ptype = array(int(), dim = c(0, 1)))
  expect_equal(rownames(out), names)
})

test_that("names can be placed correctly on proxied objects", {
  names <- letters[1:2]
  x <- set_names(1:2, names)
  datetime_lt <- as.POSIXlt(new_datetime(0))
  out <- slide_vec(x, ~datetime_lt, .ptype = datetime_lt)
  expect_equal(names(out), names)
})

# ------------------------------------------------------------------------------
# suffix tests

test_that("slide_int() works", {
  expect_equal(slide_int(1L, ~.x), 1L)
})

test_that("slide_int() can coerce", {
  expect_equal(slide_int(1, ~.x), 1L)
})

test_that("slide_dbl() works", {
  expect_equal(slide_dbl(1, ~.x), 1)
})

test_that("slide_dbl() can coerce", {
  expect_equal(slide_dbl(1L, ~.x), 1)
})

test_that("slide_chr() works", {
  expect_equal(slide_chr("x", ~.x), "x")
})

test_that("slide_chr() can coerce", {
  expect_equal(slide_chr(1, ~.x), "1")
})

test_that("slide_lgl() works", {
  expect_equal(slide_lgl(TRUE, ~.x), TRUE)
})

test_that("slide_lgl() can coerce", {
  expect_equal(slide_lgl(1, ~.x), TRUE)
})

# ------------------------------------------------------------------------------
# data frame suffix tests

test_that("slide_dfr() works", {
  expect_equal(
    slide_dfr(1:2, ~.x, .before = 1),
    data.frame(...1 = c(1, 1), ...2 = c(NA, 2))
  )

  x <- 1:2
  expect_equal(
    slide_dfr(x, ~data.frame(x = .x), .before = 1),
    data.frame(x = c(1, 1, 2))
  )
})

test_that("slide_dfc() works", {
  expect_equal(
    slide_dfc(1:2, ~.x, .before = 1),
    data.frame(...1 = c(1, 1), ...2 = c(1, 2))
  )

  x <- 1:2
  expect_equal(
    slide_dfc(x, ~data.frame(x = .x), .before = 1),
    data.frame(x...1 = c(1, 1), x...2 = c(1, 2))
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
      slide_vec(1:5, ~mat, .ptype = mat),
      NA
    )
  })

  # expect_equal(
  #   slide_vec(1:5, ~mat, .ptype = mat),
  #   rbind(mat, mat, mat, mat, mat)
  # )
})





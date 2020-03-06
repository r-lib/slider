# ------------------------------------------------------------------------------
# type / size strict-ness

test_that("size of each `.f` result must be 1", {
  expect_error(
    hop_vec(1:2, 1, 1, ~c(.x, 1)),
    "In iteration 1, the result of `.f` had size 2, not 1"
  )
})

test_that("inner type is allowed to be different", {
  expect_equal(
    hop_vec(1:2, 1:2, 1:2, ~if (.x == 1L) {1} else {"hi"}, .ptype = list()),
    list(1, "hi")
  )
})

test_that("inner type can be restricted with list_of", {
  expect_error(
    hop_vec(1:2, 1:2, 1:2, ~if (.x == 1L) {1} else {"hi"}, .ptype = list_of(.ptype = double())),
    class = "vctrs_error_cast_lossy"
  )
})

# ------------------------------------------------------------------------------
# .ptype

test_that(".ptype is respected", {
  expect_equal(hop_vec(1, 1, 1, ~.x), 1)
  expect_equal(hop_vec(1, 1, 1, ~.x, .ptype = int()), 1L)
  expect_equal(hop_vec(1, 1, 1, ~.x, .ptype = new_date()), as.Date("1970-01-02"))
  expect_error(hop_vec(1, 1, 1, ~.x + .5, .ptype = integer()), class = "vctrs_error_cast_lossy")
})

test_that("`.ptype = NULL` results in 'guessed' .ptype", {
  expect_equal(
    hop_vec(1, 1, 1, ~.x, .ptype = NULL),
    hop_vec(1, 1, 1, ~.x, .ptype = dbl())
  )
})

test_that("`.ptype = NULL` fails if no common type is found", {
  expect_error(
    hop_vec(1:2, 1:2, 1:2, ~ifelse(.x == 1L, "hello", 1), .ptype = NULL),
    class = "vctrs_error_incompatible_type"
  )
})

test_that("`.ptype = NULL` validates that element lengths are 1", {
  expect_error(
    hop_vec(1:2, 1:2, 1:2, ~if(.x == 1L) {1:2} else {1}, .ptype = NULL),
    "In iteration 1, the result of `.f` had size 2, not 1."
  )
})

test_that("`.ptype = NULL` returns `NULL` with size 0 `.x`", {
  expect_equal(hop_vec(integer(), integer(), integer(), ~.x, .ptype = NULL), NULL)
})

test_that(".ptypes with a vec_proxy() are restored to original type", {
  expect_is(
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

# ------------------------------------------------------------------------------
# input names

test_that("names exist on inner sliced elements", {
  names <- letters[1:5]
  x <- set_names(1:5, names)
  exp <- set_names(as.list(names), names)
  expect_equal(hop_vec(x, 1:5, 1:5, ~list(names(.x))), exp)
})

test_that("names can be placed on atomics", {
  names <- letters[1:5]
  x <- set_names(1:5, names)
  expect_equal(names(hop_vec(x, 1:5, 1:5, ~.x)), names)
  expect_equal(names(hop_vec(x, 1:5, 1:5, ~.x, .ptype = int())), names)
  expect_equal(names(hop_vec(x, 1:5, 1:5, ~.x, .ptype = dbl())), names)
})

test_that("names are not placed on data frames rownames", {
  names <- letters[1:2]
  x <- set_names(1:2, names)
  out <- hop_vec(x, 1:2, 1:2, ~data.frame(x = .x), .ptype = data.frame(x = int()))
  expect_equal(rownames(out), c("1", "2"))
})

test_that("names can be placed on arrays", {
  names <- letters[1:2]
  x <- set_names(1:2, names)
  out <- hop_vec(x, 1:2, 1:2, ~array(.x, c(1, 1)), .ptype = array(int(), dim = c(0, 1)))
  expect_equal(rownames(out), names)
})

test_that("names can be placed correctly on proxied objects", {
  names <- letters[1:2]
  x <- set_names(1:2, names)
  datetime_lt <- as.POSIXlt(new_datetime(0))
  out <- hop_vec(x, 1:2, 1:2, ~datetime_lt, .ptype = datetime_lt)
  expect_equal(names(out), names)
})

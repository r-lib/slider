# ------------------------------------------------------------------------------
# type / size strict-ness

test_that("size of each `.f` result must be 1", {
  expect_error(
    slide_vec(1:2, ~c(.x, 1)),
    "Incompatible lengths"
  )
})

test_that("inner type is allowed to be different", {
  expect_equal(
    slide_vec(1:2, ~if (.x == 1L) {1} else {"hi"}),
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
  expect_equal(slide_vec(1, ~.x), list(1))
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
    "must be 1"
  )
})

test_that(".ptypes with a vec_proxy() are restored to original type", {
  expect_is(
    slide_vec(Sys.Date() + 1:5, ~.x, .ptype = as.POSIXlt(Sys.Date())),
    "POSIXlt"
  )
})

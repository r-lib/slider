# ------------------------------------------------------------------------------
# type / size strict-ness

test_that("size of each `.f` result must be 1", {
  expect_snapshot(error = TRUE, slide_vec(1:2, ~c(.x, 1)))
})

test_that("size of each `.f` result must be 1", {
  expect_snapshot(error = TRUE, slide_dbl(1:2, ~c(.x, 1)))
})

test_that("inner type is allowed to be different", {
  expect_equal(
    slide_vec(1:2, ~if (.x == 1L) {list(1)} else {list("hi")}, .ptype = list()),
    list(1, "hi")
  )
})

test_that("inner type can be restricted with list_of", {
  expect_snapshot({
    (expect_error(
      slide_vec(1:2, ~if (.x == 1L) {list_of(1)} else {list_of("hi")}, .ptype = list_of(.ptype = double())),
      class = "vctrs_error_incompatible_type"
    ))
  })
})

test_that("inner type can be restricted", {
  expect_snapshot({
    (expect_error(
      slide_dbl(1:2, ~if (.x == 1L) {1} else {"x"}),
      class = "vctrs_error_incompatible_type"
    ))
  })
})

# ------------------------------------------------------------------------------
# .ptype

test_that(".ptype is respected", {
  expect_equal(slide_vec(1, ~.x), 1)
  expect_equal(slide_vec(1, ~.x, .ptype = int()), 1L)
  expect_snapshot({
    (expect_error(slide_vec(1, ~.x + .5, .ptype = integer()), class = "vctrs_error_cast_lossy"))
  })
})

test_that("`.ptype = NULL` results in 'guessed' .ptype", {
  expect_equal(
    slide_vec(1, ~.x, .ptype = NULL),
    slide_vec(1, ~.x, .ptype = dbl())
  )
})

test_that("`.ptype = NULL` fails if no common type is found", {
  expect_snapshot({
    (expect_error(
      slide_vec(1:2, ~ifelse(.x == 1L, "hello", 1), .ptype = NULL),
      class = "vctrs_error_incompatible_type"
    ))
  })
})

test_that("`.ptype = NULL` validates that element lengths are 1", {
  expect_snapshot(error = TRUE, {
    slide_vec(1:2, ~if(.x == 1L) {1:2} else {1}, .ptype = NULL)
  })
})

test_that("`.ptype = NULL` returns `NULL` with size 0 `.x`", {
  expect_equal(slide_vec(integer(), ~.x, .ptype = NULL), NULL)
})

test_that("`.ptype = NULL` is size stable (#78)", {
  expect_length(slide_vec(1:4, ~.x, .step = 2), 4)
  expect_length(slide_vec(1:4, ~1, .before = 1, .complete = TRUE), 4)
})

test_that(".ptypes with a vec_proxy() are restored to original type", {
  expect_s3_class(
    slide_vec(Sys.Date() + 1:5, ~.x, .ptype = as.POSIXlt(Sys.Date())),
    "POSIXlt"
  )
})

test_that("can return a matrix and rowwise bind the results together", {
  mat <- matrix(1, ncol = 2)
  expect_equal(
    slide_vec(1:5, ~mat, .ptype = mat),
    rbind(mat, mat, mat, mat, mat)
  )
})

test_that("`slide_vec()` falls back to `c()` method as required", {
  local_c_foobar()

  expect_identical(slide_vec(1:3, ~foobar(.x), .ptype = foobar(integer())), foobar(1:3))
  expect_condition(slide_vec(1:3, ~foobar(.x), .ptype = foobar(integer())), class = "slider_c_foobar")

  expect_identical(slide_vec(1:3, ~foobar(.x)), foobar(1:3))
  expect_condition(slide_vec(1:3, ~foobar(.x)), class = "slider_c_foobar")
})

# ------------------------------------------------------------------------------
# .step

test_that(".step produces typed `NA` values", {
  expect_identical(slide_int(1:3, identity, .step = 2), c(1L, NA, 3L))
  expect_identical(slide_dbl(1:3, identity, .step = 2), c(1, NA, 3))
  expect_identical(slide_chr(c("a", "b", "c"), identity, .step = 2), c("a", NA, "c"))
  expect_identical(slide_vec(1:3, identity, .step = 2), c(1L, NA, 3L))
  expect_identical(slide_vec(1:3, identity, .step = 2, .ptype = integer()), c(1L, NA, 3L))
})

# ------------------------------------------------------------------------------
# .complete

test_that(".complete produces typed `NA` values", {
  expect_identical(slide_int(1:3, ~1L, .before = 1, .complete = TRUE), c(NA, 1L, 1L))
  expect_identical(slide_dbl(1:3, ~1, .before = 1, .complete = TRUE), c(NA, 1, 1))
  expect_identical(slide_chr(1:3, ~"1", .before = 1, .complete = TRUE), c(NA, "1", "1"))
  expect_identical(slide_vec(1:3, ~1, .before = 1, .complete = TRUE), c(NA, 1, 1))
  expect_identical(slide_vec(1:3, ~1, .before = 1, .complete = TRUE, .ptype = integer()), c(NA, 1L, 1L))
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
  expect_equal(names(slide_int(x, ~.x)), names)
  expect_equal(names(slide_dbl(x, ~.x)), names)
})

test_that("names from `.x` are kept, and new names from `.f` results are dropped", {
  x <- set_names(1, "x")

  expect_identical(slide_vec(x, ~c(y = 2), .ptype = NULL), c(x = 2))
  expect_identical(slide_vec(1, ~c(y = 2), .ptype = NULL), 2)

  expect_identical(slide_dbl(x, ~c(y = 2)), c(x = 2))
  expect_identical(slide_dbl(1, ~c(y = 2)), 2)
})

test_that("names can be placed on data frames", {
  names <- letters[1:2]
  x <- set_names(1:2, names)

  out <- slide_vec(x, ~data.frame(x = .x))
  expect_equal(rownames(out), names)

  out <- slide_vec(x, ~data.frame(x = .x), .ptype = data.frame(x = int()))
  expect_equal(rownames(out), names)
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

test_that("slide_chr() cannot coerce", {
  expect_snapshot({
    (expect_error(slide_chr(1, ~.x), class = "vctrs_error_incompatible_type"))
  })
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
  expect_identical(
    slide_dfr(
      1:2,
      ~new_data_frame(list(x = list(.x))),
      .before = 1
    ),
    data_frame(
      x = list(1L, 1:2)
    )
  )
})

test_that("slide_dfc() works", {
  x <- 1:2

  fn <- function(x) {
    if (length(x) == 1) {
      data.frame(x1 = x)
    } else {
      data.frame(x2 = x)
    }
  }

  expect_identical(
    slide_dfc(1:2, fn, .before = 1),
    data.frame(
      x1 = c(1L, 1L),
      x2 = 1:2
    )
  )
})

# ------------------------------------------------------------------------------
# stop_index_incompatible_type()

test_that("output is verified", {
  expect_snapshot(error = TRUE, check_index_incompatible_type(1, ".i"))
})

test_that("class names are collapsed", {
  x <- structure(1, class = c("foo", "bar", "baz"))
  expect_snapshot(error = TRUE, check_index_incompatible_type(x, ".i"))
})

# ------------------------------------------------------------------------------
# stop_endpoints_must_be_ascending()

test_that("output is verified", {
  expect_snapshot(error = TRUE, {
    check_endpoints_must_be_ascending(c(1, 2, 1, 3, 4, 2), ".starts")
  })
})

# ------------------------------------------------------------------------------
# stop_generated_endpoints_cannot_be_na()

test_that("output is verified", {
  expect_snapshot(error = TRUE, {
    check_generated_endpoints_cannot_be_na(c(NA, 1, NA), ".before")
  })
})

# ------------------------------------------------------------------------------
# stop_endpoints_cannot_be_na()

test_that("output is verified", {
  expect_snapshot(error = TRUE, {
    check_endpoints_cannot_be_na(c(NA, 1, NA), ".starts")
  })
})

# ------------------------------------------------------------------------------
# stop_index_must_be_ascending()

test_that("output is verified", {
  expect_snapshot(error = TRUE, {
    check_index_must_be_ascending(c(1, 2, 1, 4, 5, 3), ".i")
  })
})

test_that("not assuming strictly ascending", {
  expect_silent(check_index_must_be_ascending(c(1, 1)))
})

# ------------------------------------------------------------------------------
# stop_index_cannot_be_na()

test_that("output is verified", {
  expect_snapshot(error = TRUE, {
    check_index_cannot_be_na(c(NA, 1, NA), ".i")
  })
})

test_that("trimming works", {
  expect_snapshot(error = TRUE, {
    check_index_cannot_be_na(rep(NA, 100), ".i")
  })
})

# ------------------------------------------------------------------------------
# stop_index_incompatible_size()

test_that("output is verified", {
  expect_snapshot(error = TRUE, {
    stop_index_incompatible_size(1, 2, ".i")
  })
})

# ------------------------------------------------------------------------------
# stop_generated_endpoints_cannot_be_na()

test_that("output is verified", {
  verify_output(
    test_path("output/test-stop-generated-endpoints-cannot-be-na-1.txt"),
    check_generated_endpoins_cannot_be_na(c(NA, 1, NA), ".before")
  )
})

# ------------------------------------------------------------------------------
# stop_endpoints_cannot_be_na()

test_that("output is verified", {
  verify_output(
    test_path("output/test-stop-endpoints-cannot-be-na-1.txt"),
    check_endpoints_cannot_be_na(c(NA, 1, NA), ".starts")
  )
})

# ------------------------------------------------------------------------------
# stop_index_cannot_be_na()

test_that("output is verified", {
  verify_output(
    test_path("output/test-stop-index-cannot-be-na-1.txt"),
    check_index_cannot_be_na(c(NA, 1, NA), ".i")
  )
})

test_that("trimming works", {
  verify_output(
    test_path("output/test-stop-index-cannot-be-na-2.txt"),
    check_index_cannot_be_na(rep(NA, 100), ".i")
  )
})

# ------------------------------------------------------------------------------
# stop_index_incompatible_size()

test_that("output is verified", {
  verify_output(
    test_path("output/test-stop-index-incompatible-size-1.txt"),
    stop_index_incompatible_size(1, 2, ".i")
  )
})

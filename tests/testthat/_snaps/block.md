# `x` must be a vector

    Code
      (expect_error(block(as.name("x"), new_date(0)), class = "vctrs_error_scalar_type")
      )
    Output
      <error/vctrs_error_scalar_type>
      Error in `block()`:
      ! `x` must be a vector, not a symbol.

# `i` can not have `NA` values

    Code
      (expect_error(block(1:2, new_date(c(0, NA_real_))), class = "slider_error_index_cannot_be_na")
      )
    Output
      <error/slider_error_index_cannot_be_na>
      Error in `stop_index_cannot_be_na()`:
      ! `i` cannot be `NA`.
      i It is `NA` at locations: 2.

# type of `i` is validated

    Code
      (expect_error(block(1, 1), class = "slider_error_index_incompatible_type"))
    Output
      <error/slider_error_index_incompatible_type>
      Error in `stop_index_incompatible_type()`:
      ! `i` has an incorrect type.
      x It must inherit from Date, POSIXct, or POSIXlt, not numeric.

# length of `i` must be identical to `x`

    Code
      (expect_error(block(c(1, 2), new_date(0)), class = "slider_error_index_incompatible_size")
      )
    Output
      <error/slider_error_index_incompatible_size>
      Error in `stop_index_incompatible_size()`:
      ! `i` has an incorrect size.
      x It must have size 2, not 1.

# `i` must be ascending

    Code
      (expect_error(block(c(1, 2, 3), new_date(c(2, 1, 0))), class = "slider_error_index_must_be_ascending")
      )
    Output
      <error/slider_error_index_must_be_ascending>
      Error in `stop_index_must_be_ascending()`:
      ! `i` must be in ascending order.
      i It is not ascending at locations: 3, 2.


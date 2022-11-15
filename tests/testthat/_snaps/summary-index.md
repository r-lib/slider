# input must be castable to logical

    Code
      (expect_error(slide_index_all(1:5, 1:5), class = "vctrs_error_cast_lossy"))
    Output
      <error/vctrs_error_cast_lossy>
      Error:
      ! Can't convert from <integer> to <logical> due to loss of precision.
      * Locations: 2, 3, 4, 5

---

    Code
      (expect_error(slide_index_any(1:5, 1:5), class = "vctrs_error_cast_lossy"))
    Output
      <error/vctrs_error_cast_lossy>
      Error:
      ! Can't convert from <integer> to <logical> due to loss of precision.
      * Locations: 2, 3, 4, 5

# x and i must be the same size

    Code
      (expect_error(slide_index_sum(1, 1:3), class = "slider_error_index_incompatible_size")
      )
    Output
      <error/slider_error_index_incompatible_size>
      Error in `slide_index_sum()`:
      ! `i` must have size 1, not 3.

# types that can't be cast to numeric are not supported

    Code
      (expect_error(slide_index_sum("x", 1), class = "vctrs_error_incompatible_type"))
    Output
      <error/vctrs_error_cast>
      Error:
      ! Can't convert <character> to <double>.

# arrays of dimensionality >1 are not supported

    Code
      (expect_error(slide_index_sum(array(1:4, dim = c(2, 2)), 1:2, before = 1),
      class = "vctrs_error_incompatible_type"))
    Output
      <error/vctrs_error_cast>
      Error:
      ! Can't convert <double[,2]> to <double>.
      Can't decrease dimensionality from 2 to 1.


# input must be castable to logical

    Code
      (expect_error(slide_all(1:5), class = "vctrs_error_cast_lossy"))
    Output
      <error/vctrs_error_cast_lossy>
      Error:
      ! Can't convert from <integer> to <logical> due to loss of precision.
      * Locations: 2, 3, 4, 5

---

    Code
      (expect_error(slide_any(1:5), class = "vctrs_error_cast_lossy"))
    Output
      <error/vctrs_error_cast_lossy>
      Error:
      ! Can't convert from <integer> to <logical> due to loss of precision.
      * Locations: 2, 3, 4, 5

# types that can't be cast to numeric are not supported

    Code
      (expect_error(slide_sum("x"), class = "vctrs_error_incompatible_type"))
    Output
      <error/vctrs_error_cast>
      Error:
      ! Can't convert <character> to <double>.

# arrays of dimensionality >1 are not supported

    Code
      (expect_error(slide_sum(array(1:4, dim = c(2, 2)), before = 1), class = "vctrs_error_incompatible_type")
      )
    Output
      <error/vctrs_error_cast>
      Error:
      ! Can't convert <double[,2]> to <double>.
      Can't decrease dimensionality from 2 to 1.


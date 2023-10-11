# `.x` must be a vector

    Code
      (expect_error(slide_period(call("fn")), class = "vctrs_error_scalar_type"))
    Output
      <error/vctrs_error_scalar_type>
      Error in `slide_period()`:
      ! `.x` must be a vector, not a call.

# .x must be the same size as .i

    Code
      (expect_error(slide_period(1, new_date(c(1, 2)), "year", identity), class = "slider_error_index_incompatible_size")
      )
    Output
      <error/slider_error_index_incompatible_size>
      Error in `slide_period()`:
      ! `.i` must have size 1, not 2.

# .i must be ascending

    Code
      (expect_error(slide_period(1:2, new_date(c(2, 1)), "year", identity), class = "slider_error_index_must_be_ascending")
      )
    Output
      <error/slider_error_index_must_be_ascending>
      Error in `slide_period()`:
      i In locations: 2
      ! `.i` must be in ascending order.

# empty input returns a list, but after the index size check

    Code
      (expect_error(slide_period(integer(), new_date(0), "year", ~.x), class = "slider_error_index_incompatible_size")
      )
    Output
      <error/slider_error_index_incompatible_size>
      Error in `slide_period()`:
      ! `.i` must have size 0, not 1.

# .i must not contain NA values

    Code
      (expect_error(slide_period(1:2, new_date(c(1, NA)), "year", identity), class = "slider_error_index_cannot_be_na")
      )
    Output
      <error/slider_error_index_cannot_be_na>
      Error in `slide_period()`:
      i In locations: 2
      ! `.i` can't be `NA`.
    Code
      (expect_error(slide_period(1:2, new_date(c(NA, 1)), "year", identity), class = "slider_error_index_cannot_be_na")
      )
    Output
      <error/slider_error_index_cannot_be_na>
      Error in `slide_period()`:
      i In locations: 1
      ! `.i` can't be `NA`.

# `.before` range cannot be after `.after` range

    Code
      slide_period(1:3, i, "month", identity, .before = -1)
    Condition
      Error:
      i In locations: 1, 2, and 3
      i In the ranges generated by `.before` and `.after`:
      ! The start of the range can't be after the end of the range.

# `.before` cannot be NA

    Code
      slide_period(1, new_date(0), "year", identity, .before = NA_integer_)
    Condition
      Error in `slide_period()`:
      ! `.before` can't be `NA`.

# `.before` cannot be -Inf

    Code
      (expect_error(slide_period(1, new_date(0), "year", identity, .before = -Inf),
      class = "vctrs_error_cast_lossy"))
    Output
      <error/vctrs_error_cast_lossy>
      Error in `slide_period()`:
      ! Can't convert from `.before` <double> to <integer> due to loss of precision.
      * Locations: 1

# .before must be size 1

    Code
      expect_error(slide_period(1, new_date(0), "year", identity, .before = c(1L, 2L)),
      class = "vctrs_error_assert_size")

# error if .before is NULL

    Code
      expect_error(slide_period(1, new_date(0), "year", identity, .before = NULL),
      class = "vctrs_error_scalar_type")

# `.after` range cannot be before `.before` range

    Code
      slide_period(1:3, i, "month", identity, .after = -1)
    Condition
      Error:
      i In locations: 1, 2, and 3
      i In the ranges generated by `.before` and `.after`:
      ! The start of the range can't be after the end of the range.

# `.after` cannot be NA

    Code
      slide_period(1, new_date(0), "year", identity, .after = NA_integer_)
    Condition
      Error in `slide_period()`:
      ! `.after` can't be `NA`.

# `.after` cannot be -Inf

    Code
      (expect_error(slide_period(1, new_date(0), "year", identity, .after = -Inf),
      class = "vctrs_error_cast_lossy"))
    Output
      <error/vctrs_error_cast_lossy>
      Error in `slide_period()`:
      ! Can't convert from `.after` <double> to <integer> due to loss of precision.
      * Locations: 1

# .after must be size 1

    Code
      (expect_error(slide_period(1, new_date(0), "year", identity, .after = c(1L, 2L)),
      class = "vctrs_error_assert_size"))
    Output
      <error/vctrs_error_assert_size>
      Error in `slide_period()`:
      ! `.after` must have size 1, not size 2.

# error if .after is NULL

    Code
      (expect_error(slide_period(1, new_date(0), "year", identity, .after = NULL),
      class = "vctrs_error_scalar_type"))
    Output
      <error/vctrs_error_scalar_type>
      Error in `slide_period()`:
      ! `.after` must be a vector, not `NULL`.

# `.complete` cannot be NA

    Code
      slide_period(1, new_date(0), "year", identity, .complete = NA)
    Condition
      Error in `slide_period()`:
      ! `.complete` can't be `NA`.

# .complete must be size 1

    Code
      expect_error(slide_period(1, new_date(0), "year", identity, .complete = c(TRUE,
        FALSE)), class = "vctrs_error_assert_size")

# error if .complete is NULL

    Code
      expect_error(slide_period(1, new_date(0), "year", identity, .complete = NULL),
      class = "vctrs_error_scalar_type")

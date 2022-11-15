# .x must be the same size as .i

    Code
      (expect_error(hop_index(1, 1:2, 1, 1, identity), class = "slider_error_index_incompatible_size")
      )
    Output
      <error/slider_error_index_incompatible_size>
      Error in `hop_index()`:
      ! `.i` has an incorrect size.
      x It must have size 1, not 2.

# .i must be ascending

    Code
      (expect_error(hop_index(1:2, 2:1, 1:2, 1:2, identity), class = "slider_error_index_must_be_ascending")
      )
    Output
      <error/slider_error_index_must_be_ascending>
      Error in `hop_index()`:
      ! `.i` must be in ascending order.
      i It is not ascending at locations: 2.

# .starts must be ascending

    Code
      (expect_error(hop_index(1:2, 1:2, 2:1, 1:2, identity), class = "slider_error_endpoints_must_be_ascending")
      )
    Output
      <error/slider_error_endpoints_must_be_ascending>
      Error in `hop_index()`:
      ! `.starts` must be in ascending order.
      i It is not ascending at locations: 2.

# .stops must be ascending

    Code
      (expect_error(hop_index(1:2, 1:2, 1:2, 2:1, identity), class = "slider_error_endpoints_must_be_ascending")
      )
    Output
      <error/slider_error_endpoints_must_be_ascending>
      Error in `hop_index()`:
      ! `.stops` must be in ascending order.
      i It is not ascending at locations: 2.

# empty `.x` and `.i`, but size `n > 0` `.starts` and `.stops`: sizes and types are checked first

    Code
      (expect_error(hop_index(integer(), integer(), 1:3, 1:2, ~.x), class = "vctrs_error_incompatible_size")
      )
    Output
      <error/vctrs_error_incompatible_size>
      Error in `hop_index()`:
      ! Can't recycle `.starts` (size 3) to match `.stops` (size 2).
    Code
      (expect_error(hop_index(integer(), integer(), 1, "x", ~.x), class = "vctrs_error_incompatible_type")
      )
    Output
      <error/vctrs_error_cast>
      Error in `hop_index()`:
      ! Can't convert `.stops` <character> to match type of `.i` <integer>.

# .i must not contain NA values

    Code
      (expect_error(hop_index(1:2, c(1, NA), 1:2, 1:2, identity), class = "slider_error_index_cannot_be_na")
      )
    Output
      <error/slider_error_index_cannot_be_na>
      Error in `hop_index()`:
      ! `.i` cannot be `NA`.
      i It is `NA` at locations: 2.
    Code
      (expect_error(hop_index(1:2, c(NA, 1), 1:2, 1:2, identity), class = "slider_error_index_cannot_be_na")
      )
    Output
      <error/slider_error_index_cannot_be_na>
      Error in `hop_index()`:
      ! `.i` cannot be `NA`.
      i It is `NA` at locations: 1.

# .starts must not contain NA values

    Code
      (expect_error(hop_index(1:2, 1:2, c(1, NA), 1:2, identity), class = "slider_error_endpoints_cannot_be_na")
      )
    Output
      <error/slider_error_endpoints_cannot_be_na>
      Error in `hop_index()`:
      ! `.starts` cannot be `NA`.
      i It is `NA` at locations: 2.
    Code
      (expect_error(hop_index(1:2, 1:2, c(NA, 1), 1:2, identity), class = "slider_error_endpoints_cannot_be_na")
      )
    Output
      <error/slider_error_endpoints_cannot_be_na>
      Error in `hop_index()`:
      ! `.starts` cannot be `NA`.
      i It is `NA` at locations: 1.

# .stops must not contain NA values

    Code
      (expect_error(hop_index(1:2, 1:2, 1:2, c(1, NA), identity), class = "slider_error_endpoints_cannot_be_na")
      )
    Output
      <error/slider_error_endpoints_cannot_be_na>
      Error in `hop_index()`:
      ! `.stops` cannot be `NA`.
      i It is `NA` at locations: 2.
    Code
      (expect_error(hop_index(1:2, 1:2, 1:2, c(NA, 1), identity), class = "slider_error_endpoints_cannot_be_na")
      )
    Output
      <error/slider_error_endpoints_cannot_be_na>
      Error in `hop_index()`:
      ! `.stops` cannot be `NA`.
      i It is `NA` at locations: 1.

# recycling is used for .starts/.stops

    Code
      (expect_error(hop_index(1:2, 1:2, 1:2, 1:3, ~.x), class = "vctrs_error_incompatible_size")
      )
    Output
      <error/vctrs_error_incompatible_size>
      Error in `hop_index()`:
      ! Can't recycle `.starts` (size 2) to match `.stops` (size 3).

# .starts and .stops are cast to .i

    Code
      (expect_error(hop_index(1:2, i, starts, stops, ~.x), class = "vctrs_error_incompatible_type")
      )
    Output
      <error/vctrs_error_cast>
      Error in `hop_index()`:
      ! Can't convert `.starts` <character> to match type of `.i` <date>.


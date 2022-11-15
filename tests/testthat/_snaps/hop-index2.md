# empty input returns a list, but after the index size check

    Code
      (expect_error(hop_index2(.x = integer(), .y = integer(), .i = 1, .starts = integer(),
      .stops = integer(), .f = ~.x), class = "slider_error_index_incompatible_size"))
    Output
      <error/slider_error_index_incompatible_size>
      Error in `hop_index2()`:
      ! `.i` must have size 0, not 1.

# empty `.x` and `.y` and `.i`, but size `n > 0` `.starts` and `.stops`: sizes and types are checked first

    Code
      (expect_error(hop_index2(integer(), integer(), integer(), 1:3, 1:2, ~.x),
      class = "vctrs_error_incompatible_size"))
    Output
      <error/vctrs_error_incompatible_size>
      Error in `hop_index2()`:
      ! Can't recycle `.starts` (size 3) to match `.stops` (size 2).
    Code
      (expect_error(hop_index2(integer(), integer(), integer(), 1, "x", ~.x), class = "vctrs_error_incompatible_type")
      )
    Output
      <error/vctrs_error_cast>
      Error in `hop_index2()`:
      ! Can't convert `.stops` <character> to match type of `.i` <integer>.


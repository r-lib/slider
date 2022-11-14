# empty `.l` and `.i`, but size `n > 0` `.starts` and `.stops`: sizes and types are checked first

    Code
      (expect_error(phop_index(list(), integer(), 1:3, 1:2, ~.x), class = "vctrs_error_incompatible_size")
      )
    Output
      <error/vctrs_error_incompatible_size>
      Error in `phop_index()`:
      ! Can't recycle `.starts` (size 3) to match `.stops` (size 2).
    Code
      (expect_error(phop_index(list(), integer(), 1, "x", ~.x), class = "vctrs_error_incompatible_type")
      )
    Output
      <error/vctrs_error_cast>
      Error in `phop_index()`:
      ! Can't convert `.stops` <character> to match type of `.i` <integer>.


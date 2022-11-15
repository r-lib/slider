# empty input returns a list, but after the index size check

    Code
      (expect_error(pslide_period(list(integer(), integer()), i, "day", ~.x), class = "slider_error_index_incompatible_size")
      )
    Output
      <error/slider_error_index_incompatible_size>
      Error in `pslide_period()`:
      ! `.i` must have size 0, not 1.


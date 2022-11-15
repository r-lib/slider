# empty input returns a list, but after the index size check

    Code
      (expect_error(slide_period2(.x = integer(), .y = integer(), .i = structure(0,
        class = "Date"), .period = "day", .f = ~.x), class = "slider_error_index_incompatible_size")
      )
    Output
      <error/slider_error_index_incompatible_size>
      Error in `slide_period2()`:
      ! `.i` must have size 0, not 1.


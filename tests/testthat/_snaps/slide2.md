# Recycling is carried out using tidyverse recycling rules

    Code
      (expect_error(slide2(x0, x2, ~.x), class = "vctrs_error_incompatible_size"))
    Output
      <error/vctrs_error_incompatible_size>
      Error in `slide2_impl()`:
      ! Can't recycle `..1` (size 0) to match `..2` (size 2).
    Code
      (expect_error(slide2(x2, x3, ~.x), class = "vctrs_error_incompatible_size"))
    Output
      <error/vctrs_error_incompatible_size>
      Error in `slide2_impl()`:
      ! Can't recycle `..1` (size 2) to match `..2` (size 3).


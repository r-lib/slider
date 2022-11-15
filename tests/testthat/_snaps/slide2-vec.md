# slide2_vec() errors if it can't simplify

    Code
      (expect_error(slide2_vec(1:2, 1:2, fn, .ptype = NULL), class = "vctrs_error_incompatible_type")
      )
    Output
      <error/vctrs_error_ptype2>
      Error in `slide2_vec()`:
      ! Can't combine `out[[1]]` <double> and `out[[2]]` <character>.

# slide2_*() errors if it can't cast

    Code
      (expect_error(slide2_int(1:2, 1:2, fn), class = "vctrs_error_incompatible_type")
      )
    Output
      <error/vctrs_error_cast>
      Error:
      ! Can't convert <character> to <integer>.

# slide2_chr() cannot coerce

    Code
      (expect_error(slide2_chr(1, 1, ~ .x + .y), class = "vctrs_error_incompatible_type")
      )
    Output
      <error/vctrs_error_cast>
      Error:
      ! Can't convert <double> to <character>.


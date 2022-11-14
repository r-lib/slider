# pslide_vec() errors if it can't simplify

    Code
      (expect_error(pslide_vec(list(1:2, 1:2), fn, .ptype = NULL), class = "vctrs_error_incompatible_type")
      )
    Output
      <error/vctrs_error_ptype2>
      Error in `pslide_vec()`:
      ! Can't combine `out[[1]]` <double> and `out[[2]]` <character>.

# pslide_*() errors if it can't cast

    Code
      (expect_error(pslide_int(list(1:2, 1:2), fn), class = "vctrs_error_incompatible_type")
      )
    Output
      <error/vctrs_error_cast>
      Error:
      ! Can't convert <character> to <integer>.

# pslide_chr() cannot coerce

    Code
      (expect_error(pslide_chr(list(1, 1), ~ .x + .y), class = "vctrs_error_incompatible_type")
      )
    Output
      <error/vctrs_error_cast>
      Error:
      ! Can't convert <double> to <character>.


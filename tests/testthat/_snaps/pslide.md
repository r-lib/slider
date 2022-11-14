# Recycling is carried out using tidyverse recycling rules

    Code
      (expect_error(pslide(list(x0, x2), ~.x), class = "vctrs_error_incompatible_size")
      )
    Output
      <error/vctrs_error_incompatible_size>
      Error in `pslide()`:
      ! Can't recycle `.l[[1]]` (size 0) to match `.l[[2]]` (size 2).

---

    Code
      (expect_error(pslide(list(x2, x3), ~.x), class = "vctrs_error_incompatible_size")
      )
    Output
      <error/vctrs_error_incompatible_size>
      Error in `pslide()`:
      ! Can't recycle `.l[[1]]` (size 2) to match `.l[[2]]` (size 3).

# pslide() requires a list-like input

    Code
      pslide(1:5, ~.x)
    Condition
      Error in `pslide()`:
      ! `.l` must be a list, not an integer vector.


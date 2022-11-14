# Recycling is carried out using tidyverse recycling rules

    Code
      (expect_error(phop(list(x0, x2), 1, 1, ~.x), class = "vctrs_error_incompatible_size")
      )
    Output
      <error/vctrs_error_incompatible_size>
      Error in `phop()`:
      ! Can't recycle `.l[[1]]` (size 0) to match `.l[[2]]` (size 2).

---

    Code
      (expect_error(phop(list(x2, x3), 1:3, 1:3, ~.x), class = "vctrs_error_incompatible_size")
      )
    Output
      <error/vctrs_error_incompatible_size>
      Error in `phop()`:
      ! Can't recycle `.l[[1]]` (size 2) to match `.l[[2]]` (size 3).

# phop() requires a list-like input

    Code
      phop(1:5, ~.x)
    Condition
      Error in `phop()`:
      ! `.l` must be a list, not an integer vector.


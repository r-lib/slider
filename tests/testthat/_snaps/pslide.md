# pslide() requires a list-like input

    Code
      pslide(1:5, ~.x)
    Condition
      Error in `pslide_impl()`:
      ! `.l` must be a list, not integer.


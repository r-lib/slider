# pslide() requires a list-like input

    Code
      pslide(1:5, ~.x)
    Condition
      Error in `check_is_list()`:
      ! `.l` must be a list, not integer.


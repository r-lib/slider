# phop() requires a list-like input

    Code
      phop(1:5, ~.x)
    Condition
      Error in `check_is_list()`:
      ! `.l` must be a list, not integer.


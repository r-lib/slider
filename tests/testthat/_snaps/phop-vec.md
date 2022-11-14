# `.ptype = NULL` validates that element lengths are 1

    Code
      (expect_error(phop_vec(list(1:2, 1:2), 1:2, 1:2, ~ if (.x == 1L) {
        1:2
      } else {
        1
      }, .ptype = NULL)))
    Output
      <error/rlang_error>
      Error in `stop_not_all_size_one()`:
      ! In iteration 1, the result of `.f` had size 2, not 1.
    Code
      (expect_error(phop_vec(list(1:2, 1:2), 1:2, 1:2, ~ if (.x == 1L) {
        NULL
      } else {
        2
      }, .ptype = NULL)))
    Output
      <error/rlang_error>
      Error in `stop_not_all_size_one()`:
      ! In iteration 1, the result of `.f` had size 0, not 1.


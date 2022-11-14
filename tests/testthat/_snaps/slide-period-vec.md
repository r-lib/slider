# size of each `.f` result must be 1

    Code
      (expect_error(slide_period_vec(1:2, new_date(c(1, 2)), "day", ~ c(.x, 1))))
    Output
      <error/rlang_error>
      Error in `stop_not_all_size_one()`:
      ! In iteration 1, the result of `.f` had size 2, not 1.
    Code
      (expect_error(slide_period_dbl(1:2, new_date(c(1, 2)), "day", ~ c(.x, 1))))
    Output
      <error/rlang_error>
      Error in `stop_not_all_size_one()`:
      ! In iteration 1, the result of `.f` had size 2, not 1.

# `.ptype = NULL` validates that element lengths are 1

    Code
      (expect_error(slide_period_vec(1:2, new_date(c(0, 1)), "day", ~ if (.x == 1L) {
        1:2
      } else {
        1
      }, .ptype = NULL)))
    Output
      <error/rlang_error>
      Error in `stop_not_all_size_one()`:
      ! In iteration 1, the result of `.f` had size 2, not 1.
    Code
      (expect_error(slide_period_vec(1:2, new_date(c(0, 1)), "day", ~ if (.x == 1L) {
        NULL
      } else {
        1
      }, .ptype = NULL)))
    Output
      <error/rlang_error>
      Error in `stop_not_all_size_one()`:
      ! In iteration 1, the result of `.f` had size 0, not 1.


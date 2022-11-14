# size of each `.f` result must be 1

    Code
      (expect_error(hop_index_vec(1, 1, 1, 1, ~ c(.x, 1))))
    Output
      <error/rlang_error>
      Error in `stop_not_all_size_one()`:
      ! In iteration 1, the result of `.f` had size 2, not 1.

# inner type can be restricted with list_of

    Code
      (expect_error(hop_index_vec(1:2, 1:2, 1:2, 1:2, ~ if (.x == 1L) {
        list_of(1)
      } else {
        list_of("hi")
      }, .ptype = list_of(.ptype = double())), class = "vctrs_error_incompatible_type")
      )
    Output
      <error/vctrs_error_cast>
      Error in `list_unchop()`:
      ! Can't convert `..1` <character> to <double>.

# `.ptype = NULL` validates that element lengths are 1

    Code
      (expect_error(hop_index_vec(1:2, 1:2, 1:2, 1:2, ~ if (.x == 1L) {
        1:2
      } else {
        1
      }, .ptype = NULL)))
    Output
      <error/rlang_error>
      Error in `stop_not_all_size_one()`:
      ! In iteration 1, the result of `.f` had size 2, not 1.
    Code
      (expect_error(hop_index_vec(1:2, 1:2, 1:2, 1:2, ~ if (.x == 1L) {
        NULL
      } else {
        2
      }, .ptype = NULL)))
    Output
      <error/rlang_error>
      Error in `stop_not_all_size_one()`:
      ! In iteration 1, the result of `.f` had size 0, not 1.


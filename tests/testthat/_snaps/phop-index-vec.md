# phop_index_vec() errors if it can't simplify

    Code
      (expect_error(phop_index_vec(list(1:2, 1:2), 1:2, 1:2, 1:2, fn, .ptype = NULL),
      class = "vctrs_error_incompatible_type"))
    Output
      <error/vctrs_error_ptype2>
      Error in `phop_index_vec()`:
      ! Can't combine `out[[1]]` <double> and `out[[2]]` <character>.

# `.ptype = NULL` validates that element lengths are 1

    Code
      phop_index_vec(list(1:2, 1:2), 1:2, 1:2, 1:2, ~ if (.x == 1L) {
        1:2
      } else {
        1
      }, .ptype = NULL)
    Condition
      Error:
      i In index: 1
      ! The result of `.f` must have size 1, not 2.

---

    Code
      phop_index_vec(list(1:2, 1:2), 1:2, 1:2, 1:2, ~ if (.x == 1L) {
        NULL
      } else {
        2
      }, .ptype = NULL)
    Condition
      Error:
      i In index: 1
      ! The result of `.f` must have size 1, not 0.


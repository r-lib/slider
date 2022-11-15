# hop2_vec() errors if it can't simplify

    Code
      hop2_vec(1:2, 1:2, 1:2, 1:2, fn, .ptype = NULL)
    Condition
      Error in `hop2_vec()`:
      ! Can't combine `out[[1]]` <double> and `out[[2]]` <character>.

# `.ptype = NULL` validates that element lengths are 1

    Code
      hop2_vec(1:2, 1:2, 1:2, 1:2, ~ if (.x == 1L) {
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
      hop2_vec(1:2, 1:2, 1:2, 1:2, ~ if (.x == 1L) {
        NULL
      } else {
        2
      }, .ptype = NULL)
    Condition
      Error:
      i In index: 1
      ! The result of `.f` must have size 1, not 0.


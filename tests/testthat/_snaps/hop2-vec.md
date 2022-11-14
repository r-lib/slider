# hop2_vec() errors if it can't simplify

    Code
      hop2_vec(1:2, 1:2, 1:2, 1:2, fn, .ptype = NULL)
    Condition
      Error in `list_unchop()`:
      ! Can't combine `x[[1]]` <double> and `x[[2]]` <character>.

# `.ptype = NULL` validates that element lengths are 1

    Code
      hop2_vec(1:2, 1:2, 1:2, 1:2, ~ if (.x == 1L) {
        1:2
      } else {
        1
      }, .ptype = NULL)
    Condition
      Error:
      ! In iteration 1, the result of `.f` had size 2, not 1.

---

    Code
      hop2_vec(1:2, 1:2, 1:2, 1:2, ~ if (.x == 1L) {
        NULL
      } else {
        2
      }, .ptype = NULL)
    Condition
      Error:
      ! In iteration 1, the result of `.f` had size 0, not 1.


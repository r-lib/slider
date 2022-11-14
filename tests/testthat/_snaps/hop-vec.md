# size of each `.f` result must be 1

    Code
      hop_vec(1:2, 1, 1, ~ c(.x, 1))
    Condition
      Error in `stop_not_all_size_one()`:
      ! In iteration 1, the result of `.f` had size 2, not 1.

# `.ptype = NULL` validates that element lengths are 1

    Code
      hop_vec(1:2, 1:2, 1:2, ~ if (.x == 1L) {
        1:2
      } else {
        1
      }, .ptype = NULL)
    Condition
      Error in `stop_not_all_size_one()`:
      ! In iteration 1, the result of `.f` had size 2, not 1.

---

    Code
      hop_vec(1:2, 1:2, 1:2, ~ if (.x == 1L) {
        NULL
      } else {
        2
      }, .ptype = NULL)
    Condition
      Error in `stop_not_all_size_one()`:
      ! In iteration 1, the result of `.f` had size 0, not 1.


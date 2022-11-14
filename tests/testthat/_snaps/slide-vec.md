# size of each `.f` result must be 1

    Code
      slide_vec(1:2, ~ c(.x, 1))
    Condition
      Error in `stop_not_all_size_one()`:
      ! In iteration 1, the result of `.f` had size 2, not 1.

---

    Code
      slide_dbl(1:2, ~ c(.x, 1))
    Condition
      Error in `stop_not_all_size_one()`:
      ! In iteration 1, the result of `.f` had size 2, not 1.

# `.ptype = NULL` validates that element lengths are 1

    Code
      slide_vec(1:2, ~ if (.x == 1L) {
        1:2
      } else {
        1
      }, .ptype = NULL)
    Condition
      Error in `stop_not_all_size_one()`:
      ! In iteration 1, the result of `.f` had size 2, not 1.


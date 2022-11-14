# size of each `.f` result must be 1

    Code
      slide_period2_vec(1:2, 1:2, new_date(c(1, 2)), "day", ~ c(.x, .y))
    Condition
      Error:
      ! In iteration 1, the result of `.f` had size 2, not 1.

---

    Code
      slide_period2_int(1:2, 1:2, new_date(c(1, 2)), "day", ~ c(.x, .y))
    Condition
      Error:
      ! In iteration 1, the result of `.f` had size 2, not 1.

# `.ptype = NULL` validates that element lengths are 1

    Code
      slide_period2_vec(1:2, 1:2, new_date(c(0, 1)), "day", ~ if (.x == 1L) {
        1:2
      } else {
        1
      }, .ptype = NULL)
    Condition
      Error:
      ! In iteration 1, the result of `.f` had size 2, not 1.

---

    Code
      slide_period2_vec(1:2, 1:2, new_date(c(0, 1)), "day", ~ if (.x == 1L) {
        NULL
      } else {
        1
      }, .ptype = NULL)
    Condition
      Error:
      ! In iteration 1, the result of `.f` had size 0, not 1.

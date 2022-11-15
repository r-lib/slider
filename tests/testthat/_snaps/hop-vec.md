# size of each `.f` result must be 1

    Code
      hop_vec(1:2, 1, 1, ~ c(.x, 1))
    Condition
      Error:
      i In index: 1
      ! The result of `.f` must have size 1, not 2.

# inner type can be restricted with list_of

    Code
      (expect_error(hop_vec(1:2, 1:2, 1:2, ~ if (.x == 1L) {
        list_of(1)
      } else {
        list_of("hi")
      }, .ptype = list_of(.ptype = double())), class = "vctrs_error_incompatible_type")
      )
    Output
      <error/vctrs_error_cast>
      Error in `hop_vec()`:
      ! Can't convert `..1` <character> to <double>.

# `.ptype = NULL` fails if no common type is found

    Code
      (expect_error(hop_vec(1:2, 1:2, 1:2, ~ ifelse(.x == 1L, "hello", 1), .ptype = NULL),
      class = "vctrs_error_incompatible_type"))
    Output
      <error/vctrs_error_ptype2>
      Error in `hop_vec()`:
      ! Can't combine `out[[1]]` <character> and `out[[2]]` <double>.

# `.ptype = NULL` validates that element lengths are 1

    Code
      hop_vec(1:2, 1:2, 1:2, ~ if (.x == 1L) {
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
      hop_vec(1:2, 1:2, 1:2, ~ if (.x == 1L) {
        NULL
      } else {
        2
      }, .ptype = NULL)
    Condition
      Error:
      i In index: 1
      ! The result of `.f` must have size 1, not 0.


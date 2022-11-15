# size of each `.f` result must be 1

    Code
      slide_period2_vec(1:2, 1:2, new_date(c(1, 2)), "day", ~ c(.x, .y))
    Condition
      Error:
      i In index: 1
      ! The result of `.f` must have size 1, not 2.

---

    Code
      slide_period2_int(1:2, 1:2, new_date(c(1, 2)), "day", ~ c(.x, .y))
    Condition
      Error:
      i In index: 1
      ! The result of `.f` must have size 1, not 2.

# inner type can be restricted with list_of

    Code
      (expect_error(slide_period2_vec(1:2, 1:2, new_date(c(1, 2)), "day", ~ if (.x ==
        1L) {
        list_of(1)
      } else {
        list_of("hi")
      }, .ptype = list_of(.ptype = double())), class = "vctrs_error_incompatible_type")
      )
    Output
      <error/vctrs_error_cast>
      Error in `slide_period2_vec()`:
      ! Can't convert `..1` <character> to <double>.

# type can be restricted

    Code
      (expect_error(slide_period2_int(1:2, 1:2, new_date(c(1, 2)), "day", ~ if (.x ==
        1L) {
        1L
      } else {
        "hi"
      }), class = "vctrs_error_incompatible_type"))
    Output
      <error/vctrs_error_cast>
      Error:
      ! Can't convert <character> to <integer>.

# .ptype is respected

    Code
      (expect_error(slide_period2_vec(1, 1, new_date(0), "day", ~ .x + 0.5, .ptype = integer()),
      class = "vctrs_error_cast_lossy"))
    Output
      <error/vctrs_error_cast_lossy>
      Error in `slide_period2_vec()`:
      ! Can't convert from `out[[1]]` <double> to <integer> due to loss of precision.
      * Locations: 1

# `.ptype = NULL` fails if no common type is found

    Code
      (expect_error(slide_period2_vec(1:2, 1:2, new_date(c(0, 1)), "day", ~ ifelse(
        .x == 1L, "hello", 1), .ptype = NULL), class = "vctrs_error_incompatible_type")
      )
    Output
      <error/vctrs_error_ptype2>
      Error in `slide_period2_vec()`:
      ! Can't combine `out[[1]]` <character> and `out[[2]]` <double>.

# `.ptype = NULL` validates that element lengths are 1

    Code
      slide_period2_vec(1:2, 1:2, new_date(c(0, 1)), "day", ~ if (.x == 1L) {
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
      slide_period2_vec(1:2, 1:2, new_date(c(0, 1)), "day", ~ if (.x == 1L) {
        NULL
      } else {
        1
      }, .ptype = NULL)
    Condition
      Error:
      i In index: 1
      ! The result of `.f` must have size 1, not 0.

# slide_period2_chr() cannot coerce

    Code
      (expect_error(slide_period2_chr(1, 1, new_date(0), "day", ~.x), class = "vctrs_error_incompatible_type")
      )
    Output
      <error/vctrs_error_cast>
      Error:
      ! Can't convert <double> to <character>.


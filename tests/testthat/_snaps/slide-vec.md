# size of each `.f` result must be 1

    Code
      slide_vec(1:2, ~ c(.x, 1))
    Condition
      Error:
      i In index: 1
      ! The result of `.f` must have size 1, not 2.

---

    Code
      slide_dbl(1:2, ~ c(.x, 1))
    Condition
      Error:
      i In index: 1
      ! The result of `.f` must have size 1, not 2.

# inner type can be restricted with list_of

    Code
      (expect_error(slide_vec(1:2, ~ if (.x == 1L) {
        list_of(1)
      } else {
        list_of("hi")
      }, .ptype = list_of(.ptype = double())), class = "vctrs_error_incompatible_type")
      )
    Output
      <error/vctrs_error_cast>
      Error in `slide_vec()`:
      ! Can't convert `..1` <character> to <double>.

# inner type can be restricted

    Code
      (expect_error(slide_dbl(1:2, ~ if (.x == 1L) {
        1
      } else {
        "x"
      }), class = "vctrs_error_incompatible_type"))
    Output
      <error/vctrs_error_cast>
      Error:
      ! Can't convert <character> to <double>.

# .ptype is respected

    Code
      (expect_error(slide_vec(1, ~ .x + 0.5, .ptype = integer()), class = "vctrs_error_cast_lossy")
      )
    Output
      <error/vctrs_error_cast_lossy>
      Error in `slide_vec()`:
      ! Can't convert from `out[[1]]` <double> to <integer> due to loss of precision.
      * Locations: 1

# `.ptype = NULL` fails if no common type is found

    Code
      (expect_error(slide_vec(1:2, ~ ifelse(.x == 1L, "hello", 1), .ptype = NULL),
      class = "vctrs_error_incompatible_type"))
    Output
      <error/vctrs_error_ptype2>
      Error in `slide_vec()`:
      ! Can't combine `out[[1]]` <character> and `out[[2]]` <double>.

# `.ptype = NULL` validates that element lengths are 1

    Code
      slide_vec(1:2, ~ if (.x == 1L) {
        1:2
      } else {
        1
      }, .ptype = NULL)
    Condition
      Error:
      i In index: 1
      ! The result of `.f` must have size 1, not 2.

# slide_chr() cannot coerce

    Code
      (expect_error(slide_chr(1, ~.x), class = "vctrs_error_incompatible_type"))
    Output
      <error/vctrs_error_cast>
      Error:
      ! Can't convert <double> to <character>.


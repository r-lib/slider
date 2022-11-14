# negative .before errors if its absolute value is past .after

    Code
      slide_index(x, i, identity, .before = -1, .after = 0)
    Condition
      Error:
      ! In the ranges generated by `.before` and `.after`, the start of the range is after the end of the range at location(s): 1, 2, 3, 4.

# errors if negative .before Duration is further than .after

    Code
      slide_index(x, i, identity, .before = -lubridate::ddays(1), .after = 0)
    Condition
      Error:
      ! In the ranges generated by `.before` and `.after`, the start of the range is after the end of the range at location(s): 1, 2, 3, 4.

# negative .after errors if its absolute value is past .before

    Code
      slide_index(x, i, identity, .after = -1, .before = 0)
    Condition
      Error:
      ! In the ranges generated by `.before` and `.after`, the start of the range is after the end of the range at location(s): 1, 2, 3, 4.

# .before/.after - generated endpoints must be in weakly ascending order

    Code
      (expect_error(slide_index(x, x, identity, .before = ~ .x - c(2, 4)), class = "slider_error_generated_endpoints_must_be_ascending")
      )
    Output
      <error/slider_error_generated_endpoints_must_be_ascending>
      Error in `compute_ranges()`:
      ! Endpoints generated by `.before` must be in ascending order.
      i They are not ascending at locations: 2.
    Code
      (expect_error(slide_index(x, x, identity, .after = ~ .x + c(4, 2)), class = "slider_error_generated_endpoints_must_be_ascending")
      )
    Output
      <error/slider_error_generated_endpoints_must_be_ascending>
      Error in `compute_ranges()`:
      ! Endpoints generated by `.after` must be in ascending order.
      i They are not ascending at locations: 2.

# .before/.after - generated endpoints must maintain .before <= .after ordering

    In the ranges generated by `.before` and `.after`, the start of the range is after the end of the range at location(s): 1, 2.

---

    In the ranges generated by `.before` and `.after`, the start of the range is after the end of the range at location(s): 1, 2.

# .before/.after - generated endpoints can't be NA

    Endpoints generated by `.before` cannot be `NA`.
    i They are `NA` at locations: 1, 2.

---

    Endpoints generated by `.after` cannot be `NA`.
    i They are `NA` at locations: 1, 2.

# .before/.after - generated endpoints shouldn't rely on original `.i` length

    Code
      (expect_error(slide_index(x, x, identity, .before = ~ .x - adjust), class = "slider_error_generated_endpoints_incompatible_size")
      )
    Output
      <error/slider_error_generated_endpoints_incompatible_size>
      Error in `compute_ranges()`:
      ! Endpoints generated by `.before` have an incorrect size.
      i They must have size 1, not 2.
    Code
      (expect_error(slide_index(x, x, identity, .after = ~ .x + adjust), class = "slider_error_generated_endpoints_incompatible_size")
      )
    Output
      <error/slider_error_generated_endpoints_incompatible_size>
      Error in `compute_ranges()`:
      ! Endpoints generated by `.after` have an incorrect size.
      i They must have size 1, not 2.

# output is verified

    Code
      check_index_incompatible_type(1, ".i")
    Condition
      Error:
      ! `.i` must be a <Date>, <POSIXct>, or <POSIXlt>, not a number.

---

    Code
      check_endpoints_must_be_ascending(c(1, 2, 1, 3, 4, 2), ".starts")
    Condition
      Error:
      i In locations: 3 and 6
      ! `.starts` must be in ascending order.

---

    Code
      check_generated_endpoints_cannot_be_na(c(NA, 1, NA), ".before")
    Condition
      Error:
      i In locations: 1 and 3
      ! Endpoints generated by `.before` can't be `NA`.

---

    Code
      check_endpoints_cannot_be_na(c(NA, 1, NA), ".starts")
    Condition
      Error:
      i In locations: 1 and 3
      ! `.starts` can't be `NA`.

---

    Code
      check_index_must_be_ascending(c(1, 2, 1, 4, 5, 3), ".i")
    Condition
      Error:
      i In locations: 3 and 6
      ! `.i` must be in ascending order.

---

    Code
      check_index_cannot_be_na(c(NA, 1, NA), ".i")
    Condition
      Error:
      i In locations: 1 and 3
      ! `.i` can't be `NA`.

---

    Code
      stop_index_incompatible_size(1, 2, ".i")
    Condition
      Error:
      ! `.i` must have size 2, not 1.

# class names are collapsed

    Code
      check_index_incompatible_type(x, ".i")
    Condition
      Error:
      ! `.i` must be a <Date>, <POSIXct>, or <POSIXlt>, not a <foo/bar/baz> object.

# trimming works

    Code
      check_index_cannot_be_na(rep(NA, 100), ".i")
    Condition
      Error:
      i In locations: 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, ..., 99, and 100
      ! `.i` can't be `NA`.

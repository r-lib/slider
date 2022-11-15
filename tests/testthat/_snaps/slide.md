# error if negative .before's abs() is > .after

    Code
      slide(1:5, identity, .before = -1)
    Condition
      Error:
      ! When `.before` (-1) is negative, its absolute value (1) can't be greater than `.after` (0).

# both .before and .after cannot be negative

    Code
      slide(1:5, identity, .before = -1, .after = -1)
    Condition
      Error:
      ! `.before` (-1) and `.after` (-1) can't both be negative.

# error if negative .after's abs() is > .before

    Code
      slide(1:5, identity, .after = -1)
    Condition
      Error:
      ! When `.after` (-1) is negative, its absolute value (1) can't be greater than `.before` (0).

# cannot use invalid .before

    Code
      slide(1, identity, .before = c(1, 2))
    Condition
      Error:
      ! `.before` must have size 1, not 2.

---

    Code
      (expect_error(slide(1, identity, .before = "x"), class = "vctrs_error_incompatible_type")
      )
    Output
      <error/vctrs_error_cast>
      Error:
      ! Can't convert <character> to <integer>.

# cannot use invalid .after

    Code
      slide(1, identity, .after = c(1, 2))
    Condition
      Error:
      ! `.after` must have size 1, not 2.

---

    Code
      (expect_error(slide(1, identity, .after = "x"), class = "vctrs_error_incompatible_type")
      )
    Output
      <error/vctrs_error_cast>
      Error:
      ! Can't convert <character> to <integer>.

# cannot use invalid .step

    Code
      slide(1, identity, .step = -1)
    Condition
      Error:
      ! `.step` must be at least 1, not -1.

---

    Code
      slide(1, identity, .step = 0)
    Condition
      Error:
      ! `.step` must be at least 1, not 0.

---

    Code
      slide(1, identity, .step = c(1, 2))
    Condition
      Error:
      ! `.step` must have size 1, not 2.

---

    Code
      (expect_error(slide(1, identity, .step = "x"), class = "vctrs_error_incompatible_type")
      )
    Output
      <error/vctrs_error_cast>
      Error:
      ! Can't convert <character> to <integer>.

# cannot use invalid .complete

    Code
      slide(1, identity, .complete = c(TRUE, TRUE))
    Condition
      Error:
      ! `.complete` must have size 1, not 2.

---

    Code
      (expect_error(slide(1, identity, .complete = "hi"), class = "vctrs_error_incompatible_type")
      )
    Output
      <error/vctrs_error_cast>
      Error:
      ! Can't convert <character> to <logical>.

# `error_call` and `.error_call` args aren't swallowed

    Code
      slide(1, fn, error_call = call("foo"))
    Condition
      Error in `foo()`:
      ! hi

---

    Code
      slide(1, fn_dot, .error_call = call("foo"))
    Condition
      Error in `foo()`:
      ! hi


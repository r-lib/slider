# size of each `.f` result must be 1

    Code
      (expect_error(hop_index_vec(1, 1, 1, 1, ~ c(.x, 1))))
    Output
      <error/rlang_error>
      Error:
      i In index: 1
      ! The result of `.f` must have size 1, not 2.

# inner type can be restricted with list_of

    Code
      (expect_error(hop_index_vec(1:2, 1:2, 1:2, 1:2, ~ if (.x == 1L) {
        list_of(1)
      } else {
        list_of("hi")
      }, .ptype = list_of(.ptype = double())), class = "vctrs_error_incompatible_type")
      )
    Output
      <error/vctrs_error_cast>
      Error in `hop_index_vec()`:
      ! Can't convert `..1` <character> to <double>.

# `.ptype = NULL` fails if no common type is found

    Code
      (expect_error(hop_index_vec(1:2, 1:2, 1:2, 1:2, ~ ifelse(.x == 1L, "hello", 1),
      .ptype = NULL), class = "vctrs_error_incompatible_type"))
    Output
      <error/vctrs_error_ptype2>
      Error in `hop_index_vec()`:
      ! Can't combine `out[[1]]` <character> and `out[[2]]` <double>.

# `.ptype = NULL` validates that element lengths are 1

    Code
      (expect_error(hop_index_vec(1:2, 1:2, 1:2, 1:2, ~ if (.x == 1L) {
        1:2
      } else {
        1
      }, .ptype = NULL)))
    Output
      <error/rlang_error>
      Error:
      i In index: 1
      ! The result of `.f` must have size 1, not 2.
    Code
      (expect_error(hop_index_vec(1:2, 1:2, 1:2, 1:2, ~ if (.x == 1L) {
        NULL
      } else {
        2
      }, .ptype = NULL)))
    Output
      <error/rlang_error>
      Error:
      i In index: 1
      ! The result of `.f` must have size 1, not 0.

# `.ptype = NULL` errors with non recyclable starts/stops

    Code
      (expect_error(hop_index_vec(integer(), integer(), integer(), 1:2, ~.x, .ptype = NULL),
      class = "vctrs_error_incompatible_size"))
    Output
      <error/vctrs_error_incompatible_size>
      Error in `hop_index_vec()`:
      ! Can't recycle `.starts` (size 0) to match `.stops` (size 2).


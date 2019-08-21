slide_index <- function(.x,
                        .i,
                        .f,
                        ...,
                        .before = 0L,
                        .after = 0L,
                        .complete = FALSE) {
  slide_index_impl(
    .x,
    .i,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .complete = .complete,
    .constrain = FALSE,
    .ptype = list()
  )
}

# ------------------------------------------------------------------------------

slide_index_impl <- function(.x,
                             .i,
                             .f,
                             ...,
                             .before,
                             .after,
                             .complete,
                             .constrain,
                             .ptype) {
  vec_assert(.x)
  vec_assert(.i)
  .f <- as_function(.f)

  f_call <- expr(.f(vec_slice(.x, window), ...))

  type <- -1L

  slide_index_core(
    x = .x,
    i = .i,
    f_call = f_call,
    before = .before,
    after = .after,
    complete = .complete,
    constrain = .constrain,
    ptype = .ptype,
    env = environment(),
    type = type
  )
}

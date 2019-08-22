slide_index2 <- function(.x,
                         .y,
                         .i,
                         .f,
                         ...,
                         .before = 0L,
                         .after = 0L,
                         .complete = FALSE) {
  slide_index_impl2(
    .x,
    .y,
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

slide_index_impl2 <- function(.x,
                              .y,
                              .i,
                              .f,
                              ...,
                              .before,
                              .after,
                              .complete,
                              .constrain,
                              .ptype) {
  vec_assert(.x)
  vec_assert(.y)

  .f <- as_function(.f)

  # TODO - more efficiently? reuse .x/.y rather than recycle
  args <- vec_recycle_common(.x, .y)

  f_call <- expr(.f(.x, .y, ...))

  type <- -2L

  slide_index_core(
    x = args,
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

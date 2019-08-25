slide_between <- function(.x, .i, .starts, .stops, .f, ...) {
  slide_between_impl(
    .x,
    .i,
    .starts,
    .stops,
    .f,
    ...,
    .constrain = FALSE,
    .ptype = list()
  )
}

slide_between_impl <- function(.x, .i, .starts, .stops, .f, ..., .constrain, .ptype) {
  vec_assert(.x)

  .f <- as_function(.f)

  f_call <- expr(.f(.x, ...))

  type <- -1L

  slide_between_common(
    x = .x,
    i = .i,
    starts = .starts,
    stops = .stops,
    f_call = f_call,
    constrain = .constrain,
    ptype = .ptype,
    env = environment(),
    type = type
  )
}

slide_impl <- function(.x,
                       .f,
                       ...,
                       .before,
                       .after,
                       .step,
                       .offset,
                       .complete,
                       .forward,
                       .ptype,
                       .constrain) {

  vec_assert(.x)
  .size <- vec_size(.x)
  .f <- as_function(.f)

  f_call <- expr(.f(.x, ...))

  out <- slide_core(
    x = .x,
    f_call = f_call,
    ptype = .ptype,
    env = environment(),
    type = -1L,
    .size = .size,
    .before = .before,
    .after = .after,
    .step = .step,
    .offset = .offset,
    .complete = .complete,
    .forward = .forward,
    .constrain = .constrain
  )

  out
}

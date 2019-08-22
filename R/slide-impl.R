slide_impl <- function(.x,
                       .f,
                       ...,
                       .before,
                       .after,
                       .step,
                       .complete,
                       .constrain,
                       .ptype) {
  vec_assert(.x)

  .f <- as_function(.f)

  f_call <- expr(.f(.x, ...))

  type <- -1L

  params <- list(
    type = type,
    constrain = .constrain,
    before = .before,
    after = .after,
    step = .step,
    complete = .complete
  )

  slide_core(
    x = .x,
    f_call = f_call,
    ptype = .ptype,
    env = environment(),
    params = params
  )
}

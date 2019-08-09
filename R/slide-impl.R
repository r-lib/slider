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
  .f <- as_function(.f)

  f_call <- expr(.f(.x, ...))

  type <- -1L

  param_list <- list(
    type,
    .constrain,
    .before,
    .after,
    .step,
    .complete,
    .forward,
    .offset
  )

  out <- slide_core(
    x = .x,
    f_call = f_call,
    ptype = .ptype,
    env = environment(),
    param_list = param_list
  )

  out
}

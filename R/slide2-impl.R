slide2_impl <- function(.x,
                        .y,
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
  vec_assert(.y)

  # TODO - Do more efficiently internally by reusing rather than recycling
  # https://github.com/tidyverse/purrr/blob/e4d553989e3d18692ebeeedb334b6223ae9ea294/src/map.c#L129
  # But use `vec_size_common()` to check sizes and get `.size`
  args <- vec_recycle_common(.x, .y)

  .f <- as_function(.f)

  f_call <- expr(.f(.x, .y, ...))

  type <- -2L

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
    x = args,
    f_call = f_call,
    ptype = .ptype,
    env = environment(),
    param_list = param_list
  )

  out
}

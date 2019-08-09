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

  .size <- vec_size(args[[1]])

  .f <- as_function(.f)

  f_call <- expr(.f(.x, .y, ...))

  out <- slide_core(
    .x = args,
    f_call = f_call,
    ptype = .ptype,
    type = -2L,
    .size = .size,
    .before = .before,
    .after = .after,
    .step = .step,
    .offset = .offset,
    .complete = .complete,
    .forward = .forward,
    .constrain = .constrain,
    .env = environment()
  )

  out
}

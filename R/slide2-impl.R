slide2_impl <- function(.x,
                        .y,
                        .f,
                        ...,
                        .before,
                        .after,
                        .step,
                        .offset,
                        .complete,
                        .dir,
                        .ptype,
                        .constrain = TRUE) {

  vec_assert(.x)
  vec_assert(.y)

  args <- vec_recycle_common(.x, .y)

  .x <- args[[1]]
  .y <- args[[2]]

  .n <- vec_size(.x)

  .f <- as_function(.f)

  .f_call <- expr(.f(vec_slice(.x, index), vec_slice(.y, index), ...))

  out <- slide_core(
    .f_call = .f_call,
    .n = .n,
    .before = .before,
    .after = .after,
    .step = .step,
    .offset = .offset,
    .complete = .complete,
    .dir = .dir,
    .ptype = .ptype,
    .constrain = .constrain,
    .env = environment()
  )

  vctrs:::vec_names(out) <- vctrs:::vec_names(.x)

  out
}

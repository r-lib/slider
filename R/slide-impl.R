slide_impl <- function(.x,
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
  .n <- vec_size(.x)
  .f <- as_function(.f)

  .f_call <- expr(.f(vec_slice(.x, index), ...))

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

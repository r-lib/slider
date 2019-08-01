slide_impl2 <- function(.x,
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
  .size <- vec_size(.x)
  .f <- as_function(.f)

  .f_call <- expr(.f(slice, ...))

  out <- slide_core2(
    .x = .x,
    .inputs = -1L,
    .f_call = .f_call,
    .size = .size,
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

  #vctrs:::vec_names(out) <- vctrs:::vec_names(.x)

  out
}

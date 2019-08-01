slide2_impl2 <- function(.x,
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

  .size <- vec_size(args[[1]])

  .f <- as_function(.f)

  .f_call <- expr(.f(slice, slice2, ...))

  out <- slide_core2(
    .x = args,
    .inputs = -2L,
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

  #vctrs:::vec_names(out) <- vctrs:::vec_names(args[[1]])

  out
}

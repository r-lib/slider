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
                       .constrain) {

  vec_assert(.x)
  .size <- vec_size(.x)
  .f <- as_function(.f)

  .f_call <- expr(.f(.x, ...))

  out <- slide_core(
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

  out
}

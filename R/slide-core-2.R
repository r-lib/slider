slide_core2 <- function(.x,
                       .inputs,
                       .f_call,
                       .size,
                       .before,
                       .after,
                       .step,
                       .offset,
                       .complete,
                       .dir,
                       .ptype,
                       .constrain,
                       .env) {

  arg_match(.dir, valid_dir())
  vec_assert(.dir, size = 1L)
  forward <- .dir == "forward"

  vec_assert(.complete, logical(), 1L)

  vec_assert(.step, size = 1L)
  .step <- vec_cast(.step, integer())

  if (.step < 1L) {
    glubort("`.step` must be at least 1, not {.step}.")
  }

  before_unbounded <- is_unbounded(.before)
  after_unbounded <- is_unbounded(.after)

  if (!before_unbounded) {
    vec_assert(.before, size = 1L)
    .before <- vec_cast(.before, integer())
  }

  if (!after_unbounded) {
    vec_assert(.after, size = 1L)
    .after <- vec_cast(.after, integer())
  }

  if (!is.null(.offset)) {
    vec_assert(.offset, size = 1L)
    .offset <- vec_cast(.offset, integer())
  }

  .Call(
    slurrr_slide2,
    .x,
    .inputs,
    .f_call,
    .size,
    .before,
    .after,
    .step,
    .offset,
    .complete,
    forward,
    .ptype,
    before_unbounded,
    after_unbounded,
    .constrain,
    .env
  )
}

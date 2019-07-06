stretch <- function(.x,
                   .f,
                   ...,
                   .offset = 0L,
                   .extend = 0L,
                   .step = 1L,
                   .partial = FALSE,
                   .dir = "forward") {

  arg_match(.dir, valid_dir())
  vec_assert(.dir, character(), 1L)
  forward <- .dir == "forward"

  if (forward) {
    .before <- unbounded()
    .after <- .extend
  } else {
    .before <- .extend
    .after <- unbounded()
  }

  slide(
    .x = .x,
    .f = .f,
    ...,
    .before = .before,
    .after = .after,
    .step = .step,
    .offset = .offset,
    .partial = .partial,
    .dir = .dir
  )
}

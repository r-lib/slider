stretch <- function(.x,
                   .f,
                   ...,
                   .offset = 0L,
                   .extend = 0L,
                   .step = 1L,
                   .partial = FALSE,
                   .dir = "forward",
                   .ptype = list()) {

  if (identical(.dir, "forward")) {
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
    .dir = .dir,
    .ptype = .ptype
  )
}

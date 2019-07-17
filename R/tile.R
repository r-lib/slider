tile <- function(.x,
                 .f,
                 ...,
                 .before = 0L,
                 .after = 0L,
                 .offset = NULL,
                 .partial = FALSE,
                 .dir = "forward",
                 .ptype = list()) {

  .step <- .before + .after + 1L

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

pslide <- function(.l,
                   .f,
                   ...,
                   .before = 0L,
                   .after = 0L,
                   .step = 1L,
                   .offset = NULL,
                   .partial = FALSE,
                   .dir = "forward") {
  pslide_impl(
    .l,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .step = .step,
    .offset = .offset,
    .partial = .partial,
    .dir = .dir,
    .ptype = list()
  )
}

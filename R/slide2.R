slide2 <- function(.x,
                   .y,
                   .f,
                   ...,
                   .before = 0L,
                   .after = 0L,
                   .step = 1L,
                   .offset = NULL,
                   .complete = FALSE,
                   .dir = "forward",
                   .ptype = list()) {
  slide2_impl(
    .x,
    .y,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .step = .step,
    .offset = .offset,
    .complete = .complete,
    .dir = .dir,
    .ptype = .ptype
  )
}

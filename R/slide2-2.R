slide2_2 <- function(.x,
                   .y,
                   .f,
                   ...,
                   .before = 0L,
                   .after = 0L,
                   .step = 1L,
                   .offset = NULL,
                   .complete = FALSE,
                   .dir = "forward") {
  slide2_impl2(
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
    .ptype = list(),
    .constrain = FALSE
  )
}

slide_2 <- function(.x,
                  .f,
                  ...,
                  .before = 0L,
                  .after = 0L,
                  .step = 1L,
                  .offset = NULL,
                  .complete = FALSE,
                  .dir = "forward") {
  slide_impl2(
    .x,
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

slide_vec_2 <- function(.x,
                      .f,
                      ...,
                      .before = 0L,
                      .after = 0L,
                      .step = 1L,
                      .offset = NULL,
                      .complete = FALSE,
                      .dir = "forward",
                      .ptype = list()) {

  if (is.null(.ptype)) {
    out <- slide_vec_simplify(
      .x,
      .f,
      ...,
      .before = .before,
      .after = .after,
      .step = .step,
      .offset = .offset,
      .complete = .complete,
      .dir = .dir
    )

    return(out)
  }

  slide_impl2(
    .x,
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

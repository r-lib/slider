old_slide <- function(.x,
                      .f,
                      ...,
                      .before = 0L,
                      .after = 0L,
                      .step = 1L,
                      .offset = NULL,
                      .complete = FALSE,
                      .dir = "forward") {
  old_slide_impl(
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

old_slide_vec <- function(.x,
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
    out <- old_slide_vec_simplify(
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

  old_slide_impl(
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

old_slide_vec_simplify <- function(.x,
                                   .f,
                                   ...,
                                   .before,
                                   .after,
                                   .step,
                                   .offset,
                                   .complete,
                                   .dir) {
  out <- old_slide(
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

  if (vec_size_common(!!!out) != 1L) {
    glubort("The size of all results from `.f` must be 1.")
  }

  vec_c(!!!out)
}

old_slide_impl <- function(.x,
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
  .n <- vec_size(.x)
  .f <- as_function(.f)

  .f_call <- expr(.f(vec_slice(.x, index), ...))

  out <- old_slide_core(
    .f_call = .f_call,
    .n = .n,
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

  vctrs:::vec_names(out) <- vctrs:::vec_names(.x)

  out
}

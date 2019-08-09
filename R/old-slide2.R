old_slide2 <- function(.x,
                       .y,
                       .f,
                       ...,
                       .before = 0L,
                       .after = 0L,
                       .step = 1L,
                       .offset = NULL,
                       .complete = FALSE,
                       .dir = "forward") {
  old_slide2_impl(
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

old_slide2_vec <- function(.x,
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

  if (is.null(.ptype)) {
    out <- old_slide2_vec_simplify(
      .x,
      .y,
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

  old_slide2_impl(
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

old_slide2_vec_simplify <- function(.x,
                                    .y,
                                    .f,
                                    ...,
                                    .before,
                                    .after,
                                    .step,
                                    .offset,
                                    .complete,
                                    .dir) {
  out <- old_slide2(
    .x,
    .y,
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
    abort("The size of all results from `.f` must be 1.")
  }

  vec_c(!!!out)
}

old_slide2_impl <- function(.x,
                            .y,
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
  vec_assert(.y)

  args <- vec_recycle_common(.x, .y)

  .x <- args[[1]]
  .y <- args[[2]]

  .n <- vec_size(.x)

  .f <- as_function(.f)

  .f_call <- expr(.f(vec_slice(.x, index), vec_slice(.y, index), ...))

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

slide_index <- function(.x,
                        .i,
                        .f,
                        ...,
                        .before = 0L,
                        .after = 0L,
                        .complete = FALSE) {
  slide_index_impl(
    .x,
    .i,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .complete = .complete,
    .constrain = FALSE,
    .ptype = list()
  )
}

# ------------------------------------------------------------------------------

slide_index_vec <- function(.x,
                            .i,
                            .f,
                            ...,
                            .before = 0L,
                            .after = 0L,
                            .complete = FALSE,
                            .ptype = list()) {

  if (is.null(.ptype)) {
    out <- slide_index_simplify(
      .x,
      .i,
      .f,
      ...,
      .before = .before,
      .after = .after,
      .complete = .complete
    )

    return(out)
  }

  slide_index_impl(
    .x,
    .i,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .complete = .complete,
    .constrain = TRUE,
    .ptype = .ptype
  )
}

slide_index_simplify <- function(.x,
                                 .i,
                                 .f,
                                 ...,
                                 .before,
                                 .after,
                                 .complete) {
  out <- slide_index(
    .x,
    .i,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .complete = .complete
  )

  size <- vec_size_common(!!!out)
  if (size != 1L) {
    abort(paste0("Incompatible lengths: ", size, ", 1."))
  }

  vec_c(!!!out)
}

# ------------------------------------------------------------------------------

slide_index_impl <- function(.x,
                             .i,
                             .f,
                             ...,
                             .before,
                             .after,
                             .complete,
                             .constrain,
                             .ptype) {
  vec_assert(.x)
  vec_assert(.i)
  .f <- as_function(.f)

  f_call <- expr(.f(.x, ...))

  type <- -1L

  slide_index_core(
    x = .x,
    i = .i,
    f_call = f_call,
    before = .before,
    after = .after,
    complete = .complete,
    constrain = .constrain,
    ptype = .ptype,
    env = environment(),
    type = type
  )
}

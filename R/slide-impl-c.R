slide_impl_c <- function(.x,
                       .f,
                       ...,
                       .before = 0L,
                       .after = 0L,
                       .step = 1L,
                       .offset = NULL,
                       .partial = FALSE,
                       .dir = "forward",
                       .ptype = list()) {

  vec_assert(.x)
  .x_n <- vec_size(.x)

  arg_match(.dir, valid_dir())
  vec_assert(.dir, character(), 1L)
  forward <- .dir == "forward"

  # If we are unbounded() in `.before` we pin `.before` to an offset such that
  # `.before + startpoint == 1L`
  .before_unbounded <- FALSE
  if (is_unbounded(.before)) {
    .before_unbounded <- TRUE
    if (forward) {
      .before <- 0L
    } else {
      .before <- .x_n - 1L
    }
  }

  # If we are unbounded() in `.after` we pin `.after` to an offset such that
  # `.after + startpoint == .x_n`
  .after_unbounded <- FALSE
  if (is_unbounded(.after)) {
    .after_unbounded <- TRUE
    if (forward) {
      .after <- .x_n - 1L
    } else {
      .after <- 0L
    }
  }

  vec_assert(.before, size = 1L)
  .before <- vec_cast(.before, integer())

  vec_assert(.after, size = 1L)
  .after <- vec_cast(.after, integer())

  # `.offset + startpoint` determines the initial `entry` in the `out` container
  # The default offset is at `.before` if forward / `.after` if backward
  if (is.null(.offset)) {
    if (forward) {
      .offset <- .before
    } else {
      .offset <- .after
    }
  }

  vec_assert(.step, size = 1L)
  vec_assert(.offset, size = 1L)
  vec_assert(.partial, logical(), 1L)

  .step <- vec_cast(.step, integer())
  .offset <- vec_cast(.offset, integer())

  .f <- as_function(.f)

  if (.before < 0L) {
    glubort("`.before` must be at least 0, not {.before}.")
  }

  if (.after < 0L) {
    glubort("`.after` must be at least 0, not {.after}.")
  }

  if (.offset < 0L) {
    glubort("`.offset` must be at least 0, not {.offset}.")
  }

  if (.step < 1L) {
    glubort("`.step` must be at least 1, not {.step}.")
  }

  width <- .before + .after + 1L
  if (.before + .after + 1L > .x_n) {
    glubort("The width of the rolling interval ({width}) must be less than or equal to the size of `.x` ({.x_n}).")
  }

  if (forward) {
    if (.offset < .before) {
      glubort("`.offset` ({.offset}) must be at least as large as `.before` ({.before}).")
    }
    if (.offset + .after + 1L > .x_n) {
      # improve message
      glubort("`.offset` and `.after` imply a location ({.offset + .after + 1}) outside the size of `.x` ({.x_n}).")
    }
  } else {
    if (.offset < .after) {
      glubort("`.offset` ({.offset}) must be at least as large as `.after` ({.after}).")
    }
    if (.offset + .before + 1L > .x_n) {
      glubort("`.offset` and `.before` imply a location ({.offset + .before + 1}) outside the size of `.x` ({.x_n}).")
    }
  }

 .Call(
   slurrr_slide,
   environment(),
   .x,
   .before,
   .after,
   .step,
   .offset,
   .partial,
   forward,
   .ptype,
   .before_unbounded,
   .after_unbounded
  )
}



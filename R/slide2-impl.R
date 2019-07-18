slide2_impl <- function(.x,
                        .y,
                        .f,
                        ...,
                        .before,
                        .after,
                        .step,
                        .offset,
                        .complete,
                        .dir,
                        .ptype) {

  vec_assert(.x)
  vec_assert(.y)

  args <- vec_recycle_common(.x, .y)

  .x <- args[[1]]
  .y <- args[[2]]

  .x_n <- vec_size(.x)

  .f <- as_function(.f)

  arg_match(.dir, valid_dir())
  vec_assert(.dir, size = 1L)
  forward <- .dir == "forward"

  vec_assert(.complete, logical(), 1L)

  vec_assert(.step, size = 1L)
  .step <- vec_cast(.step, integer())

  if (.step < 1L) {
    glubort("`.step` must be at least 1, not {.step}.")
  }

  before_unbounded <- is_unbounded(.before)
  after_unbounded <- is_unbounded(.after)

  if (!before_unbounded) {
    vec_assert(.before, size = 1L)
    .before <- vec_cast(.before, integer())
  }

  if (!after_unbounded) {
    vec_assert(.after, size = 1L)
    .after <- vec_cast(.after, integer())
  }

  .offset <- compute_offset(
    .offset,
    .before,
    .after,
    before_unbounded,
    after_unbounded,
    .complete,
    forward
  )

  # The order of checks are important here and are done so they they work even
  # if both .before/.after are unbounded(). The goal is to set unbounded()
  # .before/.after values to values such that the width of the first
  # iteration's window frame is correct
  if (forward) {
    if (before_unbounded) {
      .before <- .offset
    }
    if (after_unbounded) {
      .after <- .x_n - 1L - .offset
    }
  } else {
    if (after_unbounded) {
      .after <- .offset
    }
    if (before_unbounded) {
      .before <- .x_n - 1L - .offset
    }
  }

  validate_before_after_negativeness(.before, .after)

  if (forward) {
    startpoint <- 1L
    endpoint <- .x_n
    start <- startpoint - .before + .offset
    stop <- start + (.before + .after)
    entry_step <- .step
    entry_offset <- .offset
  }
  else {
    startpoint <- .x_n
    endpoint <- 1L
    start <- startpoint + .after - .offset
    stop <- start - (.before + .after)
    entry_step <- -.step
    entry_offset <- -.offset
  }

  entry <- startpoint + entry_offset

  # We check to see if we are unbounded() in the direction that we are sliding.
  # If so, we force `.complete = FALSE` to compute the correct number of iterations
  # (This must be done after we compute the .offset, because the .complete-ness
  # does affect the partial results at the beginning)
  if ((after_unbounded && forward) || (before_unbounded && !forward)) {
    .complete <- FALSE
  }

  n_iter <- iterations(
    startpoint,
    endpoint,
    .before,
    .after,
    .step,
    .offset,
    .complete,
    forward
  )

  start_step <- entry_step
  stop_step <- entry_step
  if (forward) {
    if (before_unbounded) {
      start_step <- 0L
    }
    if (after_unbounded) {
      stop_step <- 0L
    }
  } else {
    if (before_unbounded) {
      stop_step <- 0L
    }
    if (after_unbounded) {
      start_step <- 0L
    }
  }

  if (forward) {
    bound_start <- max
    bound_stop <- min
  } else {
    bound_start <- min
    bound_stop <- max
  }

  out <- vec_init(.ptype, n = .x_n)
  vctrs:::vec_names(out) <- vctrs:::vec_names(.x)
  .ptype_is_list <- vctrs::vec_is(.ptype, list())

  for (i in seq_len(n_iter)) {
    index <- seq(
      from = bound_start(start, startpoint),
      to = bound_stop(stop, endpoint)
    )

    elt <- .f(vec_slice(.x, index), vec_slice(.y, index), ...)

    # will be way more efficient at the C level with `copy = FALSE`
    if (.ptype_is_list) {
      out <- vec_assign(out, entry, list(elt))
    } else {
      out <- vec_assign(out, entry, elt)
    }

    start <- start + start_step
    stop <- stop + stop_step
    entry <- entry + entry_step
  }

  out
}

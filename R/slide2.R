slide <- function(.x,
                  .f,
                  ...,
                  .before = 0L,
                  .after = 0L,
                  .step = 1L,
                  .offset = NULL,
                  .partial = FALSE,
                  .dir = "forward") {

  arg_match(.dir, valid_dir())
  vec_assert(.dir, character(), 1L)
  forward <- .dir == "forward"

  if (is.null(.offset)) {
    if (forward) {
      .offset <- .before
    } else {
      .offset <- .after
    }
  }

  vec_assert(.x)
  vec_assert(.before, size = 1L)
  vec_assert(.after, size = 1L)
  vec_assert(.step, size = 1L)
  vec_assert(.offset, size = 1L)
  vec_assert(.partial, logical(), 1L)

  .before <- vec_cast(.before, integer())
  .after <- vec_cast(.after, integer())
  .step <- vec_cast(.step, integer())
  .offset <- vec_cast(.offset, integer())

  .f <- as_function(.f)

  .x_n <- vec_size(.x)

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

  out <- vec_init(list(), n = .x_n)

  assign <- get_assigner(list())

  # Number of "complete" iterations (where .partial is not involved)
  complete_iterations_n <- iterations(.x_n, .before, .after, .step, .offset, FALSE, forward)

  # compute before adjustments are made to .step/.offset
  if (.partial) {
    max_iterations_n <- iterations(.x_n, .before, .after, .step, .offset, .partial, forward)
    partial_iterations_n <- max_iterations_n - complete_iterations_n
  }

  if (forward) {
    startpoint <- 1L
    endpoint <- .x_n
    start <- startpoint - .before + .offset
    stop <- start + (.before + .after)
  }
  else {
    startpoint <- .x_n
    endpoint <- 1L
    start <- startpoint - .offset + .after
    stop <- start - (.before + .after)
    .step <- -.step
    .offset <- -.offset
  }

  entry <- startpoint + .offset

  i <- seq(from = start, to = stop)

  for (j in seq_len(complete_iterations_n)) {
    elt <- .f(vec_slice(.x, i), ...)
    # will be way more efficient at the C level with `copy = FALSE`
    out <- assign(out, entry, value = elt)

    i <- i + .step
    entry <- entry + .step
  }

  # Done if no `.partial`
  if (!.partial) {
    return(out)
  }

  # can't compute any partial iterations
  if (partial_iterations_n == 0L) {
    return(out)
  }

  start <- i[1] # current `start` point

  for (j in seq_len(partial_iterations_n)) {
    # cannot extract outside the loop, length of `i` is possibly shrinking each iteration
    i <- seq(from = start, to = endpoint)
    elt <- .f(vec_slice(.x, i), ...)
    out <- assign(out, entry, value = elt)

    start <- start + .step
    entry <- entry + .step
  }

  out
}

# ------------------------------------------------------------------------------

adjust_partial <- function(partial, forward, before, after) {
  if (partial) {
    if (forward) {
      after + 1L
    } else {
      before + 1L
    }
  } else {
    1L
  }
}

# number of positions lost to `.offset`
adjust_n <- function(forward, offset, before, after) {
  if (forward) {
    offset - before
  } else {
    offset - after
  }
}

iterations <- function(n, before, after, step, offset, partial, forward) {
  size <- before + after + 1L
  n <- n - adjust_n(forward, offset, before, after)
  adjust <- adjust_partial(partial, forward, before, after)
  ceiling((n - size + adjust) / step)
}

slide_impl <- function(.x,
                       .f,
                       ...,
                       .before,
                       .after,
                       .step,
                       .offset,
                       .partial,
                       .dir,
                       .ptype) {

  vec_assert(.x)
  .x_n <- vec_size(.x)

  .f <- as_function(.f)

  arg_match(.dir, valid_dir())
  vec_assert(.dir, size = 1L)
  forward <- .dir == "forward"

  vec_assert(.step, size = 1L)
  .step <- vec_cast(.step, integer())

  vec_assert(.partial, logical(), 1L)

  before_unbounded <- is_unbounded(.before)
  after_unbounded <- is_unbounded(.after)
  doubly_unbounded <- before_unbounded && after_unbounded

  if (!before_unbounded) {
    vec_assert(.before, size = 1L)
    .before <- vec_cast(.before, integer())
  }

  if (!after_unbounded) {
    vec_assert(.after, size = 1L)
    .after <- vec_cast(.after, integer())
  }

  # If we are unbounded in the direction we are sliding, we compute the number
  # of complete iterations as the maximum number of iterations. This implies
  # we set `partial = TRUE` in iterations()
  partial_unbounded <- FALSE
  if (forward && after_unbounded | !forward && before_unbounded) {
    partial_unbounded <- TRUE
  }

  # It's complex to figure out the default offset if it is NULL.
  # - If doubly unbounded, its just 0
  # - If forward and before is unbounded, its normally 0 but if after < 0 you account for that
  # - If forward and before is not unbounded, its normally before but if before < 0 set a minimum at 0
  # - If backward and after is unbounded, its normally 0 but if before < 0 you account for that
  # - If backward and after is not unbounded, its normally after but if after < 0 set a minimum at 0
  if (is.null(.offset)) {
    if (doubly_unbounded) {
      .offset <- 0L
    } else {
      if (forward) {
        if (before_unbounded) {
          .offset <- abs(min(.after, 0L))
        } else {
          .offset <- max(.before, 0L)
        }
      } else {
        if (after_unbounded) {
          .offset <- abs(min(.before, 0L))
        } else {
          .offset <- max(.after, 0L)
        }
      }
    }
  }

  vec_assert(.offset, size = 1L)
  .offset <- vec_cast(.offset, integer())

  # The order of checks are important here and are done so they they work even
  # if both .before/.after are unbounded(). The goal is to set unbounded()
  # .before/.after values to values such that the width of the first
  # iteration's window frame is correct
  if (forward) {
    if (before_unbounded) {
      .before <- .offset
    }
    if (after_unbounded) {
      .after <- .x_n - 1L - max(.before, .offset)
    }
  } else {
    if (after_unbounded) {
      .after <- .offset
    }
    if (before_unbounded) {
      .before <- .x_n - 1L - max(.after, .offset)
    }
  }


  if (.offset < 0L) {
    glubort("`.offset` must be at least 0, not {.offset}.")
  }

  if (.step < 1L) {
    glubort("`.step` must be at least 1, not {.step}.")
  }

  before_negative <- .before < 0L
  after_negative <- .after < 0L

  if (before_negative && after_negative) {
    glubort("`.before` ({.before}) and `.after` ({.after}) cannot both be negative.")
  }

  if (before_negative && abs(.before) > .after) {
    glubort("When `.before` ({.before}) is negative, it's absolute value ({abs(.before)}) cannot be greater than `.after` ({.after}).")
  }

  if (after_negative && abs(.after) > .before) {
    glubort("When `.after` ({.after}) is negative, it's absolute value ({abs(.after)}) cannot be greater than `.before` ({.before}).")
  }

  width <- .before + .after + 1L
  if (width > .x_n) {
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

  out <- vec_init(.ptype, n = .x_n)
  vctrs:::vec_names(out) <- vctrs:::vec_names(.x)
  .ptype_is_list <- vctrs::vec_is(.ptype, list())

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
    start <- startpoint - .offset + .after
    stop <- start - (.before + .after)
    entry_step <- -.step
    entry_offset <- -.offset
  }

  entry <- startpoint + entry_offset

  # Number of "complete" iterations
  complete_iterations_n <- iterations(startpoint, endpoint, .before, .after, .step, .offset, partial_unbounded, forward)

  # compute before adjustments are made to .step/.offset
  if (.partial) {
    max_iterations_n <- iterations(startpoint, endpoint, .before, .after, .step, .offset, .partial, forward)
    partial_iterations_n <- max_iterations_n - complete_iterations_n
  }

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

  for (j in seq_len(complete_iterations_n)) {
    # must be inside the loop since unbounded-ness could affect the length of i
    i <- seq(from = start, to = stop)
    elt <- .f(vec_slice(.x, i), ...)

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

  # Done if no `.partial`
  if (!.partial) {
    return(out)
  }

  # can't compute any partial iterations
  if (partial_iterations_n == 0L) {
    return(out)
  }

  # Pin `stop` to the endpoint of `.x`
  stop <- endpoint
  stop_step <- 0L

  for (j in seq_len(partial_iterations_n)) {
    # cannot extract outside the loop, length of `i` is possibly shrinking each iteration
    i <- seq(from = start, to = stop)
    elt <- .f(vec_slice(.x, i), ...)

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

# ------------------------------------------------------------------------------

# Terms:
# `frame_pos` = the current position in the output vector along .x
# `frame_end` = the location of the end of the current window along .x (farthest from `startpoint`)
# `frame_start` = the location of the start of the current window along .x (closest to `startpoint`)
# `frame_boundary` = partial ? frame_start : frame_end
# OOR = "out of range"

# If `partial = FALSE`, we stop if either:
# - The `frame_pos` gets OOR
# - The `frame_end` gets OOR

# If `partial = TRUE` we stop if either:
# - The `frame_pos` gets OOR
# - The `frame_start` gets OOR

iterations <- function(startpoint, endpoint, before, after, step, offset, partial, forward) {
  if (forward) {
    frame_pos_adjustment <- offset
  } else {
    frame_pos_adjustment <- -offset
  }

  if (partial) {
    # boundary = start of frame
    if (forward) {
      frame_boundary_adjustment <- -before
    } else {
      frame_boundary_adjustment <- after
    }
  } else {
    # boundary = end of frame
    if (forward) {
      frame_boundary_adjustment <- after
    } else {
      frame_boundary_adjustment <- -before
    }
  }

  frame_pos <- startpoint + frame_pos_adjustment
  frame_boundary <- frame_pos + frame_boundary_adjustment

  n_iter_frame_pos <- compute_n_iter(endpoint, frame_pos, step)
  n_iter_frame_boundary <- compute_n_iter(endpoint, frame_boundary, step)

  min(n_iter_frame_pos, n_iter_frame_boundary)
}

compute_n_iter <- function(endpoint, loc, step) {
  (abs(endpoint - loc) %/% step) + 1L
}

# ------------------------------------------------------------------------------

valid_dir <- function() {
  c("forward", "backward")
}

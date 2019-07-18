slide_impl <- function(.x,
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

    elt <- .f(vec_slice(.x, index), ...)

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
# `frame_pos`      = the current position in the output vector along .x
# `frame_end`      = the location of the end of the current window along
#                    .x (farthest from `startpoint`)
# `frame_start`    = the location of the start of the current window
#                    along .x (closest to `startpoint`)
# `frame_boundary` = partial ? frame_start : frame_end
# OOR              = "out of range"

# If `complete = TRUE`, we stop if either:
# - The `frame_pos` gets OOR
# - The `frame_end` gets OOR

# If `complete = FALSE` we stop if either:
# - The `frame_pos` gets OOR
# - The `frame_start` gets OOR

iterations <- function(startpoint,
                       endpoint,
                       before,
                       after,
                       step,
                       offset,
                       complete,
                       forward) {

  if (forward) {
    frame_pos_adjustment <- offset
  } else {
    frame_pos_adjustment <- -offset
  }

  if (!complete) {
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

  n_iter_frame_pos <- compute_iterations(endpoint, frame_pos, step, forward)
  n_iter_frame_boundary <- compute_iterations(endpoint, frame_boundary, step, forward)

  min(n_iter_frame_pos, n_iter_frame_boundary)
}

# It is possible to set a combination of .offset/.after/.complete such that
# this difference ends up out of bounds so we pin it to 0L if that is the case.
compute_iterations <- function(endpoint, loc, step, forward) {
  diff <- endpoint - loc

  if (!forward) {
    diff <- -diff
  }

  n_iter <- (diff %/% step) + 1L

  max(n_iter, 0L)
}

# ------------------------------------------------------------------------------

valid_dir <- function() {
  c("forward", "backward")
}

# ------------------------------------------------------------------------------

compute_offset <- function(offset,
                           before,
                           after,
                           before_unbounded,
                           after_unbounded,
                           complete,
                           forward) {

  null_offset <- is.null(offset)

  if (!null_offset) {
    vec_assert(offset, size = 1L)
    offset <- vec_cast(offset, integer())
  }

  # - Checking if the start of the frame is out of range when going forward
  # - Ensure `before <= offset` so we can create a full window
  if (complete && forward && !before_unbounded && (null_offset || before > offset)) {
    offset <- before
  }

  # - Checking if the start of the frame is out of range when going backward
  # - Ensure `after <= offset` so we can create a full window
  if (complete && !forward && !after_unbounded && (null_offset || after > offset)) {
    offset <- after
  }

  # - Checking if the end of the frame is out of range when going forward
  # - Ensure that if `after < 0`, then `abs(after) <= offset`
  #   so we have some data to partially compute on
  if (!complete && forward && !after_unbounded && after < 0L) {
    if (null_offset || offset < -after) {
      offset <- -after
    }
  }

  # - Checking if the end of the frame is out of range when going backward
  # - Ensure that if `before < 0`, then `abs(before) <= offset` so we have
  #   some data to partially compute on
  if (!complete && !forward && !before_unbounded && before < 0L) {
    if (null_offset || offset < -before) {
      offset <- -before
    }
  }

  # If offset is still NULL, meaning that we have a usable window, then
  # set offset to 0
  if (is.null(offset)) {
    offset <- 0L
  }

  offset
}

# ------------------------------------------------------------------------------

validate_before_after_negativeness <- function(before, after) {
  before_negative <- before < 0L
  after_negative <- after < 0L

  if (!before_negative && !after_negative) {
    return(invisible())
  }

  if (before_negative && after_negative) {
    glubort(
      "`.before` ({before}) and `.after` ({after}) cannot both be negative."
    )
  }

  if (before_negative && abs(before) > after) {
    glubort(
      "When `.before` ({before}) is negative, ",
      "it's absolute value ({abs(before)}) ",
      "cannot be greater than `.after` ({after})."
    )
  }

  if (after_negative && abs(after) > before) {
    glubort(
      "When `.after` ({after}) is negative, ",
      "it's absolute value ({abs(after)}) ",
      "cannot be greater than `.before` ({before})."
    )
  }

  invisible()
}

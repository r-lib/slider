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
  .n <- vec_size(.x)
  .f <- as_function(.f)

  .f_call <- expr(.f(vec_slice(.x, index), ...))

  out <- slide_core(
    .f_call = .f_call,
    .n = .n,
    .before = .before,
    .after = .after,
    .step = .step,
    .offset = .offset,
    .complete = .complete,
    .dir = .dir,
    .ptype = .ptype,
    .env = environment()
  )

  vctrs:::vec_names(out) <- vctrs:::vec_names(.x)

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

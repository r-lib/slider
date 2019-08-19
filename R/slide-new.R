slide_new <- function(.x,
                        .f,
                        ...,
                        .before = 0L,
                        .after = 0L,
                        .step = 1L,
                        .complete = FALSE
) {
  slide_new_impl(
    .x,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .step = .step,
    .complete = .complete,
    .constrain = FALSE,
    .ptype = list()
  )
}

slide_new_impl <- function(.x,
                             .f,
                             ...,
                             .before,
                             .after,
                             .step,
                             .complete,
                             .constrain,
                             .ptype) {
  n <- vec_size(.x)
  .f <- as_function(.f)

  params <- list(
    before = .before,
    after = .after,
    step = .step,
    complete = .complete,
    ptype = .ptype,
    constrain = .constrain,
    entry = 1L,
    position = 1L,
    n = n
  )

  params <- check_params(params)

  window_params <- init_window_params(params)

  before_unbounded <- is_unbounded(params$before)
  after_unbounded <- is_unbounded(params$after)

  if (before_unbounded && after_unbounded) {
    loop_double_unbounded_new(.x, .f, params, window_params, ...)
  } else if (before_unbounded) {
    loop_before_unbounded_new(.x, .f, params, window_params, ...)
  } else if (after_unbounded) {
    loop_after_unbounded_new(.x, .f, params, window_params, ...)
  } else {
    loop_bounded_new(.x, .f, params, window_params, ...)
  }
}

# ------------------------------------------------------------------------------

init_window_params <- function(params) {
  if (is_unbounded(params$before)) {
    start <- 1L
    start_step <- 0L
    start_step_one <- 0L
    start_ahead <- FALSE
  } else {
    start <- 1L - params$before
    start_step <- params$step
    start_step_one <- 1L
    start_ahead <- params$before < 0L
  }

  if (is_unbounded(params$after)) {
    stop <- params$n
    stop_step <- 0L
    stop_step_one <- 0L
    stop_behind <- FALSE
  } else {
    stop <- 1L + params$after
    stop_step <- params$step
    stop_step_one <- 1L
    stop_behind <- params$after < 0L
  }

  window_params <- list(
    start = start,
    stop = stop,
    start_step = start_step,
    stop_step = stop_step,
    start_step_one = start_step_one,
    stop_step_one = stop_step_one,
    start_ahead = start_ahead,
    stop_behind = stop_behind
  )

  window_params
}

# ------------------------------------------------------------------------------

# Conceptually there are 4 ways to get out of bounds, and in these cases no
# evalution of the function should be made, and the parameters should be
# incremented by 1, not by the step value.

# 1. Start of the window is past the last data point
# slide(1:5, ~.x, .before = -1, .after = 1)
# 1 2 3 4 5 . . .
# . . . . . | - |
#           ^
#           |- Start of window outside range

is_window_start_ahead_of_last <- function(params, window_params) {
  window_params$start_ahead && window_params$start > params$n
}

# 2. End of the window is before the first data point
# slide(1:5, ~.x, .before = 1, .after = -1)
# . . . 1 2 3 4 5
# | - | . . . . .
#     ^
#     |- End of window outside range

is_window_stop_behind_first <- function(params, window_params) {
  window_params$stop_behind && window_params$stop < 1L
}

# 3. Start of the window is before the first data point, and `.complete = TRUE`
# slide(1:5, ~.x, .before = 1, .complete = TRUE)
# . 1 2 3 4 5 . .
# | - | . . . . .
# ^
# |- Start of window outside range

is_window_start_behind_first <- function(params, window_params) {
  window_params$start < 1L
}

# 3. End of the window is after the last data point, and `.complete = TRUE`
# slide(1:5, ~.x, .after = 1, .complete = TRUE)
# 1 2 3 4 5 . . .
# . . . | - | . .
#           ^
#           |- End of window outside range

is_window_stop_ahead_of_last <- function(params, window_params) {
  window_params$stop > params$n
}

# ------------------------------------------------------------------------------

loop_bounded_new <- function(x, f, params, window_params, ...) {
  out <- vec_init(params$ptype, params$n)

  while(params$position <= params$n) {
    if (params$complete) {
      if (is_window_start_behind_first(params, window_params)) {
        params <- increment_params_by_one(params)
        window_params <- increment_window_by_one(window_params)
        next
      }

      if (is_window_stop_ahead_of_last(params, window_params)) {
        params <- increment_params_by_one(params)
        window_params <- increment_window_by_one(window_params)
        next
      }
    }

    if (is_window_start_ahead_of_last(params, window_params)) {
      params <- increment_params_by_one(params)
      window_params <- increment_window_by_one(window_params)
      next
    }

    if (is_window_stop_behind_first(params, window_params)) {
      params <- increment_params_by_one(params)
      window_params <- increment_window_by_one(window_params)
      next
    }

    bounded_window_start <- max(window_params$start, 1L)
    bounded_window_stop <- min(window_params$stop, params$n)

    out <- slice_eval_assign(out, x, f, bounded_window_start, bounded_window_stop, params, ...)

    params <- increment_params_by_step(params)
    window_params <- increment_window_by_step(window_params)
  }

  out
}

loop_before_unbounded_new <- function(x, f, params, window_params, ...) {
  out <- vec_init(params$ptype, params$n)

  while(params$position <= params$n) {
    if (params$complete) {
      if (is_window_stop_ahead_of_last(params, window_params)) {
        params <- increment_params_by_one(params)
        window_params <- increment_window_by_one(window_params)
        next
      }
    }

    if (is_window_stop_behind_first(params, window_params)) {
      params <- increment_params_by_one(params)
      window_params <- increment_window_by_one(window_params)
      next
    }

    bounded_window_stop <- min(window_params$stop, params$n)

    out <- slice_eval_assign(out, x, f, window_params$start, bounded_window_stop, params, ...)

    params <- increment_params_by_step(params)
    window_params <- increment_window_by_step(window_params)
  }

  out
}

loop_after_unbounded_new <- function(x, f, params, window_params, ...) {
  out <- vec_init(params$ptype, params$n)

  while(params$position <= params$n) {
    if (params$complete) {
      if (is_window_start_behind_first(params, window_params)) {
        params <- increment_params_by_one(params)
        window_params <- increment_window_by_one(window_params)
        next
      }
    }

    if (is_window_start_ahead_of_last(params, window_params)) {
      params <- increment_params_by_one(params)
      window_params <- increment_window_by_one(window_params)
      next
    }

    bounded_window_start <- max(window_params$start, 1L)

    out <- slice_eval_assign(out, x, f, bounded_window_start, window_params$stop, params, ...)

    params <- increment_params_by_step(params)
    window_params <- increment_window_by_step(window_params)
  }

  out
}

loop_double_unbounded_new <- function(x, f, params, window_params, ...) {
  out <- vec_init(params$ptype, params$n)

  while(params$position <= params$n) {
    out <- slice_eval_assign(out, x, f, window_params$start, window_params$stop, params, ...)

    params <- increment_params_by_step(params)
    window_params <- increment_window_by_step(window_params)
  }

  out
}

# ------------------------------------------------------------------------------

increment_params_by_one <- function(params) {
  params$position <- params$position + 1L
  params$entry <- params$entry + 1L
  params
}

increment_params_by_step <- function(params) {
  params$position <- params$position + params$step
  params$entry <- params$entry + params$step
  params
}

increment_window_by_one <- function(window_params) {
  window_params$start <- window_params$start + window_params$start_step_one
  window_params$stop <- window_params$stop + window_params$stop_step_one
  window_params
}

increment_window_by_step <- function(window_params) {
  window_params$start <- window_params$start + window_params$start_step
  window_params$stop <- window_params$stop + window_params$stop_step
  window_params
}


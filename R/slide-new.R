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

  # check_params()
  params <- init_params(.before, .after, .step, .complete, .constrain, .ptype, n)

  window_params <- init_window_params(params)

  out <- vec_init(.ptype, params$n)

  while(params$iteration <= params$iteration_max) {
    start <- max(window_params$start, 0L)
    stop <- min(window_params$stop, params$n)

    slice <- vec_slice(.x, seq2(start, stop))

    elt <- .f(slice, ...)

    if (params$constrain) {
      elt <- vec_cast(elt, .ptype)

      if (vec_size(elt) != 1L) {
        abort(sprintf("The size of each result of `.f` must be size 1. Iteration %i was size %i.", params$iteration, vec_size(elt)))
      }

      out <- vec_assign(out, params$iteration, elt)
    } else {
      out[[params$iteration]] <- elt
    }

    params$iteration <- params$iteration + params$step
    window_params$start <- window_params$start + window_params$start_step
    window_params$stop <- window_params$stop + window_params$stop_step
  }

  out
}

# ------------------------------------------------------------------------------

init_params <- function(before, after, step, complete, constrain, ptype, n) {
  before_unbounded <- is_unbounded(before)
  after_unbounded <- is_unbounded(after)

  if (before_unbounded) {
    before <- 0L
  }

  if (after_unbounded) {
    after <- 0L
  }

  before_positive <- before >= 0L
  after_positive <- after >= 0L

  if (!before_positive && !after_positive) {
    abort("Both `.before` and `.after` cannot be negative.")
  }

  iteration <- 1L
  iteration_max <- n

  if (complete) {
    if (before_positive) {
      iteration <- iteration + before
    }

    if (after_positive) {
      iteration_max <- iteration_max - after
    }
  } else {
    if (!before_positive) {
      iteration_max <- iteration_max - abs(before)
    }

    if (!after_positive) {
      iteration <- iteration + abs(after)
    }
  }

  list(
    before = before,
    after = after,
    before_unbounded = before_unbounded,
    after_unbounded = after_unbounded,
    before_positive = before_positive,
    after_positive = after_positive,
    step = step,
    complete = complete,
    constrain = constrain,
    iteration = iteration,
    iteration_max = iteration_max,
    n = n
  )
}

# ------------------------------------------------------------------------------

init_window_params <- function(params) {
  window_adjustment <- 0L

  # Forward adjustment to match the number of iterations
  if (params$complete) {
    if (params$before_positive) {
      window_adjustment <- params$before
    }
  } else {
    if (!params$after_positive) {
      window_adjustment <- abs(params$after)
    }
  }

  if (params$before_unbounded) {
    start <- 1L
    start_step <- 0L
  } else {
    start <- 1L - params$before + window_adjustment
    start_step <- params$step
  }

  if (params$after_unbounded) {
    stop <- params$n
    stop_step <- 0L
  } else {
    stop <- 1L + params$after + window_adjustment
    stop_step <- params$step
  }

  window_params <- list(
    start = start,
    stop = stop,
    start_step = start_step,
    stop_step = stop_step
  )

  window_params
}


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

  if (is_unbounded(params$before)) {
    window_start_step <- 0L
    window_start <- 1L
    before_negative <- FALSE
  } else {
    window_start_step <- params$step
    window_start <- 1L - params$before
    before_negative <- params$before < 0L
  }

  if (is_unbounded(params$after)) {
    window_stop_step <- params$step
    window_stop <- params$n
    after_negative <- FALSE
  } else {
    window_stop_step <- 1L
    window_stop <- 1L + params$after
    after_negative <- params$after < 0L
  }

  window_params <- list(
    window_start = window_start,
    window_stop = window_stop,
    window_start_step = window_start_step,
    window_stop_step = window_stop_step,
    before_negative = before_negative,
    after_negative = after_negative
  )

  params <- c(params, window_params)

  loop_new(.x, .f, params, ...)
}

# ------------------------------------------------------------------------------

loop_new <- function(x, f, params, ...) {
  out <- vec_init(params$ptype, params$n)

  while(params$position <= params$n) {
    if (params$complete && (params$window_start < 1L || params$window_stop > params$n)) {
      params <- increment_by_one_new(params)
      next
    }

    if (params$before_negative && params$window_start > params$n) {
      params <- increment_by_one_new(params)
      next
    }

    if (params$after_negative && params$window_stop < 1L) {
      params <- increment_by_one_new(params)
      next
    }

    bounded_window_start <- max(params$window_start, 1L)
    bounded_window_stop <- min(params$window_stop, params$n)

    out <- slice_eval_assign(out, x, f, bounded_window_start, bounded_window_stop, params, ...)

    params <- increment_by_step_new(params)
  }

  out
}

increment_by_one_new <- function(params) {
  params <- increment_by_one(params)
  params$window_start <- params$window_start + 1L
  params$window_stop <- params$window_stop + 1L
  params
}

increment_by_step_new <- function(params) {
  params <- increment_by_step(params)
  params$window_start <- params$window_start + params$window_start_step
  params$window_stop <- params$window_stop + params$window_stop_step
  params
}


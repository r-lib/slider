# ------------------------------------------------------------------------------

# Look backwards until index[[i]] is past current_index, then add 1 to position
# Additional check to see if the start of our window is
# already outside the last value (no data here)
locate_window_start_positive <- function(position, i, range_start, finish) {
  check_na_range(range_start, ".i - .before")

  while (i[[position]] >= range_start) {
    if (position == finish) {
      return(position)
    }

    position <- position - 1L
  }

  # Always goes 1 too far, so add it back afterwards
  position + 1L
}

# Look forward until index[[i]] is at or past current_index
locate_window_start_negative <- function(position, i, range_start, finish) {
  check_na_range(range_start, ".i + .before")

  if (range_start > i[[finish]]) {
    return(NA_integer_)
  }

  while (i[[position]] < range_start) {
    if (position == finish) {
      return(position)
    }

    position <- position + 1L
  }

  position
}

# Look forward until index[[i]] is past current_index, then subtract 1 from position
locate_window_stop_positive <- function(position, i, range_stop, finish) {
  check_na_range(range_stop, ".i + .after")

  while (i[[position]] <= range_stop) {
    if (position == finish) {
      return(position)
    }

    position <- position + 1L
  }

  # Always goes 1 too far, so back it off afterwards
  position - 1L
}

# Look backwards until index[[i]] is at or past current_index
# Additional check to see if the end of our window is
# already outside the first value (no data here)
locate_window_stop_negative <- function(position, i, range_stop, finish) {
  check_na_range(range_stop, ".i - .after")

  if (range_stop < i[[finish]]) {
    return(NA_integer_)
  }

  while (i[[position]] > range_stop) {
    if (position == finish) {
      return(position)
    }

    position <- position - 1L
  }

  position
}

check_na_range <- function(x, what) {
  if (is.na(x)) {
    abort(sprintf("`NA` value detected in the `%s` calculation.", what))
  }

  invisible(x)
}

# ------------------------------------------------------------------------------

locate_window_start <- function(position, i, range_start, n, positive) {
  if (positive) {
    locate_window_start_positive(position, i, range_start, 1L)
  } else {
    locate_window_start_negative(position, i, range_start, n)
  }
}

locate_window_stop <- function(position, i, range_stop, n, positive) {
  if (positive) {
    locate_window_stop_positive(position, i, range_stop, n)
  } else {
    locate_window_stop_negative(position, i, range_stop, 1L)
  }
}

# ------------------------------------------------------------------------------

compute_range_starts <- function(i, before, positive) {
  if (is_function(before)) {
    out <- before(i)
    return(out)
  }

  if (positive) {
    i - before
  } else {
    i + before
  }
}

compute_range_stops <- function(i, after, positive) {
  if (is_function(after)) {
    out <- after(i)
    return(out)
  }

  if (positive) {
    i + after
  } else {
    i - after
  }
}

# ------------------------------------------------------------------------------

check_index_size <- function(n, i) {
  n_i <- vec_size(i)

  if (n != n_i) {
    abort(
      sprintf("The size of `.x` (%i) and `.i` (%i) must be the same.", n, n_i)
    )
  }

  invisible()
}

separate_after <- function(params) {
  after <- params$after

  if (!vec_is(after)) {
    abort("When `.before` is `NULL`, `.after` must have type list.")
  }

  n <- vec_size(after)

  if (n != 2L) {
    abort(sprintf("When `.before` is `NULL`, `.after` must have size 2, not %i.", n))
  }

  params$before_positive <- FALSE
  params$before <- after[[1]]
  params$after <- after[[2]]

  if (is.numeric(params$before) && is.numeric(params$after)) {
    if (params$before > params$after) {
      abort(sprintf("When `.before` is `NULL`, the first `.after` value (%i) must be less than or equal to the second (%i).", params$before, params$after))
    }
  }

  params
}

separate_before <- function(params) {
  before <- params$before

  if (!vec_is(before)) {
    abort("When `.after` is `NULL`, `.before` must have type list.")
  }

  n <- vec_size(before)

  if (n != 2L) {
    abort(sprintf("When `.after` is `NULL`, `.before` must have size 2, not %i.", n))
  }

  params$after_positive <- FALSE
  params$before <- before[[1]]
  params$after <- before[[2]]

  if (is.numeric(params$before) && is.numeric(params$after)) {
    if (params$after > params$before) {
      abort(sprintf("When `.after` is `NULL`, the first `.before` value (%i) must be greater than or equal to the second (%i).", params$after, params$before))
    }
  }

  params
}

check_before_after <- function(params) {
  if (is.null(params$before)) {
    params <- separate_after(params)
  }

  if (is.null(params$after)) {
    params <- separate_before(params)
  }

  if (is_formula(params$before, scoped = TRUE, lhs = FALSE)) {
    params$before <- as_function(params$before)
  }

  if (is_formula(params$after, scoped = TRUE, lhs = FALSE)) {
    params$after <- as_function(params$after)
  }

  if (!is_function(params$before)) {
    vec_assert(params$before, size = 1L, arg = ".before")
  }

  if (!is_function(params$after)) {
    vec_assert(params$after, size = 1L, arg = ".after")
  }

  params
}

check_step <- function(params) {
  params$step <- vec_cast(params$step, integer())
  vec_assert(params$step, size = 1L, arg = ".step")
  params
}

check_params <- function(params) {
  params <- check_before_after(params)
  params <- check_step(params)
  params
}

# ------------------------------------------------------------------------------

slide_index_impl <- function(.x,
                             .i,
                             .f,
                             ...,
                             .before,
                             .after,
                             .step,
                             #.offset,
                             #.complete,
                             #.forward,
                             .constrain,
                             .ptype) {
  n <- vec_size(.x)
  .f <- as_function(.f)

  params <- list(
    before = .before,
    after = .after,
    step = .step,
    before_positive = TRUE,
    after_positive = TRUE
  )

  params <- check_params(params)

  range_starts <- compute_range_starts(.i, params$before, params$before_positive)
  range_stops <- compute_range_stops(.i, params$after, params$after_positive)

  position <- 1L
  entry <- 1L

  out <- vec_init(.ptype, n)

  while(position <= n) {
    range_start <- range_starts[[position]]
    range_stop <- range_stops[[position]]

    if (isTRUE(range_start > range_stop)) {
      range_start <- as.character(range_start)
      range_stop <- as.character(range_stop)
      abort(sprintf("In iteration %i, the start of the range, %s, cannot be after the end of the range, %s.", position, range_start, range_stop))
    }

    window_start <- locate_window_start(position, .i, range_start, n, params$before_positive)
    window_stop <- locate_window_stop(position, .i, range_stop, n, params$after_positive)

    if (is.na(window_start) || is.na(window_stop)) {
      # Step forward
      position <- position + params$step
      entry <- entry + params$step
      next
    }

    slice <- vec_slice(.x, seq2(window_start, window_stop))

    elt <- .f(slice, ...)

    if (.constrain) {
      elt <- vec_cast(elt, .ptype)

      if (vec_size(elt) != 1L) {
        abort(sprintf("The size of each result of `.f` must be size 1. Iteration %i was size %i.", position, vec_size(elt)))
      }

      out <- vec_assign(out, entry, elt)
    } else {
      out[[entry]] <- elt
    }

    # Step forward
    position <- position + params$step
    entry <- entry + params$step
  }

  out
}

# ------------------------------------------------------------------------------

slide_index <- function(.x,
                        .i,
                        .f,
                        ...,
                        .before = 0L,
                        .after = 0L,
                        .step = 1L
                        #.offset = NULL,
                        #.complete = FALSE,
                        #.forward = TRUE
                        ) {
  slide_index_impl(
    .x,
    .i,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .step = .step,
    # .offset = .offset,
    # .complete = .complete,
    # .forward = .forward,
    .constrain = FALSE,
    .ptype = list()
  )
}

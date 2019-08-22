slide_index_core <- function(x,
                             i,
                             f_call,
                             before,
                             after,
                             complete,
                             constrain,
                             ptype,
                             env,
                             type) {

  vec_assert(i)

  if (type == -1L) {
    size <- vec_size(x)
  } else {
    size <- vec_size(x[[1L]])
  }

  check_index_size(size, i)

  before <- check_before(before)
  after <- check_after(after)
  complete <- check_complete(complete)

  split <- vec_split_id(i)

  i <- split$key
  out_indices <- split$id

  check_index_na(i)

  before_unbounded <- is_unbounded(before)
  after_unbounded <- is_unbounded(after)

  range <- compute_range_info(i, before, after, before_unbounded, after_unbounded)
  starts <- range$starts
  stops <- range$stops

  i <- vec_cast(i, range$ptype)
  i <- vec_proxy_compare(i)

  # TODO
  # Do from here down in C
  params <- list(type, complete, before_unbounded, after_unbounded, constrain, size)
  .Call(slide_index_core_impl, x, i, starts, stops, out_indices, f_call, ptype, env, params)

  # i_size <- vec_size(i)
  #
  # # Will hopefully be able to do this quicker in C
  # if (!before_unbounded && !after_unbounded) {
  #   check_range_start_not_past_stop(starts, stops)
  # }
  #
  # # Number of unique index values
  # iteration_min <- 1L
  # iteration_max <- i_size
  #
  # window_sizes <- vapply(out_indices, vec_size, integer(1))
  # window_stops <- cumsum(window_sizes)
  # window_starts <- window_stops - window_sizes + 1L
  #
  # # Iteration adjustment
  # if (complete) {
  #   if (!before_unbounded) {
  #     iteration_min <- adjust_iteration_min(iteration_min, starts, i, i_size)
  #   }
  #   if (!after_unbounded) {
  #     iteration_max <- adjust_iteration_max(iteration_max, stops, i, i_size)
  #   }
  # } else {
  #   if (!before_unbounded) {
  #     iteration_max <- adjust_iteration_max(iteration_max, starts, i, i_size)
  #   }
  #   if (!after_unbounded) {
  #     iteration_min <- adjust_iteration_min(iteration_min, stops, i, i_size)
  #   }
  # }
  #
  # out <- vec_init(ptype, size)
  #
  # locate_window_start_index <- make_locate_window_start_index()
  # locate_window_stop_index <- make_locate_window_stop_index()
  #
  # window_start <- 1L
  # window_stop <- size
  #
  # for (iteration in seq2(iteration_min, iteration_max)) {
  #   if (!before_unbounded) {
  #     start <- vec_slice(starts, iteration)
  #     window_start_index <- locate_window_start_index(i, start, i_size)
  #     window_start <- window_starts[[window_start_index]]
  #   }
  #
  #   if (!after_unbounded) {
  #     stop <- vec_slice(stops, iteration)
  #     window_stop_index <- locate_window_stop_index(i, stop, i_size)
  #     window_stop <- window_stops[[window_stop_index]]
  #   }
  #
  #   # This can happen with an irregular index, and is a sign of the full window
  #   # being between two index points and means we select nothing
  #   if (window_stop < window_start) {
  #     window_start <- 0L
  #     window_stop <- 0L
  #   }
  #
  #   window <- seq2(window_start, window_stop)
  #
  #   env_bind(env, window = window)
  #
  #   elt <- eval_bare(f_call, env = env)
  #
  #   out_index <- out_indices[[iteration]]
  #
  #   if (constrain) {
  #     elt <- vec_cast(elt, ptype)
  #
  #     if (vec_size(elt) != 1L) {
  #       abort(sprintf("The size of each result of `.f` must be size 1. Iteration %i was size %i.", iteration, vec_size(elt)))
  #     }
  #
  #     out <- vec_assign(out, out_index, elt)
  #   } else {
  #     for (j in out_index) {
  #       out[[j]] <- elt
  #     }
  #   }
  #
  # }
  #
  # out
}

# ------------------------------------------------------------------------------

make_locate_window_start_index <- function() {
  previous_position <- 1L

  function(i, range_start, n) {
    position <- previous_position

    i_position <- vec_slice(i, position)

    while (vec_lt(i_position, range_start)) {
      if (position == n) {
        previous_position <<- position
        return(position)
      }

      position <- position + 1L

      i_position <- vec_slice(i, position)
    }

    previous_position <<- position
    position
  }
}

make_locate_window_stop_index <- function() {
  previous_position <- 1L

  function(i, range_stop, n) {
    position <- previous_position
    i_position <- vec_slice(i, position)

    while (vec_lte(i_position, range_stop)) {
      if (position == n) {
        previous_position <<- position
        return(position)
      }

      position <- position + 1L
      i_position <- vec_slice(i, position)
    }

    previous_position <<- position
    position - 1L
  }
}

# ------------------------------------------------------------------------------

compute_range_starts <- function(i, before) {
  if (is_function(before)) {
    out <- before(i)
    check_ascending(out, ".before")
    check_range_size(out, i, ".before")
  } else {
    out <- i - before
  }

  check_range_not_na(out, ".before")

  out
}

compute_range_stops <- function(i, after) {
  if (is_function(after)) {
    out <- after(i)
    check_ascending(out, ".after")
    check_range_size(out, i, ".after")
  } else {
    out <- i + after
  }

  check_range_not_na(out, ".after")

  out
}

check_ascending <- function(x, arg) {
  order <- vec_order(x, "asc")

  if (is.unsorted(order)) {
    at <- which(diff(order) < 0L)
    at <- collapse_and_trim(at)
    glubort(
      "The range generated by `{arg}` must be in ascending order. ",
      "At the following locations, it is not: {at}."
    )
  }

  invisible(x)
}

check_range_not_na <- function(x, arg) {
  na <- vec_equal_na(x)

  if (any(na)) {
    at <- which(na)
    at <- collapse_and_trim(at)
    glubort(
      "The range generated by `{arg}` cannot have `NA` values, ",
      "which were found at location(s): {at}."
    )
  }

  invisible(x)
}

check_range_size <- function(x, i, arg) {
  size_x <- vec_size(x)
  size_i <- vec_size(i)

  if (size_x != size_i) {
    glubort(
      "The range generated by `{arg}` has size {size_x}, ",
      "but must have the same size as the unique values of `.i`, {size_i}."
    )
  }

  invisible(x)
}

# ------------------------------------------------------------------------------

adjust_iteration_min <- function(iteration_min, range, i, size) {
  i_first <- vec_slice(i, 1L)
  range_first <- vec_slice(range, 1L)

  if (vec_gt(i_first, range_first)) {
    # should be able to optimize this away in C
    i_first <- vec_recycle(i_first, size)

    range_first_before <- vec_gt(i_first, range)
    forward_adjustment <- sum(range_first_before)

    iteration_min <- iteration_min + forward_adjustment
  }

  iteration_min
}

adjust_iteration_max <- function(iteration_max, range, i, size) {
  i_last <- vec_slice(i, size)
  range_last <- vec_slice(range, size)

  if (vec_lt(i_last, range_last)) {
    # should be able to optimize this away in C
    i_last <- vec_recycle(i_last, size)

    range_last_after <- vec_lt(i_last, range)
    backward_adjustment <- sum(range_last_after)

    iteration_max <- iteration_max - backward_adjustment
  }

  iteration_max
}

# ------------------------------------------------------------------------------

compute_range_info <- function(i, before, after, before_unbounded, after_unbounded) {
  starts <- NULL
  if (!before_unbounded) {
    starts <- compute_range_starts(i, before)
  }

  stops <- NULL
  if (!after_unbounded) {
    stops <- compute_range_stops(i, after)
  }

  ptype <- vec_ptype_common(i, starts, stops)

  if (!before_unbounded) {
    starts <- vec_cast(starts, ptype)
    starts <- vec_proxy_compare(starts)
  }

  if (!after_unbounded) {
    stops <- vec_cast(stops, ptype)
    stops <- vec_proxy_compare(stops)
  }

  list(starts = starts, stops = stops, ptype = ptype)
}

check_range_start_not_past_stop <- function(starts, stops) {
  start_after_stop <- vec_compare(starts, stops) == 1L

  if (any(start_after_stop)) {
    at <- which(start_after_stop)
    at <- collapse_and_trim(at)

    msg <- paste0(
      "In the ranges generated by `.before` and `.after`, ",
      "the start of the range is after the end of the range at location(s): {at}."
    )

    glubort(msg)
  }

  invisible()
}

# ------------------------------------------------------------------------------
# Should only ever pass objects of size 1 through here

vec_gt <- function(x, y) {
  .Call(vctrs:::vctrs_compare, x, y, FALSE) > 0L
  #vec_compare(x, y) > 0L
}

vec_gte <- function(x, y) {
  .Call(vctrs:::vctrs_compare, x, y, FALSE) >= 0L
  #vec_compare(x, y) >= 0L
}

vec_lt <- function(x, y) {
  .Call(vctrs:::vctrs_compare, x, y, FALSE) < 0L
  #vec_compare(x, y) < 0L
}

vec_lte <- function(x, y) {
  .Call(vctrs:::vctrs_compare, x, y, FALSE) <= 0L
  #vec_compare(x, y) <= 0L
}

# ------------------------------------------------------------------------------

check_index_size <- function(n, i) {
  n_i <- vec_size(i)

  if (n != n_i) {
    glubort("The size of `.x` ({n}) and `.i` ({n_i}) must be the same.")
  }

  invisible()
}

check_index_na <- function(i) {
  na <- vec_equal_na(i)

  if (any(na)) {
    at <- which(na)
    at <- collapse_and_trim(at)
    glubort("The `.i`ndex cannot have `NA` values, which were found at location(s): {at}.")
  }

  invisible(i)
}

check_before <- function(before) {
  if (is_unbounded(before)) {
    return(before)
  }

  if (is_formula(before, scoped = TRUE, lhs = FALSE)) {
    before <- as_function(before)
    return(before)
  }

  if (!is_function(before)) {
    vec_assert(before, size = 1L, arg = ".before")
  }

  before
}

check_after <- function(after) {
  if (is_unbounded(after)) {
    return(after)
  }

  if (is_formula(after, scoped = TRUE, lhs = FALSE)) {
    after <- as_function(after)
    return(after)
  }

  if (!is_function(after)) {
    vec_assert(after, size = 1L, arg = ".after")
  }

  after
}

check_complete <- function(complete) {
  complete <- vec_cast(complete, logical(), x_arg = ".complete")
  vec_assert(complete, size = 1L, arg = ".complete")
  complete
}

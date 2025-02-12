slide_index_common <- function(
  x,
  i,
  f_call,
  before,
  after,
  complete,
  ptype,
  constrain,
  atomic,
  env,
  type,
  slider_error_call
) {
  info <- slide_index_info(
    i = i,
    before = before,
    after = after,
    i_arg = ".i",
    before_arg = ".before",
    after_arg = ".after",
    slider_error_call = slider_error_call
  )

  x_size <- compute_size(x, type)
  i_size <- vec_size(i)

  if (i_size != x_size) {
    stop_index_incompatible_size(i_size, x_size, ".i", call = slider_error_call)
  }

  complete <- check_complete(complete, ".complete", call = slider_error_call)

  i <- info$i
  starts <- info$starts
  stops <- info$stops
  peer_sizes <- info$peer_sizes

  .Call(
    slide_index_common_impl,
    x,
    i,
    starts,
    stops,
    f_call,
    ptype,
    env,
    peer_sizes,
    type,
    constrain,
    atomic,
    x_size,
    complete
  )
}

# ------------------------------------------------------------------------------

slide_index_info <- function(
  i,
  before,
  after,
  i_arg,
  before_arg,
  after_arg,
  slider_error_call
) {
  vec_assert(i, arg = i_arg, call = slider_error_call)

  check_index_cannot_be_na(i, i_arg, call = slider_error_call)
  check_index_must_be_ascending(i, i_arg, call = slider_error_call)

  # `i` is ascending, so we can detect uniques quickly with `vec_unrep()`.
  # We must unrep before applying `before`/`after`, as we expect that they are
  # only applied on the unique values of `i`.
  # Otherwise, the same value of `i` could have different start/stop values,
  # like `c(1, 1) - c(2, 3)`).
  unrep <- vec_unrep(i)
  i <- unrep$key

  before <- check_before(before, before_arg, call = slider_error_call)
  after <- check_after(after, after_arg, call = slider_error_call)

  ranges <- compute_ranges(
    i = i,
    before = before,
    after = after,
    i_arg = i_arg,
    before_arg = before_arg,
    after_arg = after_arg,
    slider_error_call = slider_error_call
  )

  list(
    i = ranges$i,
    starts = ranges$starts,
    stops = ranges$stops,
    peer_sizes = unrep$times
  )
}

compute_ranges <- function(
  i,
  before,
  after,
  i_arg,
  before_arg,
  after_arg,
  slider_error_call
) {
  i_size <- vec_size(i)

  start_unbounded <- before$unbounded
  stop_unbounded <- after$unbounded

  # Setting to `NULL`, as that is what the C level new_range_info() expects
  # for unbounded start / stop ranges
  if (start_unbounded) {
    starts <- NULL
  } else {
    starts <- before$fn(i)
    starts <- vec_cast(starts, i, to_arg = ".i", call = slider_error_call)
    check_generated_endpoints_incompatible_size(
      starts,
      i_size,
      before_arg,
      call = slider_error_call
    )
    check_generated_endpoints_cannot_be_na(
      starts,
      before_arg,
      call = slider_error_call
    )
    check_generated_endpoints_must_be_ascending(
      starts,
      before_arg,
      call = slider_error_call
    )
  }

  if (stop_unbounded) {
    stops <- NULL
  } else {
    stops <- after$fn(i)
    stops <- vec_cast(stops, i, to_arg = ".i", call = slider_error_call)
    check_generated_endpoints_incompatible_size(
      stops,
      i_size,
      after_arg,
      call = slider_error_call
    )
    check_generated_endpoints_cannot_be_na(
      stops,
      after_arg,
      call = slider_error_call
    )
    check_generated_endpoints_must_be_ascending(
      stops,
      after_arg,
      call = slider_error_call
    )
  }

  ranks <- compute_combined_ranks(i = i, starts = starts, stops = stops)
  i <- ranks$i
  if (!start_unbounded) {
    starts <- ranks$starts
  }
  if (!stop_unbounded) {
    stops <- ranks$stops
  }

  list(i = i, starts = starts, stops = stops)
}

# ------------------------------------------------------------------------------

check_before <- function(before, before_arg, call = caller_env()) {
  if (is_function(before)) {
    unbounded <- FALSE
    fn <- before
  } else if (is_formula(before)) {
    unbounded <- FALSE
    fn <- as_function(before, arg = before_arg, call = call)
  } else {
    vec_assert(before, size = 1L, arg = before_arg, call = call)
    unbounded <- is_unbounded(before)
    fn <- function(i) slider_minus(i, before)
  }

  list(fn = fn, unbounded = unbounded)
}

check_after <- function(after, after_arg, call = caller_env()) {
  if (is_function(after)) {
    unbounded <- FALSE
    fn <- after
  } else if (is_formula(after)) {
    unbounded <- FALSE
    fn <- as_function(after, arg = after_arg, call = call)
  } else {
    vec_assert(after, size = 1L, arg = after_arg, call = call)
    unbounded <- is_unbounded(after)
    fn <- function(i) slider_plus(i, after)
  }

  list(fn = fn, unbounded = unbounded)
}

check_complete <- function(complete, complete_arg, call = caller_env()) {
  complete <- vec_cast(complete, logical(), x_arg = complete_arg, call = call)
  vec_assert(complete, size = 1L, arg = complete_arg, call = call)
  complete
}

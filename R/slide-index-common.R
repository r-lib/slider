slide_index_common <- function(x,
                               i,
                               f_call,
                               before,
                               after,
                               complete,
                               ptype,
                               constrain,
                               atomic,
                               env,
                               type) {
  info <- slide_index_info(i, before, after)

  x_size <- compute_size(x, type)
  i_size <- vec_size(i)

  if (i_size != x_size) {
    stop_index_incompatible_size(i_size, x_size, ".i")
  }

  complete <- check_complete(complete)

  i <- info$i
  starts <- info$starts
  stops <- info$stops
  indices <- info$indices

  .Call(
    slide_index_common_impl,
    x,
    i,
    starts,
    stops,
    f_call,
    ptype,
    env,
    indices,
    type,
    constrain,
    atomic,
    x_size,
    complete
  )
}

# ------------------------------------------------------------------------------

slide_index_info <- function(i, before, after) {
  vec_assert(i)

  check_index_cannot_be_na(i, ".i")
  check_index_must_be_ascending(i, ".i")

  check_before(before)
  check_after(after)

  # Compute unique values of `i` to avoid repeated evaluations of `.f`
  split <- vec_group_loc(i)
  i <- split$key

  ranges <- compute_ranges(i, before, after)

  list(
    i = ranges$i,
    starts = ranges$starts,
    stops = ranges$stops,
    indices = split$loc
  )
}

compute_ranges <- function(i, before, after) {
  start_unbounded <- is_unbounded(before)
  stop_unbounded <- is_unbounded(after)

  # Setting to `NULL`, as that is what the C level new_range_info() expects
  # for unbounded start / stop ranges
  if (start_unbounded) {
    starts <- NULL
  } else {
    starts <- i - before
    check_generated_endpoints_cannot_be_na(starts, ".before")
  }

  if (stop_unbounded) {
    stops <- NULL
  } else {
    stops <- i + after
    check_generated_endpoints_cannot_be_na(stops, ".after")
  }

  ptype <- vec_ptype_common(i, starts, stops)

  if (!start_unbounded) {
    starts <- vec_cast(starts, ptype)
    starts <- vec_proxy_compare(starts)
  }

  if (!stop_unbounded) {
    stops <- vec_cast(stops, ptype)
    stops <- vec_proxy_compare(stops)
  }

  i <- vec_cast(i, ptype)
  i <- vec_proxy_compare(i)

  list(i = i, starts = starts, stops = stops)
}

# ------------------------------------------------------------------------------

check_before <- function(before) {
  vec_assert(before, size = 1L, arg = ".before")
  invisible(before)
}

check_after <- function(after) {
  vec_assert(after, size = 1L, arg = ".after")
  invisible(after)
}

check_complete <- function(complete) {
  complete <- vec_cast(complete, logical(), x_arg = ".complete")
  vec_assert(complete, size = 1L, arg = ".complete")
  complete
}

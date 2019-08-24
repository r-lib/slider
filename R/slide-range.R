slide_range <- function(.x, .i, .starts, .stops, .f, ...) {
  slide_range_impl(
    .x,
    .i,
    .starts,
    .stops,
    .f,
    ...,
    .constrain = FALSE,
    .ptype = list()
  )
}

slide_range_impl <- function(.x, .i, .starts, .stops, .f, ..., .constrain, .ptype) {
  vec_assert(.x)

  .f <- as_function(.f)

  f_call <- expr(.f(.x, ...))

  type <- -1L

  slide_range_core(
    x = .x,
    i = .i,
    starts = .starts,
    stops = .stops,
    f_call = f_call,
    constrain = .constrain,
    ptype = .ptype,
    env = environment(),
    type = type
  )
}

slide_range_core <- function(x, i, starts, stops, f_call, constrain, ptype, env, type) {

  x_size <- compute_size(x, type)

  check_index_size(x_size, i)
  check_index_not_na(i)
  check_index_ascending(i)

  # TODO - better message
  check_range_not_na(starts, ".starts")
  check_range_not_na(stops, ".stops")

  # TODO - better message
  check_ascending(starts, ".starts")
  check_ascending(stops, ".stops")

  out_size <- vec_size(starts)
  if (out_size != vec_size(stops)) {
    abort("size of `starts` and `stops` must be identical.")
  }

  args <- vec_cast_common(i, starts, stops)
  i <- args[[1L]]
  starts <- args[[2L]]
  stops <- args[[3L]]

  i <- vec_proxy_compare(i)
  starts <- vec_proxy_compare(starts)
  stops <- vec_proxy_compare(stops)

  split <- vec_split_id(i)
  i <- split$key
  window_indices <- split$id

  # Get unique values based on the `starts` and `stops`, not the `i`
  ranges <- data.frame(starts = starts, stops = stops)
  split <- vec_split_id(ranges)

  ranges <- split$key
  starts <- ranges$starts
  stops <- ranges$stops

  out_indices <- split$id

  params <- list(
    type,
    constrain,
    out_size
  )

  slide_range_bare(
    x, i, starts, stops, f_call, ptype, env, out_indices, window_indices, params
  )
}

slide_range_bare <- function(x, i, starts, stops, f_call, ptype, env, out_indices, window_indices, params) {
  .Call(
    slide_range_bare_impl,
    x,
    i,
    starts,
    stops,
    f_call,
    ptype,
    env,
    out_indices,
    window_indices,
    params
  )
}

slide_between <- function(.x, .i, .starts, .stops, .f, ...) {
  slide_between_impl(
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

slide_between_impl <- function(.x, .i, .starts, .stops, .f, ..., .constrain, .ptype) {
  vec_assert(.x)

  .f <- as_function(.f)

  f_call <- expr(.f(.x, ...))

  type <- -1L

  slide_between_common(
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

slide_between_common <- function(x, i, starts, stops, f_call, constrain, ptype, env, type) {

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

  # Can be different from `out_size` because this is computed
  # after using `vec_split_id()`
  size_starts <- vec_size(starts)

  out_indices <- split$id

  # All false for slide_between()
  complete <- FALSE
  before_unbounded <- FALSE
  after_unbounded <- FALSE

  params <- list(
    type,
    constrain,
    out_size,
    complete,
    before_unbounded,
    after_unbounded,
    size_starts
  )

  slide_between_base(
    x = x,
    i = i,
    starts = starts,
    stops = stops,
    f_call = f_call,
    ptype = ptype,
    env = env,
    out_indices = out_indices,
    window_indices = window_indices,
    params = params
  )
}

slide_between_base <- function(x, i, starts, stops, f_call, ptype, env, out_indices, window_indices, params) {
  .Call(
    slide_between_base_impl,
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

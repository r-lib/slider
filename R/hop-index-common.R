hop_index_common <- function(x,
                             i,
                             starts,
                             stops,
                             f_call,
                             ptype,
                             constrain,
                             atomic,
                             env,
                             type) {

  x_size <- compute_size(x, type)
  i_size <- vec_size(i)

  if (i_size != x_size) {
    stop_index_incompatible_size(i_size, x_size, ".i")
  }

  check_index_cannot_be_na(i, ".i")
  check_index_must_be_ascending(i, ".i")

  check_endpoints_cannot_be_na(starts, ".starts")
  check_endpoints_must_be_ascending(starts, ".starts")

  check_endpoints_cannot_be_na(stops, ".stops")
  check_endpoints_must_be_ascending(stops, ".stops")

  size <- vec_size_common(starts, stops)

  args <- vec_recycle_common(starts, stops, .size = size)
  args <- vec_cast_common(i, !!!args)
  args <- lapply(args, vec_proxy_compare)

  i <- args[[1L]]
  starts <- args[[2L]]
  stops <- args[[3L]]

  split <- vec_group_loc(i)
  i <- split$key
  window_indices <- split$loc

  .Call(
    hop_index_common_impl,
    x,
    i,
    starts,
    stops,
    f_call,
    ptype,
    env,
    window_indices,
    type,
    constrain,
    atomic,
    size
  )
}

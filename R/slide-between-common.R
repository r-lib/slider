slide_between_common <- function(x,
                                 i,
                                 starts,
                                 stops,
                                 f_call,
                                 constrain,
                                 ptype,
                                 env,
                                 type) {

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
  args <- lapply(args, vec_proxy_compare)

  i <- args[[1L]]
  starts <- args[[2L]]
  stops <- args[[3L]]

  split <- vec_split_id(i)
  i <- split$key
  window_indices <- split$id

  params <- list(
    type,
    constrain,
    out_size
  )

  .Call(
    slide_between_common_impl,
    x,
    i,
    starts,
    stops,
    f_call,
    ptype,
    env,
    window_indices,
    params
  )
}

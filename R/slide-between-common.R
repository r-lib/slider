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
  check_not_na(i, "`.i`")
  check_index_ascending(i)

  check_not_na(starts, "`.starts`")
  check_ascending(starts, "`.starts`")

  check_not_na(stops, "`.stops`")
  check_ascending(stops, "`.stops`")

  out_size <- vec_size_common(starts, stops)

  args <- vec_recycle_common(starts, stops, .size = out_size)
  args <- vec_cast_common(i, !!!args)
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

hop_common <- function(x, starts, stops, f_call, ptype, env, type, constrain) {
  x_size <- compute_size(x, type)

  check_not_na(starts, "`.starts`")
  check_not_na(stops, "`.stops`")

  # TODO `convert_values = "none"` would be nice to disallow negative positions
  starts <- vec_coerce_index(starts, allow_types = "position", arg = ".starts")
  stops <- vec_coerce_index(stops, allow_types = "position", arg = ".stops")

  size <- vec_size_common(starts, stops)
  args <- vec_recycle_common(starts, stops, .size = size)

  # Early exit if empty input
  # (but after all size checks have been done)
  if (size == 0L) {
    return(vec_init(ptype, 0L))
  }

  if (x_size == 0L) {
    return(vec_init(ptype, size))
  }

  starts <- args[[1L]]
  stops <- args[[2L]]

  params <- list(
    type = type,
    constrain = constrain
  )

  .Call(hop_common_impl, x, starts, stops, f_call, ptype, env, params)
}

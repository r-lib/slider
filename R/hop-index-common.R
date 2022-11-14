hop_index_common <- function(x,
                             i,
                             starts,
                             stops,
                             f_call,
                             ptype,
                             constrain,
                             atomic,
                             env,
                             type,
                             slider_error_call) {

  x_size <- compute_size(x, type)
  i_size <- vec_size(i)

  if (i_size != x_size) {
    stop_index_incompatible_size(i_size, x_size, ".i", call = slider_error_call)
  }

  check_index_cannot_be_na(i, ".i", call = slider_error_call)
  check_index_must_be_ascending(i, ".i", call = slider_error_call)

  check_endpoints_cannot_be_na(starts, ".starts", call = slider_error_call)
  check_endpoints_must_be_ascending(starts, ".starts", call = slider_error_call)

  check_endpoints_cannot_be_na(stops, ".stops", call = slider_error_call)
  check_endpoints_must_be_ascending(stops, ".stops", call = slider_error_call)

  # `i` is known to be ascending,
  # so we can detect uniques very quickly with `vec_unrep()`
  unrep <- vec_unrep(i)
  i <- unrep$key
  peer_sizes <- unrep$times

  starts <- vec_cast(starts, i, x_arg = ".starts", to_arg = ".i", call = slider_error_call)
  stops <- vec_cast(stops, i, x_arg = ".stops", to_arg = ".i", call = slider_error_call)

  size <- vec_size_common(
    .starts = starts,
    .stops = stops,
    .call = slider_error_call
  )
  args <- vec_recycle_common(
    .starts = starts,
    .stops = stops,
    .size = size,
    .call = slider_error_call
  )
  starts <- args$.starts
  stops <- args$.stops

  args <- compute_combined_ranks(i = i, starts = starts, stops = stops)
  i <- args$i
  starts <- args$starts
  stops <- args$stops

  .Call(
    hop_index_common_impl,
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
    size
  )
}

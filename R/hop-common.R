hop_common <- function(x, starts, stops, f_call, ptype, env, type, constrain, atomic, slider_error_call) {
  x_size <- compute_size(x, type)

  check_endpoints_cannot_be_na(starts, ".starts", call = slider_error_call)
  check_endpoints_cannot_be_na(stops, ".stops", call = slider_error_call)

  starts <- vec_as_subscript(
    starts,
    logical = "error",
    character = "error",
    arg = ".starts",
    call = slider_error_call
  )
  stops <- vec_as_subscript(
    stops,
    logical = "error",
    character = "error",
    arg = ".stops",
    call = slider_error_call
  )

  args <- vec_recycle_common(
    starts = starts,
    stops = stops,
    .call = slider_error_call
  )

  starts <- args[[1L]]
  stops <- args[[2L]]

  params <- list(
    type = type,
    constrain = constrain,
    atomic = atomic
  )

  .Call(hop_common_impl, x, starts, stops, f_call, ptype, env, params)
}

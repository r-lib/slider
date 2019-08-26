slide_common <- function(x, f_call, ptype, env, params) {
  .Call(slide_common_impl, x, f_call, ptype, env, params)
}

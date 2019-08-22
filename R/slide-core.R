slide_core <- function(x, f_call, ptype, env, params) {
  .Call(slide_core_impl, x, f_call, ptype, env, params)
}

slide_core <- function(x, f_call, ptype, env, param_list) {
  .Call(slide_core_impl, x, f_call, ptype, env, param_list)
}

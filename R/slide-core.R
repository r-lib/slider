slide_core <- function(x, f_call, ptype, env, param_list) {
  .Call(slurrr_slide, x, f_call, ptype, env, param_list)
}

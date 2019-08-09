slide_core <- function(x,
                       f_call,
                       ptype,
                       env,
                       type,
                       .size,
                       .before,
                       .after,
                       .step,
                       .offset,
                       .complete,
                       .forward,
                       .constrain) {

  param_list <- list(
    type, # type
    .size,
    .constrain,
    .before,
    .after,
    .step,
    .complete,
    .forward,
    .offset
  )

  .Call(slurrr_slide, x, f_call, ptype, env, param_list)
}

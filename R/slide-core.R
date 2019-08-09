slide_core <- function(.x,
                       type,
                       .f_call,
                       .size,
                       .before,
                       .after,
                       .step,
                       .offset,
                       .complete,
                       .forward,
                       .ptype,
                       .constrain,
                       .env) {

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

  .Call(slurrr_slide, .x, .f_call, .ptype, .env, param_list)
}

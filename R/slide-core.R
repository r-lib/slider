slide_core <- function(.x,
                       .inputs,
                       .f_call,
                       .size,
                       .before,
                       .after,
                       .step,
                       .offset,
                       .complete,
                       .dir,
                       .ptype,
                       .constrain,
                       .env) {

  # change to `.forward`
  arg_match(.dir, valid_dir())
  forward <- .dir == "forward"

  param_list <- list(
    .inputs, # type
    .size,
    .constrain,
    .before,
    .after,
    .step,
    .complete,
    forward,
    .offset
  )

  .Call(slurrr_slide, .x, .f_call, .ptype, .env, param_list)
}

# ------------------------------------------------------------------------------

valid_dir <- function() {
  c("forward", "backward")
}

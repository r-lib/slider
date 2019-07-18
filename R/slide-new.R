slide <- function(.x,
                  .f,
                  ...,
                  .before = 0L,
                  .after = 0L,
                  .step = 1L,
                  .offset = NULL,
                  .complete = FALSE,
                  .dir = "forward",
                  .ptype = list()) {

  if (is.null(.ptype)) {
    .ptype <- list()
    simplify <- TRUE
  } else {
    simplify <- FALSE
  }

  out <- slide_new_impl(
    .x,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .step = .step,
    .offset = .offset,
    .complete = .complete,
    .dir = .dir,
    .ptype = .ptype
  )

  if (simplify) {
    out <- vec_simplify(out)
  }

  out
}

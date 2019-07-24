#' @rdname slide2
#' @export
pslide <- function(.l,
                   .f,
                   ...,
                   .before = 0L,
                   .after = 0L,
                   .step = 1L,
                   .offset = NULL,
                   .complete = FALSE,
                   .dir = "forward") {
  pslide_impl(
    .l,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .step = .step,
    .offset = .offset,
    .complete = .complete,
    .dir = .dir,
    .ptype = list(),
    .constrain = FALSE
  )
}

#' @rdname slide2
#' @export
pslide_vec <- function(.l,
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
    out <- pslide_vec_simplify(
      .l,
      .f,
      ...,
      .before = .before,
      .after = .after,
      .step = .step,
      .offset = .offset,
      .complete = .complete,
      .dir = .dir
    )

    return(out)
  }

  pslide_impl(
    .l,
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
}

pslide_vec_simplify <- function(.l,
                                .f,
                                ...,
                                .before,
                                .after,
                                .step,
                                .offset,
                                .complete,
                                .dir) {
  out <- pslide(
    .l,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .step = .step,
    .offset = .offset,
    .complete = .complete,
    .dir = .dir
  )

  if (vec_size_common(!!!out) != 1L) {
    glubort("The size of all results from `.f` must be 1.")
  }

  vec_c(!!!out)
}

pslide_vec_handoff <- function(.ptype, ..., .env = caller_env()) {
  args <- env_get_list(
    env = .env,
    nms = c(
      ".l",
      ".f",
      ".before",
      ".after",
      ".step",
      ".offset",
      ".complete",
      ".dir"
    )
  )

  pslide_vec_call <- expr(pslide_vec(!!! args, ..., .ptype = .ptype))

  eval_bare(pslide_vec_call)
}

#' @rdname slide2
#' @export
pslide_dbl <- function(.l,
                       .f,
                       ...,
                       .before = 0L,
                       .after = 0L,
                       .step = 1L,
                       .offset = NULL,
                       .complete = FALSE,
                       .dir = "forward") {
  pslide_vec_handoff(dbl(), ...)
}

#' @inheritParams vctrs::vec_rbind
#' @rdname slide2
#' @export
pslide_dfr <- function(.l,
                      .f,
                      ...,
                      .before = 0L,
                      .after = 0L,
                      .step = 1L,
                      .offset = NULL,
                      .complete = FALSE,
                      .dir = "forward",
                      .names_to = NULL,
                      .name_repair = c("unique", "universal", "check_unique")) {
  out <- pslide(
    .l,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .step = .step,
    .offset = .offset,
    .complete = .complete,
    .dir = .dir
  )

  vec_rbind(!!!out, .names_to = .names_to, .name_repair = .name_repair)
}

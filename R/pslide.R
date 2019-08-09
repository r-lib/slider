#' @include slide2.R
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
                   .forward = TRUE) {
  pslide_impl(
    .l,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .step = .step,
    .offset = .offset,
    .complete = .complete,
    .forward = .forward,
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
                       .forward = TRUE,
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
      .forward = .forward
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
    .forward = .forward,
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
                                .forward) {
  out <- pslide(
    .l,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .step = .step,
    .offset = .offset,
    .complete = .complete,
    .forward = .forward
  )

  if (vec_size_common(!!!out) != 1L) {
    abort("The size of all results from `.f` must be 1.")
  }

  vec_c(!!!out)
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
                       .forward = TRUE) {
  pslide_vec(
    .l,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .step = .step,
    .offset = .offset,
    .complete = .complete,
    .forward = .forward,
    .ptype = double()
  )
}

#' @rdname slide2
#' @export
pslide_int <- function(.l,
                       .f,
                       ...,
                       .before = 0L,
                       .after = 0L,
                       .step = 1L,
                       .offset = NULL,
                       .complete = FALSE,
                       .forward = TRUE) {
  pslide_vec(
    .l,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .step = .step,
    .offset = .offset,
    .complete = .complete,
    .forward = .forward,
    .ptype = integer()
  )
}

#' @rdname slide2
#' @export
pslide_lgl <- function(.l,
                       .f,
                       ...,
                       .before = 0L,
                       .after = 0L,
                       .step = 1L,
                       .offset = NULL,
                       .complete = FALSE,
                       .forward = TRUE) {
  pslide_vec(
    .l,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .step = .step,
    .offset = .offset,
    .complete = .complete,
    .forward = .forward,
    .ptype = logical()
  )
}

#' @rdname slide2
#' @export
pslide_chr <- function(.l,
                       .f,
                       ...,
                       .before = 0L,
                       .after = 0L,
                       .step = 1L,
                       .offset = NULL,
                       .complete = FALSE,
                       .forward = TRUE) {
  pslide_vec(
    .l,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .step = .step,
    .offset = .offset,
    .complete = .complete,
    .forward = .forward,
    .ptype = character()
  )
}

#' @rdname slide2
#' @export
pslide_raw <- function(.l,
                       .f,
                       ...,
                       .before = 0L,
                       .after = 0L,
                       .step = 1L,
                       .offset = NULL,
                       .complete = FALSE,
                       .forward = TRUE) {
  pslide_vec(
    .l,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .step = .step,
    .offset = .offset,
    .complete = .complete,
    .forward = .forward,
    .ptype = raw()
  )
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
                       .forward = TRUE,
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
    .forward = .forward
  )

  vec_rbind(!!!out, .names_to = .names_to, .name_repair = .name_repair)
}

#' @inheritParams vctrs::vec_cbind
#' @rdname slide2
#' @export
pslide_dfc <- function(.l,
                       .f,
                       ...,
                       .before = 0L,
                       .after = 0L,
                       .step = 1L,
                       .offset = NULL,
                       .complete = FALSE,
                       .forward = TRUE,
                       .size = NULL,
                       .name_repair = c("unique", "universal", "check_unique", "minimal")) {
  out <- pslide(
    .l,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .step = .step,
    .offset = .offset,
    .complete = .complete,
    .forward = .forward
  )

  vec_cbind(!!!out, .size = .size, .name_repair = .name_repair)
}

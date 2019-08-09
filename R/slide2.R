#' Slide over multiple inputs simultaneously
#'
#' These are variants of [slide()] that iterate over multiple inputs in
#' parallel. They are parallel in the sense that each input is processed in
#' parallel with the others, not in the sense of multicore computing. These
#' functions work similarly to `map2()` and `pmap()` from purrr.
#'
#' @inheritParams slide
#'
#' @param .x,.y `[vector]` Vectors to iterate over. Vectors of size 1 will
#'   be recycled.
#'
#' @param .l `[list]` A list of vectors. The length of `.l` determines the
#'   number of arguments that `.f` will be called with. If `.l` has names,
#'   they will be used as named arguments to `.f`.
#'
#' @export
slide2 <- function(.x,
                   .y,
                   .f,
                   ...,
                   .before = 0L,
                   .after = 0L,
                   .step = 1L,
                   .offset = NULL,
                   .complete = FALSE,
                   .forward = TRUE) {
  slide2_impl(
    .x,
    .y,
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
slide2_vec <- function(.x,
                       .y,
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
    out <- slide2_vec_simplify(
      .x,
      .y,
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

  slide2_impl(
    .x,
    .y,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .step = .step,
    .offset = .offset,
    .complete = .complete,
    .forward = .forward,
    .ptype = .ptype,
    .constrain = TRUE
  )
}

slide2_vec_simplify <- function(.x,
                                .y,
                                .f,
                                ...,
                                .before,
                                .after,
                                .step,
                                .offset,
                                .complete,
                                .forward) {
  out <- slide2(
    .x,
    .y,
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
slide2_dbl <- function(.x,
                       .y,
                       .f,
                       ...,
                       .before = 0L,
                       .after = 0L,
                       .step = 1L,
                       .offset = NULL,
                       .complete = FALSE,
                       .forward = TRUE) {
  slide2_vec(
    .x,
    .y,
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
slide2_int <- function(.x,
                       .y,
                       .f,
                       ...,
                       .before = 0L,
                       .after = 0L,
                       .step = 1L,
                       .offset = NULL,
                       .complete = FALSE,
                       .forward = TRUE) {
  slide2_vec(
    .x,
    .y,
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
slide2_lgl <- function(.x,
                       .y,
                       .f,
                       ...,
                       .before = 0L,
                       .after = 0L,
                       .step = 1L,
                       .offset = NULL,
                       .complete = FALSE,
                       .forward = TRUE) {
  slide2_vec(
    .x,
    .y,
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
slide2_chr <- function(.x,
                       .y,
                       .f,
                       ...,
                       .before = 0L,
                       .after = 0L,
                       .step = 1L,
                       .offset = NULL,
                       .complete = FALSE,
                       .forward = TRUE) {
  slide2_vec(
    .x,
    .y,
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
slide2_raw <- function(.x,
                       .y,
                       .f,
                       ...,
                       .before = 0L,
                       .after = 0L,
                       .step = 1L,
                       .offset = NULL,
                       .complete = FALSE,
                       .forward = TRUE) {
  slide2_vec(
    .x,
    .y,
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
slide2_dfr <- function(.x,
                       .y,
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
  out <- slide2(
    .x,
    .y,
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
slide2_dfc <- function(.x,
                       .y,
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
  out <- slide2(
    .x,
    .y,
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

#' Slide
#'
#'
#'
#' @param .x A list or atomic vector.
#'
#' @param .f A function or formula.
#'
#' @param ... Additional arguments passed on to the mapped function.
#'
#' @param .size A single positive integer. The size of the sliding window.
#'
#' @param .step A single positive integer. The size of the step between each
#'   function evaluation.
#'
#' @param .align The alignment to begin the placement of results in the output
#' vector.
#'
#'   One of:
#'   * `"right"`
#'   * `"left"`
#'   * `"center"`
#'   * `"center-left"`
#'   * `"center-right"`
#'
#' When `.size` is even, `"center"` is assumed to be
#' equivalent to `"center-left"`.
#'
#' @param .partial Should partial results be computed?
#' If `TRUE` and `.dir == "forward"`, this may result in partial results at the
#' _end_ of the vector. If `.dir == "backwards"`, this may result in partial
#' results at the _start_ of the vector.
#'
#' @param .dir The direction to slide.
#'
#' One of:
#' - `"forward"`
#' - `"backward"`
#'
#' @export
slide <- function(.x,
                  .f,
                  ...,
                  .before = 0L,
                  .after = 0L,
                  .step = 1L,
                  .offset = NULL,
                  .partial = FALSE,
                  .dir = "forward") {
  slide_impl(
    .x,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .step = .step,
    .offset = .offset,
    .partial = .partial,
    .dir = .dir,
    .ptype = list()
  )
}

#' @rdname slide
#' @export
slide_dbl <- function(.x,
                      .f,
                      ...,
                      .before = 0L,
                      .after = 0L,
                      .step = 1L,
                      .offset = NULL,
                      .partial = FALSE,
                      .dir = "forward") {
  slide_impl(
    .x,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .step = .step,
    .offset = .offset,
    .partial = .partial,
    .dir = .dir,
    .ptype = double()
  )
}

#' @rdname slide
#' @export
slide_dfr <- function(.x,
                      .f,
                      ...,
                      .before = 0L,
                      .after = 0L,
                      .step = 1L,
                      .offset = NULL,
                      .partial = FALSE,
                      .dir = "forward",
                      .names_to = NULL,
                      .name_repair = c("unique", "universal", "check_unique")) {
  out <- slide(
    .x,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .step = .step,
    .offset = .offset,
    .partial = .partial,
    .dir = .dir
  )

  vec_rbind(!!!out, .names_to = .names_to, .name_repair = .name_repair)
}

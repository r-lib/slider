#' Slide along multiple inputs simultaneously relative to an index chunked by period
#'
#' `slide_period2()` and `pslide_period()` represent the combination
#' of [slide2()] and [pslide()] with [slide_period()], allowing you to slide
#' over multiple vectors at once, using indices defined by breaking up the
#' `.i`-ndex by `.period`.
#'
#' @inheritParams slide_period
#'
#' @template param-x-y
#' @template param-l
#'
#' @return
#' A vector fulfilling the following invariants:
#'
#' \subsection{`slide_period2()`}{
#'
#'  * `vec_size(slide_period2(.x, .y)) == vec_size(unique(warp::warp_distance(.i)))`
#'
#'  * `vec_ptype(slide_period2(.x, .y)) == list()`
#'
#' }
#'
#' \subsection{`slide_period2_vec()` and `slide_period2_*()` variants}{
#'
#'  * `vec_size(slide_period2_vec(.x, .y)) == vec_size(unique(warp::warp_distance(.i)))`
#'
#'  * `vec_size(slide_period2_vec(.x, .y)[[1]]) == 1L`
#'
#'  * `vec_ptype(slide_period2_vec(.x, .y, .ptype = ptype)) == ptype`
#'
#' }
#'
#' \subsection{`pslide_period()`}{
#'
#'  * `vec_size(pslide_period(.l)) == vec_size(unique(warp::warp_distance(.i)))`
#'
#'  * `vec_ptype(pslide_period(.l)) == list()`
#'
#' }
#'
#' \subsection{`pslide_period_vec()` and `pslide_period_*()` variants}{
#'
#'  * `vec_size(pslide_period_vec(.l)) == vec_size(unique(warp::warp_distance(.i)))`
#'
#'  * `vec_size(pslide_period_vec(.l)[[1]]) == 1L`
#'
#'  * `vec_ptype(pslide_period_vec(.l, .ptype = ptype)) == ptype`
#'
#' }
#'
#' @examples
#' i <- as.Date("2019-01-28") + 0:5
#'
#' slide_period2(
#'   .x = 1:6,
#'   .y = i,
#'   .i = i,
#'   .period = "month",
#'   .f = ~data.frame(x = .x, i = .y)
#' )
#'
#' pslide_period(
#'   .l = list(1:6, 7:12, i),
#'   .i = i,
#'   .period = "month",
#'   .f = ~data.frame(x = .x, y = .y, i = ..3)
#' )
#'
#' @seealso [slide2()], [slide_index2()], [slide_period()]
#' @export
slide_period2 <- function(.x,
                          .y,
                          .i,
                          .period,
                          .f,
                          ...,
                          .every = 1L,
                          .origin = NULL,
                          .before = 0L,
                          .after = 0L,
                          .complete = FALSE) {
  slide_period2_impl(
    .x,
    .y,
    .i,
    .period,
    .f,
    ...,
    .every = .every,
    .origin = .origin,
    .before = .before,
    .after = .after,
    .complete = .complete,
    .ptype = list(),
    .constrain = FALSE,
    .atomic = FALSE
  )
}

#' @rdname slide_period2
#' @export
slide_period2_vec <- function(.x,
                              .y,
                              .i,
                              .period,
                              .f,
                              ...,
                              .every = 1L,
                              .origin = NULL,
                              .before = 0L,
                              .after = 0L,
                              .complete = FALSE,
                              .ptype = NULL) {
  out <- slide_period2_impl(
    .x,
    .y,
    .i,
    .period,
    .f,
    ...,
    .every = .every,
    .origin = .origin,
    .before = .before,
    .after = .after,
    .complete = .complete,
    .ptype = list(),
    .constrain = FALSE,
    .atomic = TRUE
  )

  vec_simplify(out, .ptype)
}

slide_period2_vec_direct <- function(.x,
                                     .y,
                                     .i,
                                     .period,
                                     .f,
                                     ...,
                                     .every,
                                     .origin,
                                     .before,
                                     .after,
                                     .complete,
                                     .ptype,
                                     .slider_error_call = caller_env()) {
  slide_period2_impl(
    .x,
    .y,
    .i,
    .period,
    .f,
    ...,
    .every = .every,
    .origin = .origin,
    .before = .before,
    .after = .after,
    .complete = .complete,
    .ptype = .ptype,
    .constrain = TRUE,
    .atomic = TRUE,
    .slider_error_call = .slider_error_call
  )
}

#' @rdname slide_period2
#' @export
slide_period2_dbl <- function(.x,
                              .y,
                              .i,
                              .period,
                              .f,
                              ...,
                              .every = 1L,
                              .origin = NULL,
                              .before = 0L,
                              .after = 0L,
                              .complete = FALSE) {
  slide_period2_vec_direct(
    .x,
    .y,
    .i,
    .period,
    .f,
    ...,
    .every = .every,
    .origin = .origin,
    .before = .before,
    .after = .after,
    .complete = .complete,
    .ptype = double()
  )
}

#' @rdname slide_period2
#' @export
slide_period2_int <- function(.x,
                              .y,
                              .i,
                              .period,
                              .f,
                              ...,
                              .every = 1L,
                              .origin = NULL,
                              .before = 0L,
                              .after = 0L,
                              .complete = FALSE) {
  slide_period2_vec_direct(
    .x,
    .y,
    .i,
    .period,
    .f,
    ...,
    .every = .every,
    .origin = .origin,
    .before = .before,
    .after = .after,
    .complete = .complete,
    .ptype = integer()
  )
}

#' @rdname slide_period2
#' @export
slide_period2_lgl <- function(.x,
                              .y,
                              .i,
                              .period,
                              .f,
                              ...,
                              .every = 1L,
                              .origin = NULL,
                              .before = 0L,
                              .after = 0L,
                              .complete = FALSE) {
  slide_period2_vec_direct(
    .x,
    .y,
    .i,
    .period,
    .f,
    ...,
    .every = .every,
    .origin = .origin,
    .before = .before,
    .after = .after,
    .complete = .complete,
    .ptype = logical()
  )
}

#' @rdname slide_period2
#' @export
slide_period2_chr <- function(.x,
                              .y,
                              .i,
                              .period,
                              .f,
                              ...,
                              .every = 1L,
                              .origin = NULL,
                              .before = 0L,
                              .after = 0L,
                              .complete = FALSE) {
  slide_period2_vec_direct(
    .x,
    .y,
    .i,
    .period,
    .f,
    ...,
    .every = .every,
    .origin = .origin,
    .before = .before,
    .after = .after,
    .complete = .complete,
    .ptype = character()
  )
}

#' @rdname slide_period2
#' @export
slide_period2_dfr <- function(.x,
                              .y,
                              .i,
                              .period,
                              .f,
                              ...,
                              .every = 1L,
                              .origin = NULL,
                              .before = 0L,
                              .after = 0L,
                              .complete = FALSE,
                              .names_to = rlang::zap(),
                              .name_repair = c("unique", "universal", "check_unique")) {
  out <- slide_period2(
    .x,
    .y,
    .i,
    .period,
    .f,
    ...,
    .every = .every,
    .origin = .origin,
    .before = .before,
    .after = .after,
    .complete = .complete
  )

  vec_rbind(!!!out, .names_to = .names_to, .name_repair = .name_repair)
}

#' @rdname slide_period2
#' @export
slide_period2_dfc <- function(.x,
                              .y,
                              .i,
                              .period,
                              .f,
                              ...,
                              .every = 1L,
                              .origin = NULL,
                              .before = 0L,
                              .after = 0L,
                              .complete = FALSE,
                              .size = NULL,
                              .name_repair = c("unique", "universal", "check_unique", "minimal")) {
  out <- slide_period2(
    .x,
    .y,
    .i,
    .period,
    .f,
    ...,
    .every = .every,
    .origin = .origin,
    .before = .before,
    .after = .after,
    .complete = .complete
  )

  vec_cbind(!!!out, .size = .size, .name_repair = .name_repair)
}

# ------------------------------------------------------------------------------

slide_period2_impl <- function(.x,
                               .y,
                               .i,
                               .period,
                               .f,
                               ...,
                               .every,
                               .origin,
                               .before,
                               .after,
                               .complete,
                               .ptype,
                               .constrain,
                               .atomic,
                               .slider_error_call = caller_env()) {
  vec_assert(.x, call = .slider_error_call)
  vec_assert(.y, call = .slider_error_call)

  args <- vec_recycle_common(.x = .x, .y = .y, .call = .slider_error_call)

  .f <- as_function(.f, call = .slider_error_call)

  f_call <- expr(.f(.x, .y, ...))

  type <- -2L

  slide_period_common(
    x = args,
    i = .i,
    period = .period,
    f_call = f_call,
    every = .every,
    origin = .origin,
    before = .before,
    after = .after,
    complete = .complete,
    ptype = .ptype,
    constrain = .constrain,
    atomic = .atomic,
    env = environment(),
    type = type,
    slider_error_call = .slider_error_call
  )
}

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
    .constrain = FALSE,
    .ptype = list()
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

  if (is.null(.ptype)) {
    out <- slide_period2_vec_simplify(
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

    return(out)
  }

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
    .constrain = TRUE,
    .ptype = .ptype
  )
}

slide_period2_vec_simplify <- function(.x,
                                       .y,
                                       .i,
                                       .period,
                                       .f,
                                       ...,
                                       .every,
                                       .origin,
                                       .before,
                                       .after,
                                       .complete) {
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

  check_all_size_one(out)

  vec_simplify(out)
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
  slide_period2_vec(
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
  slide_period2_vec(
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
  slide_period2_vec(
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
  slide_period2_vec(
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
                              .names_to = NULL,
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
                               .constrain,
                               .ptype) {
  vec_assert(.x)
  vec_assert(.y)

  # TODO - Do more efficiently internally by reusing rather than recycling
  # https://github.com/tidyverse/purrr/blob/e4d553989e3d18692ebeeedb334b6223ae9ea294/src/map.c#L129
  # But use `vec_size_common()` to check sizes and get `.size`
  args <- vec_recycle_common(.x, .y)

  .f <- as_function(.f)

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
    constrain = .constrain,
    ptype = .ptype,
    env = environment(),
    type = type
  )
}

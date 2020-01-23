#' Slide along multiples inputs simultaneously relative to an index
#'
#' `slide_index2()` and `pslide_index()` represent the combination
#' of [slide2()] and [pslide()] with [slide_index()], allowing you to iterate
#' over multiple vectors at once relative to an `.i`-ndex.
#'
#' @inheritParams slide_index
#'
#' @template param-x-y
#' @template param-l
#' @template param-before-after-slide-index
#'
#' @return
#' A vector fulfilling the following invariants:
#'
#' \subsection{`slide_index2()`}{
#'
#'  * `vec_size(slide_index2(.x, .y)) == vec_size_common(.x, .y)`
#'
#'  * `vec_ptype(slide_index2(.x, .y)) == list()`
#'
#' }
#'
#' \subsection{`slide_index2_vec()` and `slide_index2_*()` variants}{
#'
#'  * `vec_size(slide_index2_vec(.x, .y)) == vec_size_common(.x, .y)`
#'
#'  * `vec_size(slide_index2_vec(.x, .y)[[1]]) == 1L`
#'
#'  * `vec_ptype(slide_index2_vec(.x, .y, .ptype = ptype)) == ptype`
#'
#' }
#'
#' \subsection{`pslide_index()`}{
#'
#'  * `vec_size(pslide_index(.l)) == vec_size_common(!!! .l)`
#'
#'  * `vec_ptype(pslide_index(.l)) == list()`
#'
#' }
#'
#' \subsection{`pslide_index_vec()` and `pslide_index_*()` variants}{
#'
#'  * `vec_size(pslide_index_vec(.l)) == vec_size_common(!!! .l)`
#'
#'  * `vec_size(pslide_index_vec(.l)[[1]]) == 1L`
#'
#'  * `vec_ptype(pslide_index_vec(.l, .ptype = ptype)) == ptype`
#'
#' }
#'
#' @examples
#' # Notice that `i` is an irregular index!
#' x <- 1:5
#' y <- 6:10
#' i <- as.Date("2019-08-15") + c(0:1, 4, 6, 7)
#'
#' # When we slide over `i` looking back 1 day, the irregularity is respected.
#' # When there is a gap in dates, only 2 values are returned (one from
#' # `x` and one from `y`), otherwise, 4 values are returned.
#' slide_index2(x, y, i, ~c(.x, .y), .before = 1)
#'
#' @seealso [slide2()], [hop_index2()], [slide_index()]
#' @export
slide_index2 <- function(.x,
                         .y,
                         .i,
                         .f,
                         ...,
                         .before = 0L,
                         .after = 0L,
                         .complete = FALSE) {
  slide_index2_impl(
    .x,
    .y,
    .i,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .complete = .complete,
    .constrain = FALSE,
    .ptype = list()
  )
}

#' @rdname slide_index2
#' @export
slide_index2_vec <- function(.x,
                             .y,
                             .i,
                             .f,
                             ...,
                             .before = 0L,
                             .after = 0L,
                             .complete = FALSE,
                             .ptype = NULL) {

  if (is.null(.ptype)) {
    out <- slide_index2_vec_simplify(
      .x,
      .y,
      .i,
      .f,
      ...,
      .before = .before,
      .after = .after,
      .complete = .complete
    )

    return(out)
  }

  slide_index2_impl(
    .x,
    .y,
    .i,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .complete = .complete,
    .ptype = .ptype,
    .constrain = TRUE
  )
}

slide_index2_vec_simplify <- function(.x,
                                      .y,
                                      .i,
                                      .f,
                                      ...,
                                      .before,
                                      .after,
                                      .complete) {
  out <- slide_index2(
    .x,
    .y,
    .i,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .complete = .complete
  )

  check_all_size_one(out)

  vec_simplify(out)
}

#' @rdname slide_index2
#' @export
slide_index2_dbl <- function(.x,
                             .y,
                             .i,
                             .f,
                             ...,
                             .before = 0L,
                             .after = 0L,
                             .complete = FALSE) {
  slide_index2_vec(
    .x,
    .y,
    .i,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .complete = .complete,
    .ptype = double()
  )
}

#' @rdname slide_index2
#' @export
slide_index2_int <- function(.x,
                             .y,
                             .i,
                             .f,
                             ...,
                             .before = 0L,
                             .after = 0L,
                             .complete = FALSE) {
  slide_index2_vec(
    .x,
    .y,
    .i,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .complete = .complete,
    .ptype = integer()
  )
}

#' @rdname slide_index2
#' @export
slide_index2_lgl <- function(.x,
                             .y,
                             .i,
                             .f,
                             ...,
                             .before = 0L,
                             .after = 0L,
                             .complete = FALSE) {
  slide_index2_vec(
    .x,
    .y,
    .i,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .complete = .complete,
    .ptype = logical()
  )
}

#' @rdname slide_index2
#' @export
slide_index2_chr <- function(.x,
                             .y,
                             .i,
                             .f,
                             ...,
                             .before = 0L,
                             .after = 0L,
                             .complete = FALSE) {
  slide_index2_vec(
    .x,
    .y,
    .i,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .complete = .complete,
    .ptype = character()
  )
}

#' @inheritParams vctrs::vec_rbind
#' @rdname slide_index2
#' @export
slide_index2_dfr <- function(.x,
                             .y,
                             .i,
                             .f,
                             ...,
                             .before = 0L,
                             .after = 0L,
                             .complete = FALSE,
                             .names_to = NULL,
                             .name_repair = c("unique", "universal", "check_unique")) {
  out <- slide_index2(
    .x,
    .y,
    .i,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .complete = .complete
  )

  vec_rbind(!!!out, .names_to = .names_to, .name_repair = .name_repair)
}

#' @inheritParams vctrs::vec_cbind
#' @rdname slide_index2
#' @export
slide_index2_dfc <- function(.x,
                             .y,
                             .i,
                             .f,
                             ...,
                             .before = 0L,
                             .after = 0L,
                             .complete = FALSE,
                             .size = NULL,
                             .name_repair = c("unique", "universal", "check_unique", "minimal")) {
  out <- slide_index2(
    .x,
    .y,
    .i,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .complete = .complete
  )

  vec_cbind(!!!out, .size = .size, .name_repair = .name_repair)
}

# ------------------------------------------------------------------------------

slide_index2_impl <- function(.x,
                              .y,
                              .i,
                              .f,
                              ...,
                              .before,
                              .after,
                              .complete,
                              .constrain,
                              .ptype) {
  vec_assert(.x)
  vec_assert(.y)

  .f <- as_function(.f)

  # TODO - more efficiently? reuse .x/.y rather than recycle
  args <- vec_recycle_common(.x, .y)

  f_call <- expr(.f(.x, .y, ...))

  type <- -2L

  slide_index_common(
    x = args,
    i = .i,
    f_call = f_call,
    before = .before,
    after = .after,
    complete = .complete,
    constrain = .constrain,
    ptype = .ptype,
    env = environment(),
    type = type
  )
}

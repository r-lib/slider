#' Slide along multiples inputs simultaneously over an index
#'
#' `slide_index2()` and `pslide_index()` represent the combination
#' of [slide2()] and [pslide()] with [slide_index()], allowing you to iterate
#' over multiple vectors at once, while respecting an `.i`-ndex.
#'
#' @inheritParams slide_index
#'
#' @param .x,.y `[vector]`
#'
#'   Vectors to iterate over. Vectors of size 1 will be recycled.
#'
#' @param .l `[list]`
#'
#'   A list of vectors. The length of `.l` determines the
#'   number of arguments that `.f` will be called with. If `.l` has names,
#'   they will be used as named arguments to `.f`. Elements of `.l` with size
#'   1 will be recycled.
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
#' @inheritSection slide_index The `.i`-ndex
#' @seealso [slide2()], [slide_index()]
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
                             .ptype = list()) {

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

  vec_c(!!!out)
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

#' @rdname slide_index2
#' @export
slide_index2_raw <- function(.x,
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
    .ptype = raw()
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

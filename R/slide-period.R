#' Slide relative to an index chunked by period
#'
#' @description
#' `slide_period()` breaks up the `.i`ndex by `.period`, and then uses that
#' to define the indices to slide over `.x` with.
#'
#' It can be useful for, say, sliding over daily data in monthly chunks.
#'
#' @export
slide_period <- function(.x,
                         .i,
                         .period,
                         .f,
                         ...,
                         .every = 1L,
                         .origin = NULL,
                         .before = 0L,
                         .after = 0L,
                         .complete = FALSE) {
  slide_period_impl(
    .x,
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

#' @rdname slide_period
#' @export
slide_period_vec <- function(.x,
                             .i,
                             .period,
                             .f,
                             ...,
                             .every = 1L,
                             .origin = NULL,
                             .before = 0L,
                             .after = 0L,
                             .complete = FALSE,
                             .ptype = list()) {

  if (is.null(.ptype)) {
    out <- slide_period_simplify(
      .x,
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

  slide_period_impl(
    .x,
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

slide_period_simplify <- function(.x,
                                  .i,
                                  .period,
                                  .f,
                                  ...,
                                  .every,
                                  .origin,
                                  .before,
                                  .after,
                                  .complete) {
  out <- slide_period(
    .x,
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

  vec_c(!!!out)
}

#' @rdname slide_period
#' @export
slide_period_dbl <- function(.x,
                             .i,
                             .period,
                             .f,
                             ...,
                             .every = 1L,
                             .origin = NULL,
                             .before = 0L,
                             .after = 0L,
                             .complete = FALSE) {
  slide_period_vec(
    .x,
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

#' @rdname slide_period
#' @export
slide_period_int <- function(.x,
                             .i,
                             .period,
                             .f,
                             ...,
                             .every = 1L,
                             .origin = NULL,
                             .before = 0L,
                             .after = 0L,
                             .complete = FALSE) {
  slide_period_vec(
    .x,
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

#' @rdname slide_period
#' @export
slide_period_lgl <- function(.x,
                             .i,
                             .period,
                             .f,
                             ...,
                             .every = 1L,
                             .origin = NULL,
                             .before = 0L,
                             .after = 0L,
                             .complete = FALSE) {
  slide_period_vec(
    .x,
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

#' @rdname slide_period
#' @export
slide_period_chr <- function(.x,
                             .i,
                             .period,
                             .f,
                             ...,
                             .every = 1L,
                             .origin = NULL,
                             .before = 0L,
                             .after = 0L,
                             .complete = FALSE) {
  slide_period_vec(
    .x,
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

#' @rdname slide_period
#' @export
slide_period_raw <- function(.x,
                             .i,
                             .period,
                             .f,
                             ...,
                             .every = 1L,
                             .origin = NULL,
                             .before = 0L,
                             .after = 0L,
                             .complete = FALSE) {
  slide_period_vec(
    .x,
    .i,
    .period,
    .f,
    ...,
    .every = .every,
    .origin = .origin,
    .before = .before,
    .after = .after,
    .complete = .complete,
    .ptype = raw()
  )
}

#' @rdname slide_period
#' @export
slide_period_dfr <- function(.x,
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
  out <- slide_period(
    .x,
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

#' @rdname slide_period
#' @export
slide_period_dfc <- function(.x,
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
  out <- slide_period(
    .x,
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

slide_period_impl <- function(.x,
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

  .f <- as_function(.f)

  f_call <- expr(.f(.x, ...))

  type <- -1L

  slide_period_common(
    x = .x,
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
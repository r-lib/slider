#' Slide over multiple inputs between boundaries
#'
#' `slide_between2()` and `pslide_between()` represent the combination
#' of [slide2()] and [pslide()] with [slide_between()], allowing you to iterate
#' over multiple vectors at once, while sliding along an `.i`-ndex with
#' boundaries defined by `.starts` and `.stops`.
#'
#' @inheritParams slide_between
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
#' i <- as.Date("2019-08-15") + c(0:1, 4, 6, 7)
#'
#' # Manually create starts/stops. They don't have to be equally spaced,
#' # and they don't have to be the same size as `.x` or `.i`.
#' starts <- as.Date(c("2019-08-15", "2019-08-18"))
#' stops <- as.Date(c("2019-08-16", "2019-08-23"))
#'
#' # The output size is equal to the common size of `.starts` and `.stops`
#' slide_between2(x, i, i, starts, stops, ~data.frame(x = .x, y = .y))
#'
#' @inheritSection slide_index The `.i`-ndex
#' @export
slide_between2 <- function(.x, .y, .i, .starts, .stops, .f, ...) {
  slide_between2_impl(
    .x,
    .y,
    .i,
    .starts,
    .stops,
    .f,
    ...,
    .constrain = FALSE,
    .ptype = list()
  )
}

#' @rdname slide_between2
#' @export
slide_between2_vec <- function(.x,
                               .y,
                               .i,
                               .starts,
                               .stops,
                               .f,
                               ...,
                               .ptype = list()) {

  if (is.null(.ptype)) {
    out <- slide_between2_vec_simplify(
      .x,
      .y,
      .i,
      .starts,
      .stops,
      .f,
      ...
    )

    return(out)
  }

  slide_between2_impl(
    .x,
    .y,
    .i,
    .starts,
    .stops,
    .f,
    ...,
    .constrain = TRUE,
    .ptype = .ptype
  )
}

slide_between2_vec_simplify <- function(.x,
                                        .y,
                                        .i,
                                        .starts,
                                        .stops,
                                        .f,
                                        ...) {
  out <- slide_between2(
    .x,
    .y,
    .i,
    .starts,
    .stops,
    .f,
    ...
  )

  check_all_size_one(out)

  vec_c(!!!out)
}

#' @rdname slide_between2
#' @export
slide_between2_dbl <- function(.x,
                               .y,
                               .i,
                               .starts,
                               .stops,
                               .f,
                               ...) {
  slide_between2_vec(
    .x,
    .y,
    .i,
    .starts,
    .stops,
    .f,
    ...,
    .ptype = double()
  )
}

#' @rdname slide_between2
#' @export
slide_between2_int <- function(.x,
                               .y,
                               .i,
                               .starts,
                               .stops,
                               .f,
                               ...) {
  slide_between2_vec(
    .x,
    .y,
    .i,
    .starts,
    .stops,
    .f,
    ...,
    .ptype = integer()
  )
}

#' @rdname slide_between2
#' @export
slide_between2_lgl <- function(.x,
                               .y,
                               .i,
                               .starts,
                               .stops,
                               .f,
                               ...) {
  slide_between2_vec(
    .x,
    .y,
    .i,
    .starts,
    .stops,
    .f,
    ...,
    .ptype = logical()
  )
}

#' @rdname slide_between2
#' @export
slide_between2_chr <- function(.x,
                               .y,
                               .i,
                               .starts,
                               .stops,
                               .f,
                               ...) {
  slide_between2_vec(
    .x,
    .y,
    .i,
    .starts,
    .stops,
    .f,
    ...,
    .ptype = character()
  )
}

#' @rdname slide_between2
#' @export
slide_between2_raw <- function(.x,
                               .y,
                               .i,
                               .starts,
                               .stops,
                               .f,
                               ...) {
  slide_between2_vec(
    .x,
    .y,
    .i,
    .starts,
    .stops,
    .f,
    ...,
    .ptype = raw()
  )
}

#' @rdname slide_between2
#' @export
slide_between2_dfr <- function(.x,
                               .y,
                               .i,
                               .starts,
                               .stops,
                               .f,
                               ...,
                               .names_to = NULL,
                               .name_repair = c("unique", "universal", "check_unique")) {
  out <- slide_between2(
    .x,
    .y,
    .i,
    .starts,
    .stops,
    .f,
    ...
  )

  vec_rbind(!!!out, .names_to = .names_to, .name_repair = .name_repair)
}

#' @rdname slide_between2
#' @export
slide_between2_dfc <- function(.x,
                               .y,
                               .i,
                               .starts,
                               .stops,
                               .f,
                               ...,
                               .size = NULL,
                               .name_repair = c("unique", "universal", "check_unique", "minimal")) {
  out <- slide_between2(
    .x,
    .y,
    .i,
    .starts,
    .stops,
    .f,
    ...
  )

  vec_cbind(!!!out, .size = .size, .name_repair = .name_repair)
}

# ------------------------------------------------------------------------------

slide_between2_impl <- function(.x, .y, .i, .starts, .stops, .f, ..., .constrain, .ptype) {
  vec_assert(.x)
  vec_assert(.y)

  # TODO - Do more efficiently internally by reusing rather than recycling
  # https://github.com/tidyverse/purrr/blob/e4d553989e3d18692ebeeedb334b6223ae9ea294/src/map.c#L129
  # But use `vec_size_common()` to check sizes and get `.size`
  args <- vec_recycle_common(.x, .y)

  .f <- as_function(.f)

  f_call <- expr(.f(.x, .y, ...))

  type <- -2L

  slide_between_common(
    x = args,
    i = .i,
    starts = .starts,
    stops = .stops,
    f_call = f_call,
    constrain = .constrain,
    ptype = .ptype,
    env = environment(),
    type = type
  )
}

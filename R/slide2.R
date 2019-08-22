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
#' @examples
#' # Slide along two inputs at once
#' slide2(1:5, 6:10, ~list(.x, .y), .before = 2)
#'
#' # Or, for more than two, use `pslide()`
#' pslide(list(1:5, 6:10, 11:15), ~list(.x, .y, ..3), .before = 2)
#'
#' # You can even slide along the rows of multiple data frames of
#' # equal size at once
#' x <- data.frame(a = rnorm(10), b = rnorm(10))
#' y <- data.frame(c = letters[1:10], d = letters[11:20])
#'
#' row_return <- function(x_rows, y_rows) {
#'   if (sum(x_rows$a) < 0) {
#'     x_rows
#'   } else {
#'     y_rows
#'   }
#' }
#'
#' slide2(x, y, row_return, .before = 1, .after = 2)
#'
#' @export
slide2 <- function(.x,
                   .y,
                   .f,
                   ...,
                   .before = 0L,
                   .after = 0L,
                   .step = 1L,
                   .complete = FALSE) {
  slide2_impl(
    .x,
    .y,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .step = .step,
    .complete = .complete,
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
                       .complete = FALSE,
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
      .complete = .complete
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
    .complete = .complete,
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
                                .complete) {
  out <- slide2(
    .x,
    .y,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .step = .step,
    .complete = .complete
  )

  check_all_size_one(out)

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
                       .complete = FALSE) {
  slide2_vec(
    .x,
    .y,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .step = .step,
    .complete = .complete,
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
                       .complete = FALSE) {
  slide2_vec(
    .x,
    .y,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .step = .step,
    .complete = .complete,
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
                       .complete = FALSE) {
  slide2_vec(
    .x,
    .y,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .step = .step,
    .complete = .complete,
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
                       .complete = FALSE) {
  slide2_vec(
    .x,
    .y,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .step = .step,
    .complete = .complete,
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
                       .complete = FALSE) {
  slide2_vec(
    .x,
    .y,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .step = .step,
    .complete = .complete,
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
                       .complete = FALSE,
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
    .complete = .complete
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
                       .complete = FALSE,
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
    .complete = .complete
  )

  vec_cbind(!!!out, .size = .size, .name_repair = .name_repair)
}

# ------------------------------------------------------------------------------

slide2_impl <- function(.x,
                        .y,
                        .f,
                        ...,
                        .before,
                        .after,
                        .step,
                        .complete,
                        .ptype,
                        .constrain) {
  vec_assert(.x)
  vec_assert(.y)

  # TODO - Do more efficiently internally by reusing rather than recycling
  # https://github.com/tidyverse/purrr/blob/e4d553989e3d18692ebeeedb334b6223ae9ea294/src/map.c#L129
  # But use `vec_size_common()` to check sizes and get `.size`
  args <- vec_recycle_common(.x, .y)

  .f <- as_function(.f)

  f_call <- expr(.f(.x, .y, ...))

  type <- -2L

  params <- list(
    type,
    .constrain,
    .before,
    .after,
    .step,
    .complete
  )

  out <- slide_core(
    x = args,
    f_call = f_call,
    ptype = .ptype,
    env = environment(),
    params = params
  )

  out
}

#' Slide over multiple inputs between boundaries
#'
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

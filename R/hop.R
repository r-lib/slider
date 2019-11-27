#' @export
hop <- function(.x,
                .starts,
                .stops,
                .f,
                ...) {
  hop_impl(
    .x,
    .starts,
    .stops,
    .f,
    ...,
    .ptype = list(),
    .constrain = FALSE
  )
}

#' @rdname hop
#' @export
hop_vec <- function(.x,
                    .starts,
                    .stops,
                    .f,
                    ...,
                    .ptype = list()) {

  if (is.null(.ptype)) {
    out <- hop_vec_simplify(
      .x,
      .starts,
      .stops,
      .f,
      ...
    )

    return(out)
  }

  hop_impl(
    .x,
    .starts,
    .stops,
    .f,
    ...,
    .ptype = .ptype,
    .constrain = TRUE
  )
}

hop_vec_simplify <- function(.x,
                             .starts,
                             .stops,
                             .f,
                             ...) {
  out <- hop(
    .x,
    .starts,
    .stops,
    .f,
    ...
  )

  check_all_size_one(out)

  vec_c(!!!out)
}

# ------------------------------------------------------------------------------

hop_impl <- function(.x,
                     .starts,
                     .stops,
                     .f,
                     ...,
                     .ptype,
                     .constrain) {
  vec_assert(.x)

  .f <- as_function(.f)

  f_call <- expr(.f(.x, ...))

  type <- -1L

  hop_common(
    x = .x,
    starts = .starts,
    stops = .stops,
    f_call = f_call,
    ptype = .ptype,
    env = environment(),
    type = type,
    constrain = .constrain
  )
}

#' @include hop2.R
#' @rdname hop2
#' @export
phop <- function(.l,
                 .starts,
                 .stops,
                 .f,
                 ...) {
  phop_impl(
    .l,
    .starts,
    .stops,
    .f,
    ...,
    .constrain = FALSE,
    .ptype = list()
  )
}

#' @rdname hop2
#' @export
phop_vec <- function(.l,
                     .starts,
                     .stops,
                     .f,
                     ...,
                     .ptype = NULL) {

  if (is.null(.ptype)) {
    out <- phop_simplify(
      .l,
      .starts,
      .stops,
      .f,
      ...
    )

    return(out)
  }

  phop_impl(
    .l,
    .starts,
    .stops,
    .f,
    ...,
    .constrain = TRUE,
    .ptype = .ptype
  )
}

phop_simplify <- function(.l,
                          .starts,
                          .stops,
                          .f,
                          ...) {
  out <- phop(
    .l,
    .starts,
    .stops,
    .f,
    ...
  )

  check_all_size_one(out)

  vec_simplify(out)
}

# ------------------------------------------------------------------------------

phop_impl <- function(.l,
                      .starts,
                      .stops,
                      .f,
                      ...,
                      .constrain,
                      .ptype) {
  check_is_list(.l)

  lapply(.l, vec_assert)

  .f <- as_function(.f)

  # TODO - more efficiently? reuse elements rather than recycle
  .l <- vec_recycle_common(!!!.l)

  type <- vec_size(.l)

  slicers <- lapply(
    seq_len(type),
    function(x) {
      expr(.l[[!!x]])
    }
  )

  # Ensure names of `.l` are kept so they can be spliced
  # into `.f` as argument names
  names(slicers) <- names(.l)

  f_call <- expr(.f(!!! slicers, ...))

  hop_common(
    x = .l,
    starts = .starts,
    stops = .stops,
    f_call = f_call,
    ptype = .ptype,
    env = environment(),
    type = type,
    constrain = .constrain
  )
}

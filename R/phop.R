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
                     .ptype = list()) {

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

  vec_c(!!!out)
}

#' @rdname hop2
#' @export
phop_dbl <- function(.l,
                     .starts,
                     .stops,
                     .f,
                     ...) {
  phop_vec(
    .l,
    .starts,
    .stops,
    .f,
    ...,
    .ptype = double()
  )
}

#' @rdname hop2
#' @export
phop_int <- function(.l,
                     .starts,
                     .stops,
                     .f,
                     ...) {
  phop_vec(
    .l,
    .starts,
    .stops,
    .f,
    ...,
    .ptype = integer()
  )
}

#' @rdname hop2
#' @export
phop_lgl <- function(.l,
                     .starts,
                     .stops,
                     .f,
                     ...) {
  phop_vec(
    .l,
    .starts,
    .stops,
    .f,
    ...,
    .ptype = logical()
  )
}

#' @rdname hop2
#' @export
phop_chr <- function(.l,
                     .starts,
                     .stops,
                     .f,
                     ...) {
  phop_vec(
    .l,
    .starts,
    .stops,
    .f,
    ...,
    .ptype = character()
  )
}

#' @rdname hop2
#' @export
phop_raw <- function(.l,
                     .starts,
                     .stops,
                     .f,
                     ...) {
  phop_vec(
    .l,
    .starts,
    .stops,
    .f,
    ...,
    .ptype = raw()
  )
}

#' @rdname hop2
#' @export
phop_dfr <- function(.l,
                     .starts,
                     .stops,
                     .f,
                     ...,
                     .names_to = NULL,
                     .name_repair = c("unique", "universal", "check_unique")) {
  out <- phop(
    .l,
    .starts,
    .stops,
    .f,
    ...
  )

  vec_rbind(!!!out, .names_to = .names_to, .name_repair = .name_repair)
}

#' @rdname hop2
#' @export
phop_dfc <- function(.l,
                     .starts,
                     .stops,
                     .f,
                     ...,
                     .size = NULL,
                     .name_repair = c("unique", "universal", "check_unique", "minimal")) {
  out <- phop(
    .l,
    .starts,
    .stops,
    .f,
    ...
  )

  vec_cbind(!!!out, .size = .size, .name_repair = .name_repair)
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

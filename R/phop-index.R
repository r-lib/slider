#' @include hop-index2.R
#' @rdname hop_index2
#' @export
phop_index <- function(.l,
                       .i,
                       .starts,
                       .stops,
                       .f,
                       ...) {
  phop_index_impl(
    .l,
    .i,
    .starts,
    .stops,
    .f,
    ...,
    .constrain = FALSE,
    .ptype = list()
  )
}

#' @rdname hop_index2
#' @export
phop_index_vec <- function(.l,
                           .i,
                           .starts,
                           .stops,
                           .f,
                           ...,
                           .ptype = list()) {

  if (is.null(.ptype)) {
    out <- phop_index_simplify(
      .l,
      .i,
      .starts,
      .stops,
      .f,
      ...
    )

    return(out)
  }

  phop_index_impl(
    .l,
    .i,
    .starts,
    .stops,
    .f,
    ...,
    .constrain = TRUE,
    .ptype = .ptype
  )
}

phop_index_simplify <- function(.l,
                                .i,
                                .starts,
                                .stops,
                                .f,
                                ...) {
  out <- phop_index(
    .l,
    .i,
    .starts,
    .stops,
    .f,
    ...
  )

  check_all_size_one(out)

  vec_c(!!!out)
}

#' @rdname hop_index2
#' @export
phop_index_dbl <- function(.l,
                           .i,
                           .starts,
                           .stops,
                           .f,
                           ...) {
  phop_index_vec(
    .l,
    .i,
    .starts,
    .stops,
    .f,
    ...,
    .ptype = double()
  )
}

#' @rdname hop_index2
#' @export
phop_index_int <- function(.l,
                           .i,
                           .starts,
                           .stops,
                           .f,
                           ...) {
  phop_index_vec(
    .l,
    .i,
    .starts,
    .stops,
    .f,
    ...,
    .ptype = integer()
  )
}

#' @rdname hop_index2
#' @export
phop_index_lgl <- function(.l,
                           .i,
                           .starts,
                           .stops,
                           .f,
                           ...) {
  phop_index_vec(
    .l,
    .i,
    .starts,
    .stops,
    .f,
    ...,
    .ptype = logical()
  )
}

#' @rdname hop_index2
#' @export
phop_index_chr <- function(.l,
                           .i,
                           .starts,
                           .stops,
                           .f,
                           ...) {
  phop_index_vec(
    .l,
    .i,
    .starts,
    .stops,
    .f,
    ...,
    .ptype = character()
  )
}

#' @rdname hop_index2
#' @export
phop_index_raw <- function(.l,
                           .i,
                           .starts,
                           .stops,
                           .f,
                           ...) {
  phop_index_vec(
    .l,
    .i,
    .starts,
    .stops,
    .f,
    ...,
    .ptype = raw()
  )
}

#' @rdname hop_index2
#' @export
phop_index_dfr <- function(.l,
                           .i,
                           .starts,
                           .stops,
                           .f,
                           ...,
                           .names_to = NULL,
                           .name_repair = c("unique", "universal", "check_unique")) {
  out <- phop_index(
    .l,
    .i,
    .starts,
    .stops,
    .f,
    ...
  )

  vec_rbind(!!!out, .names_to = .names_to, .name_repair = .name_repair)
}

#' @rdname hop_index2
#' @export
phop_index_dfc <- function(.l,
                           .i,
                           .starts,
                           .stops,
                           .f,
                           ...,
                           .size = NULL,
                           .name_repair = c("unique", "universal", "check_unique", "minimal")) {
  out <- phop_index(
    .l,
    .i,
    .starts,
    .stops,
    .f,
    ...
  )

  vec_cbind(!!!out, .size = .size, .name_repair = .name_repair)
}

# ------------------------------------------------------------------------------

phop_index_impl <- function(.l,
                            .i,
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

  hop_index_common(
    x = .l,
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

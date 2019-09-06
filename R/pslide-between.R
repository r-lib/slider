#' @include slide-between2.R
#' @rdname slide_between2
#' @export
pslide_between <- function(.l,
                           .i,
                           .starts,
                           .stops,
                           .f,
                           ...) {
  pslide_between_impl(
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

#' @rdname slide_between2
#' @export
pslide_between_vec <- function(.l,
                               .i,
                               .starts,
                               .stops,
                               .f,
                               ...,
                               .ptype = list()) {

  if (is.null(.ptype)) {
    out <- pslide_between_simplify(
      .l,
      .i,
      .starts,
      .stops,
      .f,
      ...
    )

    return(out)
  }

  pslide_between_impl(
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

pslide_between_simplify <- function(.l,
                                    .i,
                                    .starts,
                                    .stops,
                                    .f,
                                    ...) {
  out <- pslide_between(
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

#' @rdname slide_between2
#' @export
pslide_between_dbl <- function(.l,
                               .i,
                               .starts,
                               .stops,
                               .f,
                               ...) {
  pslide_between_vec(
    .l,
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
pslide_between_int <- function(.l,
                               .i,
                               .starts,
                               .stops,
                               .f,
                               ...) {
  pslide_between_vec(
    .l,
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
pslide_between_lgl <- function(.l,
                               .i,
                               .starts,
                               .stops,
                               .f,
                               ...) {
  pslide_between_vec(
    .l,
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
pslide_between_chr <- function(.l,
                               .i,
                               .starts,
                               .stops,
                               .f,
                               ...) {
  pslide_between_vec(
    .l,
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
pslide_between_raw <- function(.l,
                               .i,
                               .starts,
                               .stops,
                               .f,
                               ...) {
  pslide_between_vec(
    .l,
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
pslide_between_dfr <- function(.l,
                               .i,
                               .starts,
                               .stops,
                               .f,
                               ...,
                               .names_to = NULL,
                               .name_repair = c("unique", "universal", "check_unique")) {
  out <- pslide_between(
    .l,
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
pslide_between_dfc <- function(.l,
                               .i,
                               .starts,
                               .stops,
                               .f,
                               ...,
                               .size = NULL,
                               .name_repair = c("unique", "universal", "check_unique", "minimal")) {
  out <- pslide_between(
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

pslide_between_impl <- function(.l,
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

  slide_between_common(
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

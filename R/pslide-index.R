#' @include slide-index2.R
#' @rdname slide_index2
#' @export
pslide_index <- function(.l,
                         .i,
                         .f,
                         ...,
                         .before = 0L,
                         .after = 0L,
                         .complete = FALSE) {
  pslide_index_impl(
    .l,
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
pslide_index_vec <- function(.l,
                             .i,
                             .f,
                             ...,
                             .before = 0L,
                             .after = 0L,
                             .complete = FALSE,
                             .ptype = NULL) {

  if (is.null(.ptype)) {
    out <- pslide_index_simplify(
      .l,
      .i,
      .f,
      ...,
      .before = .before,
      .after = .after,
      .complete = .complete
    )

    return(out)
  }

  pslide_index_impl(
    .l,
    .i,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .complete = .complete,
    .constrain = TRUE,
    .ptype = .ptype
  )
}

pslide_index_simplify <- function(.l,
                                  .i,
                                  .f,
                                  ...,
                                  .before,
                                  .after,
                                  .complete) {
  out <- pslide_index(
    .l,
    .i,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .complete = .complete
  )

  check_all_size_one(out)

  vec_simplify(out)
}

#' @rdname slide_index2
#' @export
pslide_index_dbl <- function(.l,
                             .i,
                             .f,
                             ...,
                             .before = 0L,
                             .after = 0L,
                             .complete = FALSE) {
  pslide_index_vec(
    .l,
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
pslide_index_int <- function(.l,
                             .i,
                             .f,
                             ...,
                             .before = 0L,
                             .after = 0L,
                             .complete = FALSE) {
  pslide_index_vec(
    .l,
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
pslide_index_lgl <- function(.l,
                             .i,
                             .f,
                             ...,
                             .before = 0L,
                             .after = 0L,
                             .complete = FALSE) {
  pslide_index_vec(
    .l,
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
pslide_index_chr <- function(.l,
                             .i,
                             .f,
                             ...,
                             .before = 0L,
                             .after = 0L,
                             .complete = FALSE) {
  pslide_index_vec(
    .l,
    .i,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .complete = .complete,
    .ptype = character()
  )
}

#' @inheritParams vctrs::vec_rbind
#' @rdname slide_index2
#' @export
pslide_index_dfr <- function(.l,
                             .i,
                             .f,
                             ...,
                             .before = 0L,
                             .after = 0L,
                             .complete = FALSE,
                             .names_to = NULL,
                             .name_repair = c("unique", "universal", "check_unique")) {
  out <- pslide_index(
    .l,
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
pslide_index_dfc <- function(.l,
                             .i,
                             .f,
                             ...,
                             .before = 0L,
                             .after = 0L,
                             .complete = FALSE,
                             .size = NULL,
                             .name_repair = c("unique", "universal", "check_unique", "minimal")) {
  out <- pslide_index(
    .l,
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

pslide_index_impl <- function(.l,
                              .i,
                              .f,
                              ...,
                              .before,
                              .after,
                              .complete,
                              .constrain,
                              .ptype) {
  check_is_list(.l)

  lapply(.l, vec_assert)

  .f <- as_function(.f)

  # TODO - more efficiently? reuse .x/.y rather than recycle
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

  slide_index_common(
    x = .l,
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

#' @include slide-period2.R
#' @rdname slide_period2
#' @export
pslide_period <- function(.l,
                          .i,
                          .period,
                          .f,
                          ...,
                          .every = 1L,
                          .origin = NULL,
                          .before = 0L,
                          .after = 0L,
                          .complete = FALSE) {
  pslide_period_impl(
    .l,
    .i,
    .period,
    .f,
    ...,
    .every = .every,
    .origin = .origin,
    .before = .before,
    .after = .after,
    .complete = .complete,
    .ptype = list(),
    .constrain = FALSE,
    .atomic = FALSE
  )
}

#' @rdname slide_period2
#' @export
pslide_period_vec <- function(.l,
                              .i,
                              .period,
                              .f,
                              ...,
                              .every = 1L,
                              .origin = NULL,
                              .before = 0L,
                              .after = 0L,
                              .complete = FALSE,
                              .ptype = NULL) {
  out <- pslide_period_impl(
    .l,
    .i,
    .period,
    .f,
    ...,
    .every = .every,
    .origin = .origin,
    .before = .before,
    .after = .after,
    .complete = .complete,
    .ptype = list(),
    .constrain = FALSE,
    .atomic = TRUE
  )

  vec_simplify(out, .ptype)
}

pslide_period_vec_direct <- function(.l,
                                     .i,
                                     .period,
                                     .f,
                                     ...,
                                     .every,
                                     .origin,
                                     .before,
                                     .after,
                                     .complete,
                                     .ptype) {
  pslide_period_impl(
    .l,
    .i,
    .period,
    .f,
    ...,
    .every = .every,
    .origin = .origin,
    .before = .before,
    .after = .after,
    .complete = .complete,
    .ptype = .ptype,
    .constrain = TRUE,
    .atomic = TRUE
  )
}

#' @rdname slide_period2
#' @export
pslide_period_dbl <- function(.l,
                              .i,
                              .period,
                              .f,
                              ...,
                              .every = 1L,
                              .origin = NULL,
                              .before = 0L,
                              .after = 0L,
                              .complete = FALSE) {
  pslide_period_vec_direct(
    .l,
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

#' @rdname slide_period2
#' @export
pslide_period_int <- function(.l,
                              .i,
                              .period,
                              .f,
                              ...,
                              .every = 1L,
                              .origin = NULL,
                              .before = 0L,
                              .after = 0L,
                              .complete = FALSE) {
  pslide_period_vec_direct(
    .l,
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

#' @rdname slide_period2
#' @export
pslide_period_lgl <- function(.l,
                              .i,
                              .period,
                              .f,
                              ...,
                              .every = 1L,
                              .origin = NULL,
                              .before = 0L,
                              .after = 0L,
                              .complete = FALSE) {
  pslide_period_vec_direct(
    .l,
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

#' @rdname slide_period2
#' @export
pslide_period_chr <- function(.l,
                              .i,
                              .period,
                              .f,
                              ...,
                              .every = 1L,
                              .origin = NULL,
                              .before = 0L,
                              .after = 0L,
                              .complete = FALSE) {
  pslide_period_vec_direct(
    .l,
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

#' @rdname slide_period2
#' @export
pslide_period_dfr <- function(.l,
                              .i,
                              .period,
                              .f,
                              ...,
                              .every = 1L,
                              .origin = NULL,
                              .before = 0L,
                              .after = 0L,
                              .complete = FALSE,
                              .names_to = rlang::zap(),
                              .name_repair = c("unique", "universal", "check_unique")) {
  out <- pslide_period(
    .l,
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

#' @rdname slide_period2
#' @export
pslide_period_dfc <- function(.l,
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
  out <- pslide_period(
    .l,
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

pslide_period_impl <- function(.l,
                               .i,
                               .period,
                               .f,
                               ...,
                               .every,
                               .origin,
                               .before,
                               .after,
                               .complete,
                               .ptype,
                               .constrain,
                               .atomic) {
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

  slide_period_common(
    x = .l,
    i = .i,
    period = .period,
    f_call = f_call,
    every = .every,
    origin = .origin,
    before = .before,
    after = .after,
    complete = .complete,
    ptype = .ptype,
    constrain = .constrain,
    atomic = .atomic,
    env = environment(),
    type = type
  )
}

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
  if (!is.list(.l)) {
    abort(paste0("`.l` must be a list, not ", vec_ptype_full(.l), "."))
  }

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

  slide_index_core(
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

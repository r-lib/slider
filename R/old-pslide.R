old_pslide <- function(.l,
                       .f,
                       ...,
                       .before = 0L,
                       .after = 0L,
                       .step = 1L,
                       .offset = NULL,
                       .complete = FALSE,
                       .dir = "forward") {
  old_pslide_impl(
    .l,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .step = .step,
    .offset = .offset,
    .complete = .complete,
    .dir = .dir,
    .ptype = list(),
    .constrain = FALSE
  )
}

old_pslide_vec <- function(.l,
                           .f,
                           ...,
                           .before = 0L,
                           .after = 0L,
                           .step = 1L,
                           .offset = NULL,
                           .complete = FALSE,
                           .dir = "forward",
                           .ptype = list()) {

  if (is.null(.ptype)) {
    out <- old_pslide_vec_simplify(
      .l,
      .f,
      ...,
      .before = .before,
      .after = .after,
      .step = .step,
      .offset = .offset,
      .complete = .complete,
      .dir = .dir
    )

    return(out)
  }

  old_pslide_impl(
    .l,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .step = .step,
    .offset = .offset,
    .complete = .complete,
    .dir = .dir,
    .ptype = .ptype
  )
}

old_pslide_vec_simplify <- function(.l,
                                    .f,
                                    ...,
                                    .before,
                                    .after,
                                    .step,
                                    .offset,
                                    .complete,
                                    .dir) {
  out <- old_pslide(
    .l,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .step = .step,
    .offset = .offset,
    .complete = .complete,
    .dir = .dir
  )

  if (vec_size_common(!!!out) != 1L) {
    glubort("The size of all results from `.f` must be 1.")
  }

  vec_c(!!!out)
}

old_pslide_impl <- function(.l,
                            .f,
                            ...,
                            .before,
                            .after,
                            .step,
                            .offset,
                            .complete,
                            .dir,
                            .ptype,
                            .constrain = TRUE) {

  # TODO - too strict, `pslide(list(min = 2:3, n = 1), runif)` fails
  #vec_assert(.l, ptype = list())

  lapply(.l, vec_assert)

  .l <- vec_recycle_common(!!!.l)

  # TODO - Check if .l has at least 1 element?
  .n <- vec_size(.l[[1]])

  .f <- as_function(.f)

  slicers <- lapply(
    seq_along(.l),
    function(.i) {
      expr(vec_slice(.l[[!!.i]], index))
    }
  )

  # Ensure names of `.l` are kept so they can be spliced
  # into `.f` as argument names
  names(slicers) <- names(.l)

  .f_call <- expr(.f(!!! slicers, ...))

  out <- old_slide_core(
    .f_call = .f_call,
    .n = .n,
    .before = .before,
    .after = .after,
    .step = .step,
    .offset = .offset,
    .complete = .complete,
    .dir = .dir,
    .ptype = .ptype,
    .constrain = .constrain,
    .env = environment()
  )

  vctrs:::vec_names(out) <- vctrs:::vec_names(.l[[1]])

  out
}


pslide_impl <- function(.l,
                        .f,
                        ...,
                        .before,
                        .after,
                        .step,
                        .offset,
                        .complete,
                        .dir,
                        .ptype) {

  vec_assert(.l, ptype = list())
  lapply(.l, vec_assert)

  .l <- vec_recycle_common(!!!.l)

  .n <- vec_size(.l[[1]])

  .f <- as_function(.f)

  slicers <- lapply(
    seq_along(.l),
    function(.i) {
      expr(vec_slice(.l[[!!.i]], index))
    }
  )

  .f_call <- expr(.f(!!! slicers, ...))

  out <- slide_core(
    .f_call = .f_call,
    .n = .n,
    .before = .before,
    .after = .after,
    .step = .step,
    .offset = .offset,
    .complete = .complete,
    .dir = .dir,
    .ptype = .ptype,
    .env = environment()
  )

  vctrs:::vec_names(out) <- vctrs:::vec_names(.l[[1]])

  out
}

pslide_impl <- function(.l,
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

  # TODO - Do more efficiently internally by reusing rather than recycling
  # https://github.com/tidyverse/purrr/blob/e4d553989e3d18692ebeeedb334b6223ae9ea294/src/map.c#L129
  # But use `vec_size_common()` to check sizes and get `.size`
  .l <- vec_recycle_common(!!!.l)

  # TODO - Check if .l has at least 1 element?
  .size <- vec_size(.l[[1]])

  .f <- as_function(.f)

  .inputs <- vec_size(.l)

  slicers <- lapply(
    seq_len(.inputs),
    function(.i) {
      expr(slice[[!!.i]])
    }
  )

  # Ensure names of `.l` are kept so they can be spliced
  # into `.f` as argument names
  names(slicers) <- names(.l)

  .f_call <- expr(.f(!!! slicers, ...))

  out <- slide_core(
    .x = .l,
    .inputs = .inputs,
    .f_call = .f_call,
    .size = .size,
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

  #vctrs:::vec_names(out) <- vctrs:::vec_names(.l[[1]])

  out
}

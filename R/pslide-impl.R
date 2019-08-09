pslide_impl <- function(.l,
                        .f,
                        ...,
                        .before,
                        .after,
                        .step,
                        .offset,
                        .complete,
                        .forward,
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

  type <- vec_size(.l)

  slicers <- lapply(
    seq_len(type),
    function(.i) {
      expr(.l[[!!.i]])
    }
  )

  # Ensure names of `.l` are kept so they can be spliced
  # into `.f` as argument names
  names(slicers) <- names(.l)

  f_call <- expr(.f(!!! slicers, ...))

  out <- slide_core(
    .x = .l,
    f_call = f_call,
    ptype = .ptype,
    env = environment(),
    type = type,
    .size = .size,
    .before = .before,
    .after = .after,
    .step = .step,
    .offset = .offset,
    .complete = .complete,
    .forward = .forward,
    .constrain = .constrain
  )

  out
}

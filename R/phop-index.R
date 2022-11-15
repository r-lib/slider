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
    .ptype = list(),
    .constrain = FALSE,
    .atomic = FALSE
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
                           .ptype = NULL) {
  out <- phop_index_impl(
    .l,
    .i,
    .starts,
    .stops,
    .f,
    ...,
    .ptype = list(),
    .constrain = FALSE,
    .atomic = TRUE
  )

  vec_simplify(out, .ptype)
}

# ------------------------------------------------------------------------------

phop_index_impl <- function(.l,
                            .i,
                            .starts,
                            .stops,
                            .f,
                            ...,
                            .ptype,
                            .constrain,
                            .atomic,
                            .slider_error_call = caller_env()) {
  .l <- slider_check_list(.l, call = .slider_error_call)
  list_check_all_vectors(.l, call = .slider_error_call)

  .f <- as_function(.f, call = .slider_error_call)

  .l <- vec_recycle_common(!!!.l, .arg = ".l", .call = .slider_error_call)

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
    ptype = .ptype,
    constrain = .constrain,
    atomic = .atomic,
    env = environment(),
    type = type,
    slider_error_call = .slider_error_call
  )
}

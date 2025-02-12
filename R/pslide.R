#' @include slide2.R
#' @rdname slide2
#' @export
pslide <- function(
  .l,
  .f,
  ...,
  .before = 0L,
  .after = 0L,
  .step = 1L,
  .complete = FALSE
) {
  pslide_impl(
    .l,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .step = .step,
    .complete = .complete,
    .ptype = list(),
    .constrain = FALSE,
    .atomic = FALSE
  )
}

#' @rdname slide2
#' @export
pslide_vec <- function(
  .l,
  .f,
  ...,
  .before = 0L,
  .after = 0L,
  .step = 1L,
  .complete = FALSE,
  .ptype = NULL
) {
  out <- pslide_impl(
    .l,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .step = .step,
    .complete = .complete,
    .ptype = list(),
    .constrain = FALSE,
    .atomic = TRUE
  )

  vec_simplify(out, .ptype)
}

pslide_vec_direct <- function(
  .l,
  .f,
  ...,
  .before,
  .after,
  .step,
  .complete,
  .ptype,
  .slider_error_call = caller_env()
) {
  pslide_impl(
    .l,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .step = .step,
    .complete = .complete,
    .ptype = .ptype,
    .constrain = TRUE,
    .atomic = TRUE,
    .slider_error_call = .slider_error_call
  )
}

#' @rdname slide2
#' @export
pslide_dbl <- function(
  .l,
  .f,
  ...,
  .before = 0L,
  .after = 0L,
  .step = 1L,
  .complete = FALSE
) {
  pslide_vec_direct(
    .l,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .step = .step,
    .complete = .complete,
    .ptype = double()
  )
}

#' @rdname slide2
#' @export
pslide_int <- function(
  .l,
  .f,
  ...,
  .before = 0L,
  .after = 0L,
  .step = 1L,
  .complete = FALSE
) {
  pslide_vec_direct(
    .l,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .step = .step,
    .complete = .complete,
    .ptype = integer()
  )
}

#' @rdname slide2
#' @export
pslide_lgl <- function(
  .l,
  .f,
  ...,
  .before = 0L,
  .after = 0L,
  .step = 1L,
  .complete = FALSE
) {
  pslide_vec_direct(
    .l,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .step = .step,
    .complete = .complete,
    .ptype = logical()
  )
}

#' @rdname slide2
#' @export
pslide_chr <- function(
  .l,
  .f,
  ...,
  .before = 0L,
  .after = 0L,
  .step = 1L,
  .complete = FALSE
) {
  pslide_vec_direct(
    .l,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .step = .step,
    .complete = .complete,
    .ptype = character()
  )
}

#' @inheritParams vctrs::vec_rbind
#' @rdname slide2
#' @export
pslide_dfr <- function(
  .l,
  .f,
  ...,
  .before = 0L,
  .after = 0L,
  .step = 1L,
  .complete = FALSE,
  .names_to = rlang::zap(),
  .name_repair = c("unique", "universal", "check_unique")
) {
  out <- pslide(
    .l,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .step = .step,
    .complete = .complete
  )

  vec_rbind(!!!out, .names_to = .names_to, .name_repair = .name_repair)
}

#' @inheritParams vctrs::vec_cbind
#' @rdname slide2
#' @export
pslide_dfc <- function(
  .l,
  .f,
  ...,
  .before = 0L,
  .after = 0L,
  .step = 1L,
  .complete = FALSE,
  .size = NULL,
  .name_repair = c("unique", "universal", "check_unique", "minimal")
) {
  out <- pslide(
    .l,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .step = .step,
    .complete = .complete
  )

  vec_cbind(!!!out, .size = .size, .name_repair = .name_repair)
}

# ------------------------------------------------------------------------------

pslide_impl <- function(
  .l,
  .f,
  ...,
  .before,
  .after,
  .step,
  .complete,
  .ptype,
  .constrain,
  .atomic,
  .slider_error_call = caller_env()
) {
  .l <- slider_check_list(.l, call = .slider_error_call)
  list_check_all_vectors(.l, call = .slider_error_call)

  .l <- vec_recycle_common(!!!.l, .arg = ".l", .call = .slider_error_call)

  .f <- as_function(.f, call = .slider_error_call)

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

  f_call <- expr(.f(!!!slicers, ...))

  params <- list(
    type,
    .constrain,
    .atomic,
    .before,
    .after,
    .step,
    .complete
  )

  slide_common(
    x = .l,
    f_call = f_call,
    ptype = .ptype,
    env = environment(),
    params = params
  )
}

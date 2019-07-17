glubort <- function(..., .sep = "", .envir = parent.frame()) {
  abort(glue::glue(..., .sep = .sep, .envir = .envir))
}


vec_simplify <- function(x, .ptype = NULL) {
  .ptype <- tryCatch(
    vec_ptype_common(!!!x, .ptype = .ptype),
    vctrs_error_incompatible_type = function(e) {
      list()
    }
  )

  if (vec_is(.ptype, ptype = list())) {
    return(x)
  }

  for (i in seq_along(x)) {
    if (vec_size(x[[i]]) != 1L) {
      glubort("Each result from calling `.f` must be length 1. Result {i} is length {vec_size(x[[i]])}.")
    }
  }

  vec_c(!!!x, .ptype = .ptype)
}

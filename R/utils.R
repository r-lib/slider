glubort <- function (..., .sep = "", .envir = parent.frame()) {
  abort(glue::glue(..., .sep = .sep, .envir = .envir))
}

collapse_and_trim <- function(x) {
  glue::glue_collapse(x, sep = ", ", width = 30L)
}

check_all_size_one <- function(out) {
  size <- vec_size_common(!!!out)

  if (size != 1L) {
    sizes <- vapply(out, vec_size, integer(1))
    iteration <- which(sizes != 1L)[[1L]]
    bad_size <- sizes[[iteration]]
    stop_not_all_size_one(iteration, bad_size)
  }

  invisible(out)
}

stop_not_all_size_one <- function(iteration, size) {
  glubort("In iteration {iteration}, the result of `.f` had size {size}, not 1.")
}

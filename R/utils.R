glubort <- function (..., .sep = "", .envir = parent.frame()) {
  abort(glue::glue(..., .sep = .sep, .envir = .envir))
}

collapse_and_trim <- function(x) {
  glue::glue_collapse(x, sep = ", ", width = 30L)
}

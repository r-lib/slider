.onLoad <- function(libname, pkgname) {
  # Load vctrs namespace for access to C callables
  requireNamespace("vctrs", quietly = TRUE)

  # Initialize slurrr C globals
  .Call(slurrr_init)
}

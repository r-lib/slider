# nocov start

.onLoad <- function(libname, pkgname) {
  # Load vctrs namespace for access to C callables
  requireNamespace("vctrs", quietly = TRUE)

  # Initialize slurrr C globals
  .Call(slurrr_init)
}

# Used when building the slide_index() expression
utils::globalVariables(c("window"))

# nocov end

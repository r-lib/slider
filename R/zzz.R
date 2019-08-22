# nocov start

.onLoad <- function(libname, pkgname) {
  # Load vctrs namespace for access to C callables
  requireNamespace("vctrs", quietly = TRUE)

  # Initialize slide C globals
  .Call(slide_init, ns_env("slide"))
}

# nocov end

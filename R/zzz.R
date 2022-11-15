# nocov start

.onLoad <- function(libname, pkgname) {
  # Load vctrs namespace for access to C callables
  requireNamespace("vctrs", quietly = TRUE)

  # Initialize slider C globals
  .Call(slider_initialize, ns_env("slider"))

  run_on_load()
}

# nocov end

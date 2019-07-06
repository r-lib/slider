.onLoad <- function(libname, pkgname) {
  requireNamespace("vctrs", quietly = TRUE)
  .Call(slurrr_init, ns_env("slurrr"))
}

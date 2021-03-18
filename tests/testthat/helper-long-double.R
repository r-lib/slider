skip_if_no_long_double <- function() {
  skip_if(
    condition = .Machine$sizeof.longdouble <= 8L,
    message = "`long double` is less than or equal to `double` on this platform."
  )
}

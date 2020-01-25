# vctrs doesn't publicly export these, but we can get
# them through the C API

vec_set_names <- function(x, names) {
  .Call(slider_vec_set_names, x, names)
}

vec_names <- function(x) {
  .Call(slider_vec_names, x)
}

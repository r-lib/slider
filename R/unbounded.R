#' @export
unbounded <- function() {
  new_box(missing_arg(), class = "slurrr_box_unbounded")
}

#' @export
print.slurrr_box_unbounded <- function(x, ...) {
  cat("<unbounded>")
  print(unbox(x))
}

is_unbounded <- function(x) {
  is_box(x, "slurrr_box_unbounded")
}

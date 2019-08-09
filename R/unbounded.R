#' Unbounded
#'
#' `unbounded()` can be used as the `.before` or `.after` argument to [slide()].
#' It is generally used to create a _cumulative_ sliding window. For example,
#' when `.before = unbounded()` is set, the start of sliding window will
#' always be pinned to the first position in `.x`, while the end of the sliding
#' window continues to shift forward along `.x`.
#'
#' @seealso [slide()]
#'
#' @examples
#'
#' slide(1:5, ~.x, .before = unbounded())
#'
#' @export
unbounded <- function() {
  new_box(missing_arg(), class = "slurrr_box_unbounded")
}

#' @export
print.slurrr_box_unbounded <- function(x, ...) {
  cat("<unbounded>")
  print(unbox(x))
}

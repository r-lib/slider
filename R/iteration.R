#' Context dependent information
#'
#' @description
#' `slider_iteration()` returns the current iteration number. It is only usable
#' from within a `slide_*()` function, and will error otherwise.
#'
#' @export
#' @examples
#' # Without `slider_iteration()`, there is no way to know what iteration
#' # you are on in complex slides
#' slide(1:3, ~list(slider_iteration(), .x), .before = 1, .after = 1)
#'
#' # It respects `.step` too
#' slide_int(6:10, ~slider_iteration(), .step = 2)
slider_iteration <- function() {
  .Call(slider_iteration_impl)
}

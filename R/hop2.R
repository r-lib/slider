#' Hop along multiple inputs simultaneously
#'
#' `hop2()` and `phop()` represent the combination
#' of [slide2()] and [pslide()] with [hop()], allowing you to iterate
#' over multiple vectors at once, hopping along them using boundaries defined
#' by `.starts` and `.stops`.
#'
#' @inheritParams hop
#'
#' @template param-x-y
#' @template param-l
#' @template param-starts-stops-hop
#'
#' @return
#' A vector fulfilling the following invariants:
#'
#' \subsection{`hop2()`}{
#'
#'  * `vec_size(hop2(.x, .y, .starts, .stops)) == vec_size_common(.starts, .stops)`
#'
#'  * `vec_ptype(hop2(.x, .y, .starts, .stops)) == list()`
#'
#' }
#'
#' \subsection{`hop2_vec()`}{
#'
#'  * `vec_size(hop2_vec(.x, .y, .starts, .stops)) == vec_size_common(.starts, .stops)`
#'
#'  * `vec_size(hop2_vec(.x, .y, .starts, .stops)[[1]]) == 1L`
#'
#'  * `vec_ptype(hop2_vec(.x, .y, .starts, .stops, .ptype = ptype)) == ptype`
#'
#' }
#'
#' \subsection{`phop()`}{
#'
#'  * `vec_size(phop(.l, .starts, .stops)) == vec_size_common(.starts, .stops)`
#'
#'  * `vec_ptype(phop(.l, .starts, .stops)) == list()`
#'
#' }
#'
#' \subsection{`phop_vec()`}{
#'
#'  * `vec_size(phop_vec(.l, .starts, .stops)) == vec_size_common(.starts, .stops)`
#'
#'  * `vec_size(phop_vec(.l, .starts, .stops)[[1]]) == 1L`
#'
#'  * `vec_ptype(phop_vec(.l, .starts, .stops, .ptype = ptype)) == ptype`
#'
#' }
#'
#' @examples
#' hop2(1:2, 3:4, .starts = 1, .stops = c(2, 1), ~c(x = .x, y = .y))
#'
#' phop(
#'  list(1, 2:4, 5:7),
#'  .starts = c(0, 1),
#'  .stops  = c(2, 4),
#'  ~c(x = ..1, y = ..2, z = ..3)
#' )
#'
#' @seealso [hop()], [hop_index()], [slide2()]
#' @export
hop2 <- function(.x, .y, .starts, .stops, .f, ...) {
  hop2_impl(
    .x,
    .y,
    .starts,
    .stops,
    .f,
    ...,
    .ptype = list(),
    .constrain = FALSE,
    .atomic = FALSE
  )
}

#' @rdname hop2
#' @export
hop2_vec <- function(.x,
                     .y,
                     .starts,
                     .stops,
                     .f,
                     ...,
                     .ptype = NULL) {
  out <- hop2_impl(
    .x,
    .y,
    .starts,
    .stops,
    .f,
    ...,
    .ptype = list(),
    .constrain = FALSE,
    .atomic = TRUE
  )

  vec_simplify(out, .ptype)
}

# ------------------------------------------------------------------------------

hop2_impl <- function(.x,
                      .y,
                      .starts,
                      .stops,
                      .f,
                      ...,
                      .ptype,
                      .constrain,
                      .atomic,
                      .slider_error_call = caller_env()) {
  vec_assert(.x, call = .slider_error_call)
  vec_assert(.y, call = .slider_error_call)

  args <- vec_recycle_common(.x = .x, .y = .y, .call = .slider_error_call)

  .f <- as_function(.f, call = .slider_error_call)

  f_call <- expr(.f(.x, .y, ...))

  type <- -2L

  hop_common(
    x = args,
    starts = .starts,
    stops = .stops,
    f_call = f_call,
    ptype = .ptype,
    env = environment(),
    type = type,
    constrain = .constrain,
    atomic = .atomic,
    slider_error_call = .slider_error_call
  )
}

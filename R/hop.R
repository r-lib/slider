#' Hop
#'
#' @description
#' `hop()` is the lower level engine that powers [slide()] (at least in theory).
#' It has slightly different invariants than `slide()`, and is useful
#' when you either need to hand craft boundary locations, or want to compute a
#' result with a size that is different from `.x`.
#'
#' @details
#' `hop()` is very close to being a faster version of:
#'
#' ```
#' map2(
#'   .starts,
#'   .stops,
#'   function(start, stop) {
#'     x_slice <- vec_slice(.x, start:stop)
#'     .f(x_slice, ...)
#'   }
#' )
#' ```
#'
#' Because of this, [hop_index()] is often the more useful function. `hop()`
#' mainly exists for API completeness.
#'
#' The main difference is that the start and stop values make up ranges of
#' _possible_ locations along `.x`, and it is not enforced that these locations
#' actually exist along `.x`. As an example, with `hop()` you can do the
#' following, which would be an error with `vec_slice()` because `0L` is
#' out of bounds.
#'
#' ```
#' hop(c("a", "b"), .starts = 0L, .stops = 1L, ~.x)
#' #> [[1]]
#' #> [1] "a"
#' ```
#'
#' `hop()` allows these out of bounds values to be fully compatible with
#' `slide()`. It is always possible to construct a `hop()` call from a `slide()`
#' call. For example, the following are equivalent:
#'
#' ```
#' slide(1:2, ~.x, .before = 1)
#'
#' hop(1:2, .starts = c(0, 1), .stops = c(1, 2), ~.x)
#'
#' #> [[1]]
#' #> [1] 1
#' #>
#' #> [[2]]
#' #> [1] 1 2
#' ```
#'
#' @inheritParams slide
#'
#' @template param-starts-stops-hop
#'
#' @return
#' A vector fulfilling the following invariants:
#'
#' \subsection{`hop()`}{
#'
#'  * `vec_size(hop(.x, .starts, .stops)) == vec_size_common(.starts, .stops)`
#'
#'  * `vec_ptype(hop(.x, .starts, .stops)) == list()`
#'
#' }
#'
#' \subsection{`hop_vec()`}{
#'
#'  * `vec_size(hop_vec(.x, .starts, .stops)) == vec_size_common(.starts, .stops)`
#'
#'  * `vec_size(hop_vec(.x, .starts, .stops)[[1]]) == 1L`
#'
#'  * `vec_ptype(hop_vec(.x, .starts, .stops, .ptype = ptype)) == ptype`
#'
#' }
#'
#' @examples
#' # `hop()` let's you manually specify locations to apply `.f` at.
#' hop(1:3, .starts = c(1, 3), .stops = 3, ~.x)
#'
#' # `hop()`'s start/stop locations are allowed to be out of bounds relative
#' # to the size of `.x`.
#' hop(
#'   mtcars,
#'   .starts = c(-1, 3),
#'   .stops  = c(2, 6),
#'   ~.x
#' )
#'
#' @seealso [hop2()], [hop_index()], [slide()]
#' @export
hop <- function(.x,
                .starts,
                .stops,
                .f,
                ...) {
  hop_impl(
    .x,
    .starts,
    .stops,
    .f,
    ...,
    .ptype = list(),
    .constrain = FALSE,
    .atomic = FALSE
  )
}

#' @rdname hop
#' @export
hop_vec <- function(.x,
                    .starts,
                    .stops,
                    .f,
                    ...,
                    .ptype = NULL) {
  out <- hop_impl(
    .x,
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

hop_impl <- function(.x,
                     .starts,
                     .stops,
                     .f,
                     ...,
                     .ptype,
                     .constrain,
                     .atomic,
                     .slider_error_call = caller_env()) {
  vec_assert(.x, call = .slider_error_call)

  .f <- as_function(.f, call = .slider_error_call)

  f_call <- expr(.f(.x, ...))

  type <- -1L

  hop_common(
    x = .x,
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

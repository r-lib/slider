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
#' \subsection{`hop_vec()` and `hop_*()` variants}{
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
    .constrain = FALSE
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

  if (is.null(.ptype)) {
    out <- hop_vec_simplify(
      .x,
      .starts,
      .stops,
      .f,
      ...
    )

    return(out)
  }

  hop_impl(
    .x,
    .starts,
    .stops,
    .f,
    ...,
    .ptype = .ptype,
    .constrain = TRUE
  )
}

hop_vec_simplify <- function(.x,
                             .starts,
                             .stops,
                             .f,
                             ...) {
  out <- hop(
    .x,
    .starts,
    .stops,
    .f,
    ...
  )

  check_all_size_one(out)

  vec_simplify(out)
}

#' @rdname hop
#' @export
hop_dbl <- function(.x,
                    .starts,
                    .stops,
                    .f,
                    ...) {
  hop_vec(
    .x,
    .starts,
    .stops,
    .f,
    ...,
    .ptype = double()
  )
}

#' @rdname hop
#' @export
hop_int <- function(.x,
                    .starts,
                    .stops,
                    .f,
                    ...) {
  hop_vec(
    .x,
    .starts,
    .stops,
    .f,
    ...,
    .ptype = integer()
  )
}

#' @rdname hop
#' @export
hop_lgl <- function(.x,
                    .starts,
                    .stops,
                    .f,
                    ...) {
  hop_vec(
    .x,
    .starts,
    .stops,
    .f,
    ...,
    .ptype = logical()
  )
}

#' @rdname hop
#' @export
hop_chr <- function(.x,
                    .starts,
                    .stops,
                    .f,
                    ...) {
  hop_vec(
    .x,
    .starts,
    .stops,
    .f,
    ...,
    .ptype = character()
  )
}

#' @rdname hop
#' @export
hop_dfr <- function(.x,
                    .starts,
                    .stops,
                    .f,
                    ...,
                    .names_to = NULL,
                    .name_repair = c("unique", "universal", "check_unique")) {
  out <- hop(
    .x,
    .starts,
    .stops,
    .f,
    ...
  )

  vec_rbind(!!!out, .names_to = .names_to, .name_repair = .name_repair)
}

#' @rdname hop
#' @export
hop_dfc <- function(.x,
                    .starts,
                    .stops,
                    .f,
                    ...,
                    .size = NULL,
                    .name_repair = c("unique", "universal", "check_unique", "minimal")) {
  out <- hop(
    .x,
    .starts,
    .stops,
    .f,
    ...
  )

  vec_cbind(!!!out, .size = .size, .name_repair = .name_repair)
}

# ------------------------------------------------------------------------------

hop_impl <- function(.x,
                     .starts,
                     .stops,
                     .f,
                     ...,
                     .ptype,
                     .constrain) {
  vec_assert(.x)

  .f <- as_function(.f)

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
    constrain = .constrain
  )
}

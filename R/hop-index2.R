#' Hop along multiple inputs simultaneously relative to an index
#'
#' `hop_index2()` and `phop_index()` represent the combination
#' of [slide2()] and [pslide()] with [hop_index()], allowing you to iterate
#' over multiple vectors at once, relative to an `.i`-ndex with
#' boundaries defined by `.starts` and `.stops`.
#'
#' @inheritParams hop_index
#'
#' @template param-x-y
#' @template param-l
#' @template param-starts-stops-hop-index
#'
#' @return
#' A vector fulfilling the following invariants:
#'
#' \subsection{`hop_index2()`}{
#'
#'  * `vec_size(hop_index2(.x, .y, .starts, .stops)) == vec_size_common(.starts, .stops)`
#'
#'  * `vec_ptype(hop_index2(.x, .y, .starts, .stops)) == list()`
#'
#' }
#'
#' \subsection{`hop_index2_vec()`}{
#'
#'  * `vec_size(hop_index2_vec(.x, .y, .starts, .stops)) == vec_size_common(.starts, .stops)`
#'
#'  * `vec_size(hop_index2_vec(.x, .y, .starts, .stops)[[1]]) == 1L`
#'
#'  * `vec_ptype(hop_index2_vec(.x, .y, .starts, .stops, .ptype = ptype)) == ptype`
#'
#' }
#'
#' \subsection{`phop_index()`}{
#'
#'  * `vec_size(phop_index(.l, .starts, .stops)) == vec_size_common(.starts, .stops)`
#'
#'  * `vec_ptype(phop_index(.l, .starts, .stops)) == list()`
#'
#' }
#'
#' \subsection{`phop_index_vec()`}{
#'
#'  * `vec_size(phop_index_vec(.l, .starts, .stops)) == vec_size_common(.starts, .stops)`
#'
#'  * `vec_size(phop_index_vec(.l, .starts, .stops)[[1]]) == 1L`
#'
#'  * `vec_ptype(phop_index_vec(.l, .starts, .stops, .ptype = ptype)) == ptype`
#'
#' }
#'
#' @examples
#' # Notice that `i` is an irregular index!
#' x <- 1:5
#' i <- as.Date("2019-08-15") + c(0:1, 4, 6, 7)
#'
#' # Manually create starts/stops. They don't have to be equally spaced,
#' # and they don't have to be the same size as `.x` or `.i`.
#' starts <- as.Date(c("2019-08-15", "2019-08-18"))
#' stops <- as.Date(c("2019-08-16", "2019-08-23"))
#'
#' # The output size is equal to the common size of `.starts` and `.stops`
#' hop_index2(x, i, i, starts, stops, ~data.frame(x = .x, y = .y))
#'
#' @seealso [slide2()], [slide_index2()], [hop_index()]
#' @export
hop_index2 <- function(.x, .y, .i, .starts, .stops, .f, ...) {
  hop_index2_impl(
    .x,
    .y,
    .i,
    .starts,
    .stops,
    .f,
    ...,
    .constrain = FALSE,
    .ptype = list()
  )
}

#' @rdname hop_index2
#' @export
hop_index2_vec <- function(.x,
                           .y,
                           .i,
                           .starts,
                           .stops,
                           .f,
                           ...,
                           .ptype = NULL) {

  if (is.null(.ptype)) {
    out <- hop_index2_vec_simplify(
      .x,
      .y,
      .i,
      .starts,
      .stops,
      .f,
      ...
    )

    return(out)
  }

  hop_index2_impl(
    .x,
    .y,
    .i,
    .starts,
    .stops,
    .f,
    ...,
    .constrain = TRUE,
    .ptype = .ptype
  )
}

hop_index2_vec_simplify <- function(.x,
                                    .y,
                                    .i,
                                    .starts,
                                    .stops,
                                    .f,
                                    ...) {
  out <- hop_index2(
    .x,
    .y,
    .i,
    .starts,
    .stops,
    .f,
    ...
  )

  check_all_size_one(out)

  vec_simplify(out)
}

# ------------------------------------------------------------------------------

hop_index2_impl <- function(.x, .y, .i, .starts, .stops, .f, ..., .constrain, .ptype) {
  vec_assert(.x)
  vec_assert(.y)

  # TODO - Do more efficiently internally by reusing rather than recycling
  # https://github.com/tidyverse/purrr/blob/e4d553989e3d18692ebeeedb334b6223ae9ea294/src/map.c#L129
  # But use `vec_size_common()` to check sizes and get `.size`
  args <- vec_recycle_common(.x, .y)

  .f <- as_function(.f)

  f_call <- expr(.f(.x, .y, ...))

  type <- -2L

  hop_index_common(
    x = args,
    i = .i,
    starts = .starts,
    stops = .stops,
    f_call = f_call,
    constrain = .constrain,
    ptype = .ptype,
    env = environment(),
    type = type
  )
}

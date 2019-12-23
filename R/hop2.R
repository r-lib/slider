#' Hop along multiple inputs simultaneously
#'
#' `hop2()` and `phop()` represent the combination
#' of [slide2()] and [pslide()] with [hop()], allowing you to iterate
#' over multiple vectors at once, hopping along them using boundaries defined
#' by `.starts` and `.stops`.
#'
#' @inheritParams hop
#'
#' @param .x,.y `[vector]`
#'
#'   Vectors to iterate over. Vectors of size 1 will be recycled.
#'
#' @param .l `[list]`
#'
#'   A list of vectors. The length of `.l` determines the
#'   number of arguments that `.f` will be called with. If `.l` has names,
#'   they will be used as named arguments to `.f`. Elements of `.l` with size
#'   1 will be recycled.
#'
#' @section Invariants:
#'
#' \subsection{`hop2()`}{
#'
#'  * `vec_size(hop2(.x, .y, .starts, .stops)) == vec_size_common(.starts, .stops)`
#'
#'  * `vec_ptype(hop2(.x, .y, .starts, .stops)) == list()`
#'
#' }
#'
#' \subsection{`hop2_vec()` and `hop2_*()` variants}{
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
#' \subsection{`phop_vec()` and `phop_*()` variants}{
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
#'
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
    .constrain = FALSE,
    .ptype = list()
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
                     .ptype = list()) {

  if (is.null(.ptype)) {
    out <- hop2_vec_simplify(
      .x,
      .y,
      .starts,
      .stops,
      .f,
      ...
    )

    return(out)
  }

  hop2_impl(
    .x,
    .y,
    .starts,
    .stops,
    .f,
    ...,
    .constrain = TRUE,
    .ptype = .ptype
  )
}

hop2_vec_simplify <- function(.x,
                              .y,
                              .starts,
                              .stops,
                              .f,
                              ...) {
  out <- hop2(
    .x,
    .y,
    .starts,
    .stops,
    .f,
    ...
  )

  check_all_size_one(out)

  vec_c(!!!out)
}

#' @rdname hop2
#' @export
hop2_dbl <- function(.x,
                     .y,
                     .starts,
                     .stops,
                     .f,
                     ...) {
  hop2_vec(
    .x,
    .y,
    .starts,
    .stops,
    .f,
    ...,
    .ptype = double()
  )
}

#' @rdname hop2
#' @export
hop2_int <- function(.x,
                     .y,
                     .starts,
                     .stops,
                     .f,
                     ...) {
  hop2_vec(
    .x,
    .y,
    .starts,
    .stops,
    .f,
    ...,
    .ptype = integer()
  )
}

#' @rdname hop2
#' @export
hop2_lgl <- function(.x,
                     .y,
                     .starts,
                     .stops,
                     .f,
                     ...) {
  hop2_vec(
    .x,
    .y,
    .starts,
    .stops,
    .f,
    ...,
    .ptype = logical()
  )
}

#' @rdname hop2
#' @export
hop2_chr <- function(.x,
                     .y,
                     .starts,
                     .stops,
                     .f,
                     ...) {
  hop2_vec(
    .x,
    .y,
    .starts,
    .stops,
    .f,
    ...,
    .ptype = character()
  )
}

#' @rdname hop2
#' @export
hop2_raw <- function(.x,
                     .y,
                     .starts,
                     .stops,
                     .f,
                     ...) {
  hop2_vec(
    .x,
    .y,
    .starts,
    .stops,
    .f,
    ...,
    .ptype = raw()
  )
}

#' @rdname hop2
#' @export
hop2_dfr <- function(.x,
                     .y,
                     .starts,
                     .stops,
                     .f,
                     ...,
                     .names_to = NULL,
                     .name_repair = c("unique", "universal", "check_unique")) {
  out <- hop2(
    .x,
    .y,
    .starts,
    .stops,
    .f,
    ...
  )

  vec_rbind(!!!out, .names_to = .names_to, .name_repair = .name_repair)
}

#' @rdname hop2
#' @export
hop2_dfc <- function(.x,
                     .y,
                     .starts,
                     .stops,
                     .f,
                     ...,
                     .size = NULL,
                     .name_repair = c("unique", "universal", "check_unique", "minimal")) {
  out <- hop2(
    .x,
    .y,
    .starts,
    .stops,
    .f,
    ...
  )

  vec_cbind(!!!out, .size = .size, .name_repair = .name_repair)
}

# ------------------------------------------------------------------------------

hop2_impl <- function(.x, .y, .starts, .stops, .f, ..., .constrain, .ptype) {
  vec_assert(.x)
  vec_assert(.y)

  # TODO - Do more efficiently internally by reusing rather than recycling
  # https://github.com/tidyverse/purrr/blob/e4d553989e3d18692ebeeedb334b6223ae9ea294/src/map.c#L129
  # But use `vec_size_common()` to check sizes and get `.size`
  args <- vec_recycle_common(.x, .y)

  .f <- as_function(.f)

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
    constrain = .constrain
  )
}

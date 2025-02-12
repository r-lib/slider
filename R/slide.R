#' Slide
#'
#' `slide()` iterates through `.x` using a sliding window, applying `.f` to each
#' sub-window of `.x`.
#'
#' @param .x `[vector]`
#'
#'   The vector to iterate over and apply `.f` to.
#'
#' @param .f `[function / formula]`
#'
#'   If a __function__, it is used as is.
#'
#'   If a __formula__, e.g. `~ .x + 2`, it is converted to a function. There
#'   are three ways to refer to the arguments:
#'
#'   * For a single argument function, use `.`
#'   * For a two argument function, use `.x` and `.y`
#'   * For more arguments, use `..1`, `..2`, `..3` etc
#'
#'   This syntax allows you to create very compact anonymous functions.
#'
#' @param ... Additional arguments passed on to the mapped function.
#'
#' @param .step `[positive integer(1)]`
#'
#'   The number of elements to shift the window forward between function calls.
#'
#' @param .complete `[logical(1)]`
#'
#'   Should the function be evaluated on complete windows only? If `FALSE`,
#'   the default, then partial computations will be allowed.
#'
#' @param .ptype `[vector(0) / NULL]`
#'
#'   A prototype corresponding to the type of the output.
#'
#'   If `NULL`, the default, the output type is determined by computing the
#'   common type across the results of the calls to `.f`.
#'
#'   If supplied, the result of each call to `.f` will be cast to that type,
#'   and the final output will have that type.
#'
#'   If `getOption("vctrs.no_guessing")` is `TRUE`, the `.ptype` must be
#'   supplied. This is a way to make production code demand fixed types.
#'
#' @template param-before-after-slide
#'
#' @details
#'
#' Unlike `lapply()` or `purrr::map()`, which construct calls like
#'
#' ```
#' .f(.x[[i]], ...)
#' ```
#'
#' the equivalent with `slide()` looks like
#'
#' ```
#' .f(vctrs::vec_slice(.x, i), ...)
#' ```
#'
#' which is approximately
#'
#' ```
#' .f(.x[i], ...)
#' ```
#'
#' except in the case of data frames or arrays, which are iterated
#' over row-wise.
#'
#' If `.x` has names, then the output will preserve those names.
#'
#' Using [vctrs::vec_cast()], the output of `.f` will be automatically cast
#' to the type required by the variant of `slide_*()` being used.
#'
#' @return
#' A vector fulfilling the following invariants:
#'
#' \subsection{`slide()`}{
#'
#'  * `vec_size(slide(.x)) == vec_size(.x)`
#'
#'  * `vec_ptype(slide(.x)) == list()`
#'
#' }
#'
#' \subsection{`slide_vec()` and `slide_*()` variants}{
#'
#'  * `vec_size(slide_vec(.x)) == vec_size(.x)`
#'
#'  * `vec_size(slide_vec(.x)[[1]]) == 1L`
#'
#'  * `vec_ptype(slide_vec(.x, .ptype = ptype)) == ptype`
#'
#' }
#'
#' @examples
#' # The defaults work similarly to `map()`
#' slide(1:5, ~.x)
#'
#' # Use `.before`, `.after`, and `.step` to control the window
#' slide(1:5, ~.x, .before = 1)
#'
#' # This can be used for rolling means
#' slide_dbl(rnorm(5), mean, .before = 2)
#'
#' # Or more flexible rolling operations
#' slide(rnorm(5), ~ .x - mean(.x), .before = 2)
#'
#' # `.after` allows you to "align to the left" rather than the right
#' slide(1:5, ~.x, .after = 2)
#'
#' # And a mixture of `.before` and `.after`
#' # allows you complete control over the exact alignment.
#' # Below, "center alignment" is used.
#' slide(1:5, ~.x, .before = 1, .after = 1)
#'
#' # The `.step` controls how the window is shifted along `.x`,
#' # allowing you to "skip" iterations if you only need a less granular result
#' slide(1:10, ~.x, .before = 2, .step = 3)
#'
#' # `.complete` controls whether or not partial results are computed.
#' # By default, they are, but setting `.complete = TRUE` restricts
#' # `slide()` to only evaluate the function where a complete window exists.
#' slide(1:5, ~.x, .before = 2, .after = 1)
#' slide(1:5, ~.x, .before = 2, .after = 1, .complete = TRUE)
#'
#' # ---------------------------------------------------------------------------
#' # Data frames
#'
#' # Data frames are iterated over rowwise
#' mtcars_rowwise <- slide(mtcars, ~.x)
#' mtcars_rowwise[1:3]
#'
#' # This means that any column name is easily accessible
#' slide_dbl(mtcars, ~.x$mpg + .x$cyl)
#'
#' # More advanced rowwise iteration is available as well by using the
#' # other arguments
#' mtcars_rowwise_window <- slide(mtcars, ~.x, .before = 1, .after = 1)
#' mtcars_rowwise_window[1:3]
#'
#' # ---------------------------------------------------------------------------
#' # Cumulative sliding
#'
#' # Using the special cased value, `Inf`, you can ask `slide()` to pin the
#' # start of the sliding window to the first element, effectively creating
#' # a cumulative window
#' slide(1:5, ~.x, .before = Inf)
#'
#' # Same with `.after`, this creates a window where you start with all of the
#' # elements, but decrease the total number over each iteration
#' slide(1:5, ~.x, .after = Inf)
#'
#' # ---------------------------------------------------------------------------
#' # Negative `.before` / `.after`
#'
#' # `.before` is allowed to be negative, allowing you to "look forward" in
#' # your vector. Note that `abs(.before) <= .after` must hold if `.before` is
#' # negative. In this example, we look forward to elements in locations 2 and 3
#' # but place the result in position 1 in the output.
#' slide(1:5, ~.x, .before = -1, .after = 2)
#'
#' # `.after` can be negative as well to "look backwards"
#' slide(1:5, ~.x, .before = 2, .after = -1)
#'
#' # ---------------------------------------------------------------------------
#' # Removing padding
#'
#' # If you are looking for a way to remove the `NA` values from something like
#' # this, then it doesn't exist as a built in option.
#' x <- rnorm(10)
#' slide_dbl(x, mean, .before = 3, .step = 2, .complete = TRUE)
#'
#' # Adding an option to `slide_dbl()` to remove the `NA` values would destroy
#' # its size stability. Instead, you can use a combination of `slide_dfr()`
#' # to get the start/stop indices with `hop_index_vec()`.
#' i <- seq_along(x)
#' idx <- slide_dfr(
#'   i,
#'   ~data.frame(start = .x[1], stop = .x[length(.x)]),
#'   .before = 3,
#'   .step = 2,
#'   .complete = TRUE
#' )
#'
#' idx
#'
#' hop_index_vec(x, i, idx$start, idx$stop, mean, .ptype = double())
#'
#' @seealso [slide2()], [slide_index()], [hop()]
#' @export
slide <- function(
  .x,
  .f,
  ...,
  .before = 0L,
  .after = 0L,
  .step = 1L,
  .complete = FALSE
) {
  slide_impl(
    .x,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .step = .step,
    .complete = .complete,
    .ptype = list(),
    .constrain = FALSE,
    .atomic = FALSE
  )
}

#' @rdname slide
#' @export
slide_vec <- function(
  .x,
  .f,
  ...,
  .before = 0L,
  .after = 0L,
  .step = 1L,
  .complete = FALSE,
  .ptype = NULL
) {
  out <- slide_impl(
    .x,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .step = .step,
    .complete = .complete,
    .ptype = list(),
    .constrain = FALSE,
    .atomic = TRUE
  )

  vec_simplify(out, .ptype)
}

slide_vec_direct <- function(
  .x,
  .f,
  ...,
  .before,
  .after,
  .step,
  .complete,
  .ptype,
  .slider_error_call = caller_env()
) {
  slide_impl(
    .x,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .step = .step,
    .complete = .complete,
    .ptype = .ptype,
    .constrain = TRUE,
    .atomic = TRUE,
    .slider_error_call = .slider_error_call
  )
}

#' @rdname slide
#' @export
slide_dbl <- function(
  .x,
  .f,
  ...,
  .before = 0L,
  .after = 0L,
  .step = 1L,
  .complete = FALSE
) {
  slide_vec_direct(
    .x,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .step = .step,
    .complete = .complete,
    .ptype = double()
  )
}

#' @rdname slide
#' @export
slide_int <- function(
  .x,
  .f,
  ...,
  .before = 0L,
  .after = 0L,
  .step = 1L,
  .complete = FALSE
) {
  slide_vec_direct(
    .x,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .step = .step,
    .complete = .complete,
    .ptype = integer()
  )
}

#' @rdname slide
#' @export
slide_lgl <- function(
  .x,
  .f,
  ...,
  .before = 0L,
  .after = 0L,
  .step = 1L,
  .complete = FALSE
) {
  slide_vec_direct(
    .x,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .step = .step,
    .complete = .complete,
    .ptype = logical()
  )
}

#' @rdname slide
#' @export
slide_chr <- function(
  .x,
  .f,
  ...,
  .before = 0L,
  .after = 0L,
  .step = 1L,
  .complete = FALSE
) {
  slide_vec_direct(
    .x,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .step = .step,
    .complete = .complete,
    .ptype = character()
  )
}

#' @inheritParams vctrs::vec_rbind
#' @rdname slide
#' @export
slide_dfr <- function(
  .x,
  .f,
  ...,
  .before = 0L,
  .after = 0L,
  .step = 1L,
  .complete = FALSE,
  .names_to = rlang::zap(),
  .name_repair = c("unique", "universal", "check_unique")
) {
  out <- slide(
    .x,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .step = .step,
    .complete = .complete
  )

  vec_rbind(!!!out, .names_to = .names_to, .name_repair = .name_repair)
}

#' @inheritParams vctrs::vec_cbind
#' @rdname slide
#' @export
slide_dfc <- function(
  .x,
  .f,
  ...,
  .before = 0L,
  .after = 0L,
  .step = 1L,
  .complete = FALSE,
  .size = NULL,
  .name_repair = c("unique", "universal", "check_unique", "minimal")
) {
  out <- slide(
    .x,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .step = .step,
    .complete = .complete
  )

  vec_cbind(!!!out, .size = .size, .name_repair = .name_repair)
}

# ------------------------------------------------------------------------------

slide_impl <- function(
  .x,
  .f,
  ...,
  .before,
  .after,
  .step,
  .complete,
  .ptype,
  .constrain,
  .atomic,
  .slider_error_call = caller_env()
) {
  vec_assert(.x, call = .slider_error_call)

  .f <- as_function(.f, call = .slider_error_call)

  f_call <- expr(.f(.x, ...))

  type <- -1L

  params <- list(
    type = type,
    constrain = .constrain,
    atomic = .atomic,
    before = .before,
    after = .after,
    step = .step,
    complete = .complete
  )

  slide_common(
    x = .x,
    f_call = f_call,
    ptype = .ptype,
    env = environment(),
    params = params
  )
}

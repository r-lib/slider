#' Slide
#'
#' `slide()` iterates through `.x` using a sliding window, applying `.f` to each
#' sub-window of `.x`. In slurrr, `slide()` is the most generic of the iterating
#' functions, and `tile()` and `stretch()` are just special cases of `slide()`.
#'
#' @param .x `[vector]` The vector to iterate over.
#'
#' @param .f `[function / formula]`
#'
#'   If a __function__, it is used as is.
#'
#'   If a __formula__, e.g. `~ .x + 2`, it is converted to a function with up
#'   to two arguments: `.x` (single argument) or `.x` and `.y` (two arguments).
#'   The `.` placeholder can be used instead of `.x`. This allows you to
#'   create very compact anonymous functions with up to two inputs.
#'
#' @param ... Additional arguments passed on to the mapped function.
#'
#' @param .before `[integer]` The number of values _before_ the
#'   current element to include in the sliding window. Set to `unbounded()`
#'   to select all elements before the current position. A negative value
#'   is allowed, and allows you to "look forward" as well.
#'
#' @param .after `[integer]` The number of values _after_ the
#'   current element to include in the sliding window. Set to `unbounded()`
#'   to select all elements after the current position. A negative value
#'   is allowed, and allows you to "look backward" as well.
#'
#' @param .step `[positive integer]` The number of elements to shift the
#'   window forward (or backward, depending on `.dir`) between function calls.
#'
#' @param .offset `[NULL / positive integer]` An offset from the beginning
#'   (or end, depending on `.dir`) of `.x` to place the first element in
#'   the output vector. If `NULL`, this is computed automatically as the first
#'   location where a complete sliding window can be generated.
#'
#' @param .partial `[logical]` Should partial results be computed?
#'   If sliding `"forward"`, this may result in partial results at the
#'   _end_ of the vector. If sliding `"backwards"`, this may result in partial
#'   results at the _start_ of the vector.
#'
#' @param .dir `["forward", "backward"]` The direction to slide.
#'
#' @param .ptype `[vector]` The prototype corresponding to the type of the
#'   output. Defaults to a `list()`.
#'
#' @details
#'
#' Unlike `lapply()` / `purrr::map()`, which construct calls
#' like `.f(.x[[i]], ...)`, the equivalent with `slide()`
#' looks like `.f(vec_slice(.x, i), ...)` which is approximately
#' `.f(.x[i], ...)` except in the case of data frames or arrays,
#' which are iterated over row-wise.
#'
#' If `.x` has names, then the output will preserve those names.
#'
#' Using [vctrs::vec_cast()], the output of `.f` will be automatically cast
#' to the type required by the version of `slide_*()` being used.
#'
#' @section Invariants:
#'
#'  * `vec_size(.x) == vec_size(slide(.x, .f))`
#'
#' @examples
#'
#' # The defaults work similarly to `map()`
#' slide(1:10, ~.x)
#'
#' # Use `.before`, `.after`, and `.step` to control the window
#' slide(1:10, ~.x, .before = 1)
#'
#' # This can be used for rolling means
#' slide_dbl(rnorm(10), mean, .before = 2)
#'
#' # Or more flexible rolling operations
#' slide(rnorm(10), ~ .x - mean(.x), .before = 2)
#'
#' # `.after` allows you to "align to the left" rather than the right
#' slide(1:10, ~.x, .after = 2)
#'
#' # And a mixture of `.before` and `.after`
#' # allows you complete control over the exact alignment.
#' # Below, "center alignment" is used.
#' slide(1:10, ~.x, .before = 1, .after = 1)
#'
#' # The `.step` controls how the window is shifted along `.x`,
#' # allowing you to "skip" iterations if you only need a less granular result
#' slide(1:10, ~.x, .before = 2, .step = 3)
#'
#' # The `.offset` let's you shift the initial placement of results
#' # in the output. In this example, the offset is set to place the first
#' # element at position 3, even though a result could have been computed
#' # and inserted at position 2.
#' slide(1:5, ~.x, .before = 1, .offset = 2)
#' slide(1:5, ~.x, .before = 1)
#'
#' # `.partial` controls the behavior at the end of the iterations. Here,
#' # where normally a `NULL` would be placed at the end because there are not
#' # 2 values available to construct a complete window, a partial result
#' # computed with only 1 value is instead allowed.
#' slide(1:5, ~.x, .after = 1, .partial = TRUE)
#'
#' # `.dir` controls the actual direction of sliding, and controls the
#' # order in which the sub-window of `.x` is actually sliced out (notice
#' # the elements in the backwards example are `c(5, 4)` not `c(4, 5)`).
#' slide(1:5, ~.x, .before = 1, .step = 2)
#' slide(1:5, ~.x, .before = 1, .step = 2, .dir = "backward")
#'
#' # ---------------------------------------------------------------------------
#' # Data frames
#'
#' # Data frames are iterated over rowwise
#' slide(mtcars, ~.x)
#'
#' # This means that any column name is easily accessible
#' slide_dbl(mtcars, ~.x$mpg + .x$cyl)
#'
#' # More advanced rowwise iteration is available as well by using the
#' # other arguments
#' slide(mtcars, ~.x, .before = 1, .after = 1)
#'
#' # ---------------------------------------------------------------------------
#' # Cumulative sliding
#'
#' # Using the sentinel value, `unbounded()`, you can ask `slide()` to pin the
#' # start of the sliding window to the first element, effectively creating
#' # a cumulative window
#' slide(1:5, ~.x, .before = unbounded())
#'
#' # Same with `.after`, this creates a window where you start with all of the
#' # elements, but decrease the total number over each iteration
#' slide(1:5, ~.x, .after = unbounded())
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
#' @export
slide_old <- function(.x,
                  .f,
                  ...,
                  .before = 0L,
                  .after = 0L,
                  .step = 1L,
                  .offset = NULL,
                  .partial = FALSE,
                  .dir = "forward",
                  .ptype = list()) {

  if (is.null(.ptype)) {
    .ptype <- list()
    simplify <- TRUE
  } else {
    simplify <- FALSE
  }

  out <- slide_impl(
    .x,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .step = .step,
    .offset = .offset,
    .partial = .partial,
    .dir = .dir,
    .ptype = .ptype
  )

  if (simplify) {
    out <- vec_simplify(out)
  }

  out
}

#' @rdname slide
#' @export
slide_dbl <- function(.x,
                      .f,
                      ...,
                      .before = 0L,
                      .after = 0L,
                      .step = 1L,
                      .offset = NULL,
                      .partial = FALSE,
                      .dir = "forward") {
  slide(
    .x,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .step = .step,
    .offset = .offset,
    .partial = .partial,
    .dir = .dir,
    .ptype = dbl()
  )
}

#' @inheritParams vctrs::vec_rbind
#' @rdname slide
#' @export
slide_dfr <- function(.x,
                      .f,
                      ...,
                      .before = 0L,
                      .after = 0L,
                      .step = 1L,
                      .offset = NULL,
                      .partial = FALSE,
                      .dir = "forward",
                      .names_to = NULL,
                      .name_repair = c("unique", "universal", "check_unique")) {
  out <- slide(
    .x,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .step = .step,
    .offset = .offset,
    .partial = .partial,
    .dir = .dir
  )

  vec_rbind(!!!out, .names_to = .names_to, .name_repair = .name_repair)
}

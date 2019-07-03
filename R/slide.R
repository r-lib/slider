#' Slide
#'
#'
#'
#' @param .x A list or atomic vector.
#'
#' @param .f A function or formula.
#'
#' @param ... Additional arguments passed on to the mapped function.
#'
#' @param .size A single positive integer. The size of the sliding window.
#'
#' @param .step A single positive integer. The size of the step between each
#'   function evaluation.
#'
#' @param .align The alignment to begin the placement of results in the output
#' vector.
#'
#'   One of:
#'   * `"right"`
#'   * `"left"`
#'   * `"center"`
#'   * `"center-left"`
#'   * `"center-right"`
#'
#' When `.size` is even, `"center"` is assumed to be
#' equivalent to `"center-left"`.
#'
#' @param .partial Should partial results be computed?
#' If `TRUE` and `.dir == "forward"`, this may result in partial results at the
#' _end_ of the vector. If `.dir == "backwards"`, this may result in partial
#' results at the _start_ of the vector.
#'
#' @param .dir The direction to slide.
#'
#' One of:
#' - `"forward"`
#' - `"backward"`
#'
#' @export
slide <- function(.x,
                  .f,
                  ...,
                  .size = 1L,
                  .step = 1L,
                  .align = "right",
                  .partial = FALSE,
                  .dir = "forward") {

  vec_assert(.x)
  vec_assert(.size, size = 1L)
  vec_assert(.step, size = 1L)
  vec_assert(.align, character(), 1L)
  vec_assert(.partial, logical(), 1L)
  vec_assert(.dir, character(), 1L)

  .size <- vec_cast(.size, integer())
  .step <- vec_cast(.step, integer())

  arg_match(.dir, valid_dir())
  arg_match(.align, valid_align())

  .f <- as_function(.f)
  n <- vec_size(.x)

  if (.size > n) {
    glubort("The size of `.x` ({n}) cannot be less than `.size` ({.size}).")
  }

  if (.size <= 0L) {
    glubort("`.size` must be at least 1, not {.size}.")
  }

  if (.step <= 0L) {
    glubort("`.step` must be at least 1, not {.step}.")
  }

  forward <- .dir == "forward"

  out <- new_list(n)

  if (forward) {
    start <- 1L
    stop <- .size
    entry <- entry_init(.size, .align)
  }
  else {
    start <- n
    stop <- n + 1L - .size
    entry <- entry_init(.size, .align) + n - .size
    n <- 1L
    .step <- -.step
  }

  compare <- compare_function(forward)
  bound <- bound_function(forward)

  while(TRUE) {
    i <- seq(from = start, to = stop)

    out[[entry]] <- .f(vec_slice(.x, i), ...)

    start <- start + .step
    stop <- stop + .step
    entry <- entry + .step

    if (is_finished(n, stop, entry, .partial, compare)) {
      break
    }

    if (.partial) {
      stop <- bound(stop, n)
    }
  }

  out
}

valid_dir <- function() {
  c("forward", "backward")
}

valid_align <- function() {
  c("right", "left", "center", "center-left", "center-right")
}

# align = "center" for an even size is treated as "center-left"
entry_init <- function(size, align) {
  switch(
    align,
    "left" = 1L,
    "right" = size,
    "center" =,
    "center-left" = floor(median2(size)),
    "center-right" = ceiling(median2(size)),
    abort("Invalid `align`.")
  )
}

# compute the median from a length
# figured it out after looking at the definition of median on wiki
# https://en.wikipedia.org/wiki/Median
# mimics median(seq_len(n)) but is much faster
# bench::mark(median(seq_len(5)), median2(5))
median2 <- function(n) {
  i <- (n + 1) / 2
  (floor(i) + ceiling(i)) / 2
}

# - First check if we haven't even gone past the end yet with normal stepping
# - If we have, and we aren't doing partial, we are done
# - If we have, and we are doing partial,
#   but we still have a valid location to insert info into, then continue
is_finished <- function(n, stop, entry, partial, compare) {
  if (compare(stop, n)) {
    return(FALSE)
  }

  if (!partial) {
    return(TRUE)
  }

  if (compare(entry, n)) {
    return(FALSE)
  }

  TRUE
}

compare_function <- function(forward) {
  if (forward) {
    `<=`
  }
  else {
    `>=`
  }
}

bound_function <- function(forward) {
  if (forward) {
    # faster pmin
    function(x, n) if (x < n) x else n
  }
  else {
    # faster pmax
    function(x, n) if (x < n) n else x
  }
}

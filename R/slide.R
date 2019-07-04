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
  .x_n <- vec_size(.x)

  if (.size > .x_n) {
    glubort("The size of `.x` ({.x_n}) cannot be less than `.size` ({.size}).")
  }

  if (.size <= 0L) {
    glubort("`.size` must be at least 1, not {.size}.")
  }

  if (.step <= 0L) {
    glubort("`.step` must be at least 1, not {.step}.")
  }

  forward <- .dir == "forward"

  out <- new_list(.x_n)

  iterations_n <- iterations(.x_n, .size, .step, .align, .partial, forward)
  complete_iterations_n <- iterations(.x_n, .size, .step, .align, FALSE, forward)

  if (forward) {
    start <- 1L
    stop <- .size
    entry <- .size - offset_align(.size, .align) + 1L
    endpoint <- .x_n
  }
  else {
    start <- .x_n
    stop <- .x_n - .size + 1L
    entry <- .x_n - offset_align(.size, .align) + 1L
    endpoint <- 1L
    .step <- -.step
  }

  .step_stop <- .step

  for (j in seq_len(iterations_n)) {
    i <- seq(from = start, to = stop)

    out[[entry]] <- .f(vec_slice(.x, i), ...)

    if (.partial && j == complete_iterations_n) {
      stop <- endpoint # set stop to the endpoint value
      .step_stop <- 0L # stop incrementing stop as we can no longer perform a full step
      .partial <- FALSE # no need to check second condition anymore
    }

    start <- start + .step
    stop <- stop + .step_stop
    entry <- entry + .step
  }

  out
}

# ------------------------------------------------------------------------------

valid_dir <- function() {
  c("forward", "backward")
}

valid_align <- function() {
  c("right", "left", "center", "center-left", "center-right")
}

# ------------------------------------------------------------------------------

offset_align <- function(size, align) {
  switch(
    align,
    "left" = size,
    "right" = 1L,
    "center" =,
    "center-left" = ceiling(median2(size)),
    "center-right" = floor(median2(size))
  )
}

offset_dir <- function(forward, size, align) {
  if (forward) {
    offset_align(size, align)
  } else {
    size - offset_align(size, align) + 1L
  }
}

offset <- function(partial, forward, size, align) {
  if (partial) {
    offset_dir(forward, size, align)
  } else {
    1L
  }
}

iterations <- function(n, size, step, align, partial, forward) {
  offset <- offset(partial, forward, size, align)
  ceiling((n - size + offset) / step)
}

# ------------------------------------------------------------------------------

# compute the median from a length
# figured it out after looking at the definition of median on wiki
# https://en.wikipedia.org/wiki/Median
# mimics median(seq_len(n)) but is much faster
# bench::mark(median(seq_len(5)), median2(5))
median2 <- function(n) {
  i <- (n + 1) / 2
  (floor(i) + ceiling(i)) / 2
}

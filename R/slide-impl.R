slide_impl <- function(.x,
                       .f,
                       ...,
                       .size,
                       .step,
                       .align,
                       .partial,
                       .dir,
                       .ptype) {

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

  out <- vec_init(.ptype, n = .x_n)

  assign <- get_assigner(.ptype)

  # Number of "complete" iterations (where .partial is not involved)
  complete_iterations_n <- iterations(.x_n, .size, .step, .align, FALSE, forward)

  if (.partial) {
    max_iterations_n <- iterations(.x_n, .size, .step, .align, .partial, forward)
    partial_iterations_n <- max_iterations_n - complete_iterations_n
  }

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

  i <- seq(from = start, to = stop)

  for (j in seq_len(complete_iterations_n)) {
    elt <- .f(vec_slice(.x, i), ...)
    # will be way more efficient at the C level with `copy = FALSE`
    out <- assign(out, entry, value = elt)

    i <- i + .step
    entry <- entry + .step
  }

  # Done if no `.partial`
  if (!.partial) {
    return(out)
  }

  start <- i[1] # current `start` point
  i <- seq(from = start, to = endpoint)

  for (j in seq_len(partial_iterations_n)) {
    elt <- .f(vec_slice(.x, i), ...)
    out <- assign(out, entry, value = elt)

    i <- i + .step
    entry <- entry + .step
  }

  out
}

# ------------------------------------------------------------------------------

# Need something that can assign elements of a list and of
# a non recursive vector
# https://github.com/r-lib/vctrs/issues/141
get_assigner <- function(x) {
  if (is.recursive(x)) {
    `[[<-` # SET_VECTOR_ELT
  } else {
    vec_assign # vec_assign_impl(copy = FALSE)
  }
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

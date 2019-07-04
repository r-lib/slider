stretch <- function(.x,
                    .f,
                    ...,
                    .size = 1L,
                    .step = 1L,
                    .align = "right",
                    .final = FALSE,
                    .dir = "forward") {

  vec_assert(.x)
  vec_assert(.size, size = 1L)
  vec_assert(.step, size = 1L)
  vec_assert(.align, character(), 1L)
  vec_assert(.final, logical(), 1L)
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

  iterations_n <- iterations(.x_n, .size, .step, .align, FALSE, forward)

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

  for (j in seq_len(iterations_n)) {
    i <- seq(from = start, to = stop)

    out[[entry]] <- .f(vec_slice(.x, i), ...)

    stop <- stop + .step
    entry <- entry + .step
  }

  if (.final && iterations_n != .x_n) {
    if (forward) {
      out[[endpoint]] <- .f(.x, ...)
    } else {
      i <- seq(from = .x_n, to = endpoint)
      out[[endpoint]] <- .f(vec_slice(.x, i), ...)
    }
  }

  out
}

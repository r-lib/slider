stretch <- function(.x,
                  .f,
                  ...,
                  .size = 1L,
                  .step = 1L,
                  .align = "right",
                  .final = FALSE,
                  .dir = "forward") {

  .f <- as_function(.f)
  n <- vec_size(.x)

  if (.size > n) {
    abort("The size of `.x` cannot be less than `.size`.")
  }

  validate_direction(.dir)

  out <- new_list(n)

  start <- 1L
  stop <- .size

  # compute initial entry before adjusting `n`!
  entry <- compute_initial_entry(.size, .align)

  if (.dir == "backward") {
    entry <- entry + n - .size
    start <- n + 1L - start
    stop <- n + 1L - stop
    n <- 1L
    .step <- -.step
  }

  compare <- direction_compare_function(.dir)
  bound <- direction_bound_function(.dir)

  while(TRUE) {
    i <- seq(from = start, to = stop)

    out[[entry]] <- .f(vec_slice(.x, i), ...)

    stop <- stop + .step
    entry <- entry + .step

    if (is_finished(n, stop, entry, FALSE, compare)) {
      if (.final && (entry - .step) != n) {
        entry <- n
        i <- seq(from = start, to = n)
        out[[entry]] <- .f(vec_slice(.x, i), ...)
      }
      break
    }

  }

  out
}

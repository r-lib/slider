slide <- function(.x,
                  .f,
                  ...,
                  .size = 1L,
                  .step = 1L,
                  .align = "right",
                  .partial = FALSE,
                  .dir = "forward") {

  .f <- as_function(.f)
  n <- vec_size(.x)

  if (.size > n) {
    abort("The size of `.x` cannot be less than `.size`.")
  }

  validate_direction(.dir)

  out <- new_list(n)

  if (.dir == "forward") {
    start <- 1L
    stop <- .size
    entry <- compute_initial_entry(.size, .align)
  }
  else {
    start <- n
    stop <- n + 1L - .size
    entry <- compute_initial_entry(.size, .align) + n - .size
    n <- 1L
    .step <- -.step
  }

  compare <- direction_compare_function(.dir)
  bound <- direction_bound_function(.dir)

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

validate_direction <- function(dir) {
  if (!dir %in% c("forward", "backward")) {
    abort("`.dir` must be either 'forward' or 'backward'.")
  }
  invisible(dir)
}

# align = "center" for an even size is treated as "center-left"
compute_initial_entry <- function(size, align) {
  switch(
    align,
    "left" = 1L,
    "right" = size,
    "center" =,
    "center-left" = floor(median(seq_len(size))),
    "center-right" = ceiling(median(seq_len(size))),
    abort("Invalid `align`.")
  )
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

direction_compare_function <- function(.dir) {
  if (.dir == "forward") {
    `<=`
  }
  else {
    `>=`
  }
}

direction_bound_function <- function(.dir) {
  if (.dir == "forward") {
    # faster pmin
    function(x, n) if (x < n) x else n
  }
  else {
    # faster pmax
    function(x, n) if (x < n) n else x
  }
}

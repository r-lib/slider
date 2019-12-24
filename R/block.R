block <- function(x, i, by = "year", every = 1L, origin = NULL) {
  vec_assert(x)

  boundaries <- warp_boundary(i, by = by, every = every, origin = origin)

  x_size <- vec_size(x)
  i_size <- vec_size(i)

  check_block_index_size(x_size, i_size)
  check_block_index_ascending(i)
  check_not_na(i, "`i`")

  .Call(slide_block, x, boundaries$start, boundaries$stop)
}

check_block_index_size <- function(x_size, i_size) {
  if (x_size == i_size) {
    return(invisible())
  }

  glubort("The size of `x` ({x_size}) and `i` ({i_size}) must be the same.")
}

check_block_index_ascending <- function(i) {
  i <- unclass(i)

  not_ok <- is.unsorted(i, strictly = FALSE)

  if (not_ok) {
    at <- which(diff(i) < 0L)
    at <- collapse_and_trim(at)
    glubort(
      "`i` must be in ascending order. ",
      "At the following locations, it is not: {at}."
    )
  }

  invisible(i)
}

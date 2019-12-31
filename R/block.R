#' Break a vector into blocks
#'
#' @description
#' `block()` splits `x` into blocks defined by `i`, a date time index. For
#' example, it can split `x` into monthly or yearly blocks. Combined with
#' `purrr::map()` or `lapply()`, it is one way to iterate over a vector in
#' "time blocks".
#'
#' @details
#' `block()` determines the indices to block by with [warp::warp_boundary()],
#' and splits `x` by those indices using [vctrs::vec_chop()].
#'
#' Like [slide()], `block()` splits data frame `x` values row wise.
#'
#' @param x `[vector]`
#'
#'   The vector to block.
#'
#' @param i `[Date / POSIXct / POSIXlt]`
#'
#'   The datetime index to block by. There are 3 restrictions on `i`:
#'
#'   - The size of `i` must match the size of `x`, `i` will not be recycled.
#'
#'   - `i` must be an _increasing_ vector, but duplicate values
#'     are allowed.
#'
#'   - `i` is not allowed to have missing values.
#'
#' @inheritParams warp::warp_boundary
#'
#' @return
#' A list containing elements of type `vctrs::vec_ptype(x)`. The size of the
#' output is identical to the number of unique period groups as determined
#' by [warp::warp_boundary()].
#'
#' @export
#' @examples
#' x <- 1:6
#' i <- as.Date("2019-01-01") + c(-2:2, 31)
#'
#' block(i, i, period = "year")
#'
#' # Data frames are split row wise
#' df <- data.frame(x = x, i = i)
#' block(df, i, period = "month")
#'
#' # Iterate over these blocks to apply a function over
#' # non-overlapping period blocks. For example, to compute a
#' # mean over yearly or monthly blocks.
#' vapply(block(x, i, "year"), mean, numeric(1))
#' vapply(block(x, i, "month"), mean, numeric(1))
#'
#' # block by every 2 months, ensuring that we start counting
#' # the 1st of the 2 months from `2019-01-01`
#' block(i, i, period = "month", every = 2, origin = as.Date("2019-01-01"))
#'
#' # Use the `origin` to instead start counting from `2018-12-01`, meaning
#' # that [2018-12, 2019-01] gets bucketed together.
#' block(i, i, period = "month", every = 2, origin = as.Date("2018-12-01"))
block <- function(x, i, period = "year", every = 1L, origin = NULL) {
  vec_assert(x)

  check_block_index_type(i)
  check_block_index_size(x, i)
  check_not_na(i, "`i`")
  check_block_index_ascending(i)

  boundaries <- warp_boundary(i, period = period, every = every, origin = origin)

  .Call(slide_block, x, boundaries$start, boundaries$stop)
}

check_block_index_type <- function(i) {
  ok <- inherits(i, c("Date", "POSIXt"))

  if (ok) {
    return(invisible())
  }

  class <- paste0(class(i), collapse = "/")
  glubort("The index must inherit from 'Date', 'POSIXct', or 'POSIXlt', not `{class}`.")
}

check_block_index_size <- function(x, i) {
  x_size <- vec_size(x)
  i_size <- vec_size(i)

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

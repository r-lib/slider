#' Break a vector into blocks
#'
#' @description
#' `block()` breaks up the `i`-ndex by `period`, and then uses that to define
#' the indices to chop `x` with.
#'
#' For example, it can split `x` into monthly or yearly blocks. Combined with
#' `purrr::map()`, it is a way to iterate over a vector in "time blocks".
#'
#' @details
#' `block()` determines the indices to block by with [warp::warp_boundary()],
#' and splits `x` by those indices using [vctrs::vec_chop()].
#'
#' Like [slide()], `block()` splits data frame `x` values row wise.
#'
#' @inheritParams warp::warp_boundary
#'
#' @param x `[vector]`
#'
#'   The vector to block.
#'
#' @param i `[Date / POSIXct / POSIXlt]`
#'
#'   The datetime index to block by.
#'
#'   There are 3 restrictions on the index:
#'
#'   - The size of the index must match the size of `x`, they will not be
#'     recycled to their common size.
#'
#'   - The index must be an _increasing_ vector, but duplicate values
#'     are allowed.
#'
#'   - The index cannot have missing values.
#'
#' @return
#' A vector fulfilling the following invariants:
#'
#'  * `vec_size(block(x)) == vec_size(unique(warp::warp_boundary(i)))`
#'
#'  * `vec_ptype(block(x)) == list()`
#'
#'  * `vec_ptype(block(x)[[1]]) == vec_ptype(x)`
#'
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
#'
#' @seealso [slide_period()], [slide()], [slide_index()]
#' @export
block <- function(x, i, period, every = 1L, origin = NULL) {
  vec_assert(x)

  check_index_incompatible_type(i, "i")
  check_index_cannot_be_na(i, "i")
  check_index_must_be_ascending(i, "i")

  x_size <- vec_size(x)
  i_size <- vec_size(i)

  if (x_size != i_size) {
    stop_index_incompatible_size(i_size, x_size, "i")
  }

  boundaries <- warp_boundary(
    i,
    period = period,
    every = every,
    origin = origin
  )

  .Call(slider_block, x, boundaries$start, boundaries$stop)
}

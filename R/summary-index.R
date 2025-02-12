#' Specialized sliding functions relative to an index
#'
#' @description
#' These functions are specialized variants of the most common ways that
#' [slide_index()] is generally used. Notably, [slide_index_sum()] can be used
#' for rolling sums relative to an index (like a Date column), and
#' [slide_index_mean()] can be used for rolling averages.
#'
#' These specialized variants are _much_ faster and more memory efficient than
#' using an otherwise equivalent call constructed with [slide_index_dbl()]
#' or [slide_index_lgl()], especially with a very wide window.
#'
#' @details
#' For more details about the implementation, see the help page of
#' [slide_sum()].
#'
#' @inheritParams rlang::args_dots_empty
#' @inheritParams slide_index
#'
#' @param x `[vector]`
#'
#'   A vector to compute the sliding function on.
#'
#'   - For sliding sum, mean, prod, min, and max, `x` will be cast to a double
#'   vector with [vctrs::vec_cast()].
#'
#'   - For sliding any and all, `x` will be cast to a logical vector with
#'   [vctrs::vec_cast()].
#'
#' @param na_rm `[logical(1)]`
#'
#'   Should missing values be removed from the computation?
#'
#' @return
#' A vector the same size as `x` containing the result of applying the
#' summary function over the sliding windows.
#'
#' - For sliding sum, mean, prod, min, and max, a double vector will be
#' returned.
#'
#' - For sliding any and all, a logical vector will be returned.
#'
#' @seealso [slide_sum()]
#'
#' @export
#' @name summary-index
#' @examples
#' x <- c(1, 5, 3, 2, 6, 10)
#' i <- as.Date("2019-01-01") + c(0, 1, 3, 4, 6, 8)
#'
#' # `slide_index_sum()` can be used for rolling sums relative to an index,
#' # allowing you to "respect gaps" in your series. Notice that the rolling
#' # sum in row 3 is only computed from `2019-01-04` and `2019-01-02` since
#' # `2019-01-01` is more than two days before the current date.
#' data.frame(
#'   i = i,
#'   x = x,
#'   roll = slide_index_sum(x, i, before = 2)
#' )
#'
#' # `slide_index_mean()` can be used for rolling averages
#' slide_index_mean(x, i, before = 2)
#'
#' # Only evaluate the sum on windows that have the potential to be complete
#' slide_index_sum(x, i, before = 2, after = 1, complete = TRUE)
slide_index_sum <- function(
  x,
  i,
  ...,
  before = 0L,
  after = 0L,
  complete = FALSE,
  na_rm = FALSE
) {
  check_dots_empty0(...)
  slide_index_summary(
    x,
    i,
    before,
    after,
    complete,
    na_rm,
    slide_index_sum_core
  )
}

slide_index_sum_core <- function(
  x,
  i,
  starts,
  stops,
  peer_sizes,
  complete,
  na_rm
) {
  .Call(slider_index_sum_core, x, i, starts, stops, peer_sizes, complete, na_rm)
}

# ------------------------------------------------------------------------------

#' @rdname summary-index
#' @export
slide_index_prod <- function(
  x,
  i,
  ...,
  before = 0L,
  after = 0L,
  complete = FALSE,
  na_rm = FALSE
) {
  check_dots_empty0(...)
  slide_index_summary(
    x,
    i,
    before,
    after,
    complete,
    na_rm,
    slide_index_prod_core
  )
}

slide_index_prod_core <- function(
  x,
  i,
  starts,
  stops,
  peer_sizes,
  complete,
  na_rm
) {
  .Call(
    slider_index_prod_core,
    x,
    i,
    starts,
    stops,
    peer_sizes,
    complete,
    na_rm
  )
}

# ------------------------------------------------------------------------------

#' @rdname summary-index
#' @export
slide_index_mean <- function(
  x,
  i,
  ...,
  before = 0L,
  after = 0L,
  complete = FALSE,
  na_rm = FALSE
) {
  check_dots_empty0(...)
  slide_index_summary(
    x,
    i,
    before,
    after,
    complete,
    na_rm,
    slide_index_mean_core
  )
}

slide_index_mean_core <- function(
  x,
  i,
  starts,
  stops,
  peer_sizes,
  complete,
  na_rm
) {
  .Call(
    slider_index_mean_core,
    x,
    i,
    starts,
    stops,
    peer_sizes,
    complete,
    na_rm
  )
}

# ------------------------------------------------------------------------------

#' @rdname summary-index
#' @export
slide_index_min <- function(
  x,
  i,
  ...,
  before = 0L,
  after = 0L,
  complete = FALSE,
  na_rm = FALSE
) {
  check_dots_empty0(...)
  slide_index_summary(
    x,
    i,
    before,
    after,
    complete,
    na_rm,
    slide_index_min_core
  )
}

slide_index_min_core <- function(
  x,
  i,
  starts,
  stops,
  peer_sizes,
  complete,
  na_rm
) {
  .Call(slider_index_min_core, x, i, starts, stops, peer_sizes, complete, na_rm)
}

# ------------------------------------------------------------------------------

#' @rdname summary-index
#' @export
slide_index_max <- function(
  x,
  i,
  ...,
  before = 0L,
  after = 0L,
  complete = FALSE,
  na_rm = FALSE
) {
  check_dots_empty0(...)
  slide_index_summary(
    x,
    i,
    before,
    after,
    complete,
    na_rm,
    slide_index_max_core
  )
}

slide_index_max_core <- function(
  x,
  i,
  starts,
  stops,
  peer_sizes,
  complete,
  na_rm
) {
  .Call(slider_index_max_core, x, i, starts, stops, peer_sizes, complete, na_rm)
}

# ------------------------------------------------------------------------------

#' @rdname summary-index
#' @export
slide_index_all <- function(
  x,
  i,
  ...,
  before = 0L,
  after = 0L,
  complete = FALSE,
  na_rm = FALSE
) {
  check_dots_empty0(...)
  slide_index_summary(
    x,
    i,
    before,
    after,
    complete,
    na_rm,
    slide_index_all_core
  )
}

slide_index_all_core <- function(
  x,
  i,
  starts,
  stops,
  peer_sizes,
  complete,
  na_rm
) {
  .Call(slider_index_all_core, x, i, starts, stops, peer_sizes, complete, na_rm)
}

# ------------------------------------------------------------------------------

#' @rdname summary-index
#' @export
slide_index_any <- function(
  x,
  i,
  ...,
  before = 0L,
  after = 0L,
  complete = FALSE,
  na_rm = FALSE
) {
  check_dots_empty0(...)
  slide_index_summary(
    x,
    i,
    before,
    after,
    complete,
    na_rm,
    slide_index_any_core
  )
}

slide_index_any_core <- function(
  x,
  i,
  starts,
  stops,
  peer_sizes,
  complete,
  na_rm
) {
  .Call(slider_index_any_core, x, i, starts, stops, peer_sizes, complete, na_rm)
}

# ------------------------------------------------------------------------------

slide_index_summary <- function(
  x,
  i,
  before,
  after,
  complete,
  na_rm,
  fn_core,
  slider_error_call = caller_env()
) {
  info <- slide_index_info(
    i = i,
    before = before,
    after = after,
    i_arg = "i",
    before_arg = "before",
    after_arg = "after",
    slider_error_call = slider_error_call
  )

  x_size <- compute_size(x, -1L)
  i_size <- vec_size(i)

  if (i_size != x_size) {
    stop_index_incompatible_size(i_size, x_size, "i", call = slider_error_call)
  }

  complete <- check_complete(complete, "complete", call = slider_error_call)

  i <- info$i
  starts <- info$starts
  stops <- info$stops
  peer_sizes <- info$peer_sizes

  fn_core(x, i, starts, stops, peer_sizes, complete, na_rm)
}

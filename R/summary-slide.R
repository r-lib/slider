#' Specialized sliding functions
#'
#' @description
#' These functions are specialized variants of the most common ways that
#' [slide()] is generally used. Notably, [slide_sum()] can be used for
#' rolling sums, and [slide_mean()] can be used for rolling averages.
#'
#' These specialized variants are _much_ faster and more memory efficient
#' than using an otherwise equivalent call constructed with [slide_dbl()]
#' or [slide_lgl()], especially with a very wide window.
#'
#' @details
#' Note that these functions are _not_ generic and do not respect method
#' dispatch of the corresponding summary function (i.e. [base::sum()],
#' [base::mean()]). Input will always be cast to a double or logical vector
#' using [vctrs::vec_cast()], and an internal method for computing the summary
#' function will be used.
#'
#' Due to the structure of segment trees, `slide_mean()` does not perform the
#' same "two pass" mean that `mean()` does (the intention of the second pass is
#' to perform a floating point error correction). Because of this, there may be
#' small differences between `slide_mean(x)` and `slide_dbl(x, mean)` in some
#' cases.
#'
#' @inheritParams rlang::args_dots_empty
#' @inheritParams slide
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
#' @section Implementation:
#'
#' These variants are implemented using a data structure known as a
#' _segment tree_, which allows for extremely fast repeated range queries
#' without loss of precision.
#'
#' One alternative to segment trees is to directly recompute the summary
#' function on each full window. This is what is done by using, for example,
#' `slide_dbl(x, sum)`. This is extremely slow with large window sizes and
#' wastes a lot of effort recomputing nearly the same information on each
#' window. It can be made slightly faster by moving the sum to C to avoid
#' intermediate allocations, but it still fairly slow.
#'
#' A second alternative is to use an _online_ algorithm, which uses information
#' from the previous window to compute the next window. These are extremely
#' fast, only requiring a single pass through the data, but often suffer from
#' numerical instability issues.
#'
#' Segment trees are an attempt to reconcile the performance issues of the
#' direct approach with the numerical issues of the online approach. The
#' performance of segment trees isn't quite as fast as online algorithms, but is
#' close enough that it should be usable on most large data sets without any
#' issues. Unlike online algorithms, segment trees don't suffer from any
#' extra numerical instability issues.
#'
#' @references
#' Leis, Kundhikanjana, Kemper, and Neumann (2015). "Efficient Processing of
#' Window Functions in Analytical SQL Queries".
#' https://dl.acm.org/doi/10.14778/2794367.2794375
#'
#' @seealso [slide_index_sum()]
#'
#' @export
#' @name summary-slide
#' @examples
#' x <- c(1, 5, 3, 2, 6, 10)
#'
#' # `slide_sum()` can be used for rolling sums.
#' # The following are equivalent, but `slide_sum()` is much faster.
#' slide_sum(x, before = 2)
#' slide_dbl(x, sum, .before = 2)
#'
#' # `slide_mean()` can be used for rolling averages
#' slide_mean(x, before = 2)
#'
#' # Only evaluate the sum on complete windows
#' slide_sum(x, before = 2, after = 1, complete = TRUE)
#'
#' # Skip every other calculation
#' slide_sum(x, before = 2, step = 2)
slide_sum <- function(
  x,
  ...,
  before = 0L,
  after = 0L,
  step = 1L,
  complete = FALSE,
  na_rm = FALSE
) {
  check_dots_empty0(...)
  .Call(slider_sum, x, before, after, step, complete, na_rm)
}

#' @rdname summary-slide
#' @export
slide_prod <- function(
  x,
  ...,
  before = 0L,
  after = 0L,
  step = 1L,
  complete = FALSE,
  na_rm = FALSE
) {
  check_dots_empty0(...)
  .Call(slider_prod, x, before, after, step, complete, na_rm)
}

#' @rdname summary-slide
#' @export
slide_mean <- function(
  x,
  ...,
  before = 0L,
  after = 0L,
  step = 1L,
  complete = FALSE,
  na_rm = FALSE
) {
  check_dots_empty0(...)
  .Call(slider_mean, x, before, after, step, complete, na_rm)
}

#' @rdname summary-slide
#' @export
slide_min <- function(
  x,
  ...,
  before = 0L,
  after = 0L,
  step = 1L,
  complete = FALSE,
  na_rm = FALSE
) {
  check_dots_empty0(...)
  .Call(slider_min, x, before, after, step, complete, na_rm)
}

#' @rdname summary-slide
#' @export
slide_max <- function(
  x,
  ...,
  before = 0L,
  after = 0L,
  step = 1L,
  complete = FALSE,
  na_rm = FALSE
) {
  check_dots_empty0(...)
  .Call(slider_max, x, before, after, step, complete, na_rm)
}

#' @rdname summary-slide
#' @export
slide_all <- function(
  x,
  ...,
  before = 0L,
  after = 0L,
  step = 1L,
  complete = FALSE,
  na_rm = FALSE
) {
  check_dots_empty0(...)
  .Call(slider_all, x, before, after, step, complete, na_rm)
}

#' @rdname summary-slide
#' @export
slide_any <- function(
  x,
  ...,
  before = 0L,
  after = 0L,
  step = 1L,
  complete = FALSE,
  na_rm = FALSE
) {
  check_dots_empty0(...)
  .Call(slider_any, x, before, after, step, complete, na_rm)
}

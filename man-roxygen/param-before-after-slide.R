#' @param .before,.after `[integer(1) / Inf]`
#'
#'   The number of values before or after the current element to
#'   include in the sliding window. Set to `Inf` to select all elements
#'   before or after the current element. Negative values are allowed, which
#'   allows you to "look forward" from the current element if used as the
#'   before value, or "look backwards" if used as the after value.

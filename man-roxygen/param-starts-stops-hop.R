#' @param .starts,.stops `[integer]`
#'
#'   Vectors of boundary locations that make up the windows to bucket `.x` with.
#'   Both `.starts` and `.stops` will be recycled to their common size, and
#'   that common size will be the size of the result. Both vectors should be
#'   integer locations along `.x`, but out-of-bounds values are allowed.

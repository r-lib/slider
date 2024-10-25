#' @param .starts,.stops `[vector]`
#'
#'   Vectors of boundary values that make up the windows to bucket `.i` with.
#'   Both `.starts` and `.stops` will be recycled to their common size, and
#'   that common size will be the size of the result. Both vectors will be cast
#'   to the type of `.i` using [vctrs::vec_cast()]. These boundaries are both
#'   _inclusive_, meaning that the slice of `.x` that will be used in each call
#'   to `.f` is where `.i >= start & .i <= stop` returns `TRUE`.

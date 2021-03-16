#' @param .before,.after `[vector(1) / function / Inf]`
#'
#'   - If a vector of size 1, these represent the number of values before or
#'   after the current element of `.i` to include in the sliding window.
#'   Negative values are allowed, which allows you to "look forward" from the
#'   current element if used as the `.before` value, or "look backwards" if used
#'   as `.after`. Boundaries are computed from these elements as `.i - .before`
#'   and `.i + .after`. Any object that can be added or subtracted from `.i`
#'   with `+` and `-` can be used. For example, a lubridate period, such as
#'   [lubridate::weeks()].
#'
#'   - If `Inf`, this selects all elements before or after the current element.
#'
#'   - If a function, or a one-sided formula which can be coerced to a function,
#'   it is applied to `.i` to compute the boundaries. Note that this function
#'   will only be applied to the _unique_ values of `.i`, so it should not rely
#'   on the original length of `.i` in any way. This is useful for applying a
#'   complex arithmetic operation that can't be expressed with a single `-` or
#'   `+` operation. One example would be to use [lubridate::add_with_rollback()]
#'   to avoid invalid dates at the end of the month.
#'
#'   The ranges that result from applying `.before` and `.after` have the same
#'   3 restrictions as `.i` itself.

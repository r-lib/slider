#' @param .before,.after `[vector(1) / Inf]`
#'
#'   The number of values before or after the current element of `.i` to
#'   include in the sliding window. Set to `Inf` to select all elements
#'   before or after the current element. Negative values are allowed, which
#'   allows you to "look forward" from the current element if used as the
#'   `.before` value, or "look backwards" if used as `.after`.
#'
#'   Any object that can be added or subtracted from `.i` with `+` and `-`
#'   can be used. For example, a lubridate period, such as [lubridate::weeks()].
#'
#'   The ranges that result from computing `.i - .before` and `.i + .after`
#'   have the same 3 restrictions as `.i` itself.

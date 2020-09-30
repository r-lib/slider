# Notes:
# Long double arithmetic is quite slow, but is required to get good precision
# and to match base R. It is especially slow when there are `NA`, `NaN`,
# `Inf`, or `-Inf` values involved. In these cases it is often much faster
# to set `na_rm = TRUE` to at least remove the missing values

slide_sum <- function(x,
                      before = 0L,
                      after = 0L,
                      step = 1L,
                      complete = FALSE,
                      na_rm = FALSE) {
  .Call(slider_sum, x, before, after, step, complete, na_rm)
}

slide_prod <- function(x,
                       before = 0L,
                       after = 0L,
                       step = 1L,
                       complete = FALSE,
                       na_rm = FALSE) {
  .Call(slider_prod, x, before, after, step, complete, na_rm)
}

slide_mean <- function(x,
                       before = 0L,
                       after = 0L,
                       step = 1L,
                       complete = FALSE,
                       na_rm = FALSE) {
  .Call(slider_mean, x, before, after, step, complete, na_rm)
}

slide_min <- function(x,
                      before = 0L,
                      after = 0L,
                      step = 1L,
                      complete = FALSE,
                      na_rm = FALSE) {
  .Call(slider_min, x, before, after, step, complete, na_rm)
}

slide_max <- function(x,
                      before = 0L,
                      after = 0L,
                      step = 1L,
                      complete = FALSE,
                      na_rm = FALSE) {
  .Call(slider_max, x, before, after, step, complete, na_rm)
}

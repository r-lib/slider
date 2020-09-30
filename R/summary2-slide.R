# Notes:
# Long double arithmetic is quite slow, but is required to get good precision
# and to match base R. It is especially slow when there are `NA`, `NaN`,
# `Inf`, or `-Inf` values involved. In these cases it is often much faster
# to set `na_rm = TRUE` to at least remove the missing values

slide_sum2 <- function(x,
                      before = 0L,
                      after = 0L,
                      step = 1L,
                      complete = FALSE,
                      na_rm = FALSE) {
  .Call(slider_sum2, x, before, after, step, complete, na_rm)
}

slide_prod2 <- function(x,
                        before = 0L,
                        after = 0L,
                        step = 1L,
                        complete = FALSE,
                        na_rm = FALSE) {
  .Call(slider_prod2, x, before, after, step, complete, na_rm)
}

slide_mean2 <- function(x,
                        before = 0L,
                        after = 0L,
                        step = 1L,
                        complete = FALSE,
                        na_rm = FALSE) {
  .Call(slider_mean2, x, before, after, step, complete, na_rm)
}

slide_min2 <- function(x,
                       before = 0L,
                       after = 0L,
                       step = 1L,
                       complete = FALSE,
                       na_rm = FALSE) {
  .Call(slider_min2, x, before, after, step, complete, na_rm)
}

slide_max2 <- function(x,
                       before = 0L,
                       after = 0L,
                       step = 1L,
                       complete = FALSE,
                       na_rm = FALSE) {
  .Call(slider_max2, x, before, after, step, complete, na_rm)
}

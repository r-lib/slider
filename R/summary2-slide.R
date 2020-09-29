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

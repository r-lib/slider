slide_sum2 <- function(x,
                      before = 0L,
                      after = 0L,
                      step = 1L,
                      complete = FALSE,
                      na_rm = FALSE) {
  .Call(slider_sum2, x, before, after, step, complete, na_rm)
}

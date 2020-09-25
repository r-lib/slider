slide_sum <- function(x,
                      before = 0L,
                      after = 0L,
                      step = 1L,
                      complete = FALSE,
                      na_rm = FALSE) {
  .Call(slider_sum, x, before, after, step, complete, na_rm)
}

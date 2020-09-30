slide_index_sum2 <- function(x,
                            i,
                            before = 0L,
                            after = 0L,
                            complete = FALSE,
                            na_rm = FALSE) {
  slide_index_summary(x, i, before, after, complete, na_rm, slide_index_sum_core)
}

slide_index_sum_core <- function(x, i, starts, stops, indices, complete, na_rm) {
  .Call(slider_index_sum_core, x, i, starts, stops, indices, complete, na_rm)
}

# ------------------------------------------------------------------------------

slide_index_prod2 <- function(x,
                             i,
                             before = 0L,
                             after = 0L,
                             complete = FALSE,
                             na_rm = FALSE) {
  slide_index_summary(x, i, before, after, complete, na_rm, slide_index_prod_core)
}

slide_index_prod_core <- function(x, i, starts, stops, indices, complete, na_rm) {
  .Call(slider_index_prod_core, x, i, starts, stops, indices, complete, na_rm)
}

# ------------------------------------------------------------------------------

slide_index_mean2 <- function(x,
                              i,
                              before = 0L,
                              after = 0L,
                              complete = FALSE,
                              na_rm = FALSE) {
  slide_index_summary(x, i, before, after, complete, na_rm, slide_index_mean_core)
}

slide_index_mean_core <- function(x, i, starts, stops, indices, complete, na_rm) {
  .Call(slider_index_mean_core, x, i, starts, stops, indices, complete, na_rm)
}

# ------------------------------------------------------------------------------

slide_index_min2 <- function(x,
                             i,
                             before = 0L,
                             after = 0L,
                             complete = FALSE,
                             na_rm = FALSE) {
  slide_index_summary(x, i, before, after, complete, na_rm, slide_index_min_core)
}

slide_index_min_core <- function(x, i, starts, stops, indices, complete, na_rm) {
  .Call(slider_index_min_core, x, i, starts, stops, indices, complete, na_rm)
}

# ------------------------------------------------------------------------------

slide_index_max2 <- function(x,
                             i,
                             before = 0L,
                             after = 0L,
                             complete = FALSE,
                             na_rm = FALSE) {
  slide_index_summary(x, i, before, after, complete, na_rm, slide_index_max_core)
}

slide_index_max_core <- function(x, i, starts, stops, indices, complete, na_rm) {
  .Call(slider_index_max_core, x, i, starts, stops, indices, complete, na_rm)
}

# ------------------------------------------------------------------------------

slide_index_summary <- function(x,
                                i,
                                before,
                                after,
                                complete,
                                na_rm,
                                fn_core) {
  info <- slide_index_info(i, before, after)

  x_size <- compute_size(x, -1L)
  i_size <- vec_size(i)

  if (i_size != x_size) {
    stop_index_incompatible_size(i_size, x_size, ".i")
  }

  i <- info$i
  starts <- info$starts
  stops <- info$stops
  indices <- info$indices

  fn_core(x, i, starts, stops, indices, complete, na_rm)
}

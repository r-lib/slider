slide_index_sum <- function(x,
                            i,
                            before = 0L,
                            after = 0L,
                            complete = FALSE,
                            na_rm = FALSE) {
  slide_index_summary(x, i, before, after, complete, na_rm, slide_index_sum_impl)
}

slide_index_sum_impl <- function(x, i, starts, stops, indices, complete, na_rm) {
  .Call(slider_index_sum_impl, x, i, starts, stops, indices, complete, na_rm)
}

# ------------------------------------------------------------------------------

slide_index_mean <- function(x,
                             i,
                             before = 0L,
                             after = 0L,
                             complete = FALSE,
                             na_rm = FALSE) {
  slide_index_summary(x, i, before, after, complete, na_rm, slide_index_mean_impl)
}

slide_index_mean_impl <- function(x, i, starts, stops, indices, complete, na_rm) {
  .Call(slider_index_mean_impl, x, i, starts, stops, indices, complete, na_rm)
}

# ------------------------------------------------------------------------------

slide_index_prod <- function(x,
                             i,
                             before = 0L,
                             after = 0L,
                             complete = FALSE,
                             na_rm = FALSE) {
  slide_index_summary(x, i, before, after, complete, na_rm, slide_index_prod_impl)
}

slide_index_prod_impl <- function(x, i, starts, stops, indices, complete, na_rm) {
  .Call(slider_index_prod_impl, x, i, starts, stops, indices, complete, na_rm)
}

# ------------------------------------------------------------------------------

slide_index_min <- function(x,
                            i,
                            before = 0L,
                            after = 0L,
                            complete = FALSE,
                            na_rm = FALSE) {
  slide_index_summary(x, i, before, after, complete, na_rm, slide_index_min_impl)
}

slide_index_min_impl <- function(x, i, starts, stops, indices, complete, na_rm) {
  .Call(slider_index_min_impl, x, i, starts, stops, indices, complete, na_rm)
}

# ------------------------------------------------------------------------------

slide_index_max <- function(x,
                            i,
                            before = 0L,
                            after = 0L,
                            complete = FALSE,
                            na_rm = FALSE) {
  slide_index_summary(x, i, before, after, complete, na_rm, slide_index_max_impl)
}

slide_index_max_impl <- function(x, i, starts, stops, indices, complete, na_rm) {
  .Call(slider_index_max_impl, x, i, starts, stops, indices, complete, na_rm)
}

# ------------------------------------------------------------------------------

slide_index_summary <- function(x,
                                i,
                                before,
                                after,
                                complete,
                                na_rm,
                                fn_impl) {
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

  fn_impl(x, i, starts, stops, indices, complete, na_rm)
}

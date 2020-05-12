#' Slide relative to an index chunked by period
#'
#' @description
#' `slide_period()` breaks up the `.i`-ndex by `.period`, and then uses that
#' to define the indices to slide over `.x` with.
#'
#' It can be useful for, say, sliding over daily data in monthly chunks.
#'
#' The underlying engine for breaking up `.i` is [warp::warp_distance()].
#' If you need more information about the `.period` types, that is the best
#' place to look.
#'
#' @inheritParams slide
#' @inheritParams warp::warp_distance
#'
#' @template param-before-after-slide
#'
#' @param .i `[Date / POSIXct / POSIXlt]`
#'
#'   A datetime index to break into periods.
#'
#'   There are 3 restrictions on the index:
#'
#'   - The size of the index must match the size of `.x`, they will not be
#'     recycled to their common size.
#'
#'   - The index must be an _increasing_ vector, but duplicate values
#'     are allowed.
#'
#'   - The index cannot have missing values.
#'
#' @return
#' A vector fulfilling the following invariants:
#'
#' \subsection{`slide_period()`}{
#'
#'  * `vec_size(slide_period(.x)) == vec_size(unique(warp::warp_distance(.i)))`
#'
#'  * `vec_ptype(slide_period(.x)) == list()`
#'
#' }
#'
#' \subsection{`slide_period_vec()` and `slide_period_*()` variants}{
#'
#'  * `vec_size(slide_period_vec(.x)) == vec_size(unique(warp::warp_distance(.i)))`
#'
#'  * `vec_size(slide_period_vec(.x)[[1]]) == 1L`
#'
#'  * `vec_ptype(slide_period_vec(.x, .ptype = ptype)) == ptype`
#'
#' }
#'
#' @examples
#' i <- as.Date("2019-01-28") + 0:5
#'
#' # Split `i` into 2-day periods to apply `.f` to
#' slide_period(i, i, "day", identity, .every = 2)
#'
#' # Or into 1-month periods
#' slide_period(i, i, "month", identity)
#'
#' # Now select:
#' # - The current 2-day period
#' # - Plus 1 2-day period before the current one
#' slide_period(i, i, "day", identity, .every = 2, .before = 1)
#'
#' # Alter the `origin` to control the reference date for
#' # how the 2-day groups are formed
#' origin <- as.Date("2019-01-29")
#' slide_period(i, i, "day", identity, .every = 2, .origin = origin)
#'
#' # This can be useful for, say, monthly averages
#' daily_sales <- c(2, 5, 3, 6, 9, 4)
#' slide_period_dbl(daily_sales, i, "month", mean)
#'
#' # If you need the index, slide over and return a data frame
#' sales_df <- data.frame(i = i, sales = daily_sales)
#'
#' slide_period_dfr(
#'   sales_df,
#'   sales_df$i,
#'   "month",
#'   ~data.frame(
#'      i = max(.x$i),
#'      sales = mean(.x$sales)
#'    )
#' )
#'
#' # One of the most unique features about `slide_period()` is that it is
#' # aware of how far apart elements of `.i` are in the `.period` you are
#' # interested in. For example, if you do a monthly slide with `i2`, selecting
#' # the current month and 1 month before it, then it will recognize that
#' # `2019-02-01` and `2019-04-01` are not beside each other, and it won't
#' # group them together.
#' i2 <- as.Date(c("2019-01-01", "2019-02-01", "2019-04-01", "2019-05-01"))
#'
#' slide_period(i2, i2, "month", identity, .before = 1)
#'
#' @seealso [block()], [slide()], [slide_index()]
#' @export
slide_period <- function(.x,
                         .i,
                         .period,
                         .f,
                         ...,
                         .every = 1L,
                         .origin = NULL,
                         .before = 0L,
                         .after = 0L,
                         .complete = FALSE) {
  slide_period_impl(
    .x,
    .i,
    .period,
    .f,
    ...,
    .every = .every,
    .origin = .origin,
    .before = .before,
    .after = .after,
    .complete = .complete,
    .ptype = list(),
    .constrain = FALSE,
    .atomic = FALSE
  )
}

#' @rdname slide_period
#' @export
slide_period_vec <- function(.x,
                             .i,
                             .period,
                             .f,
                             ...,
                             .every = 1L,
                             .origin = NULL,
                             .before = 0L,
                             .after = 0L,
                             .complete = FALSE,
                             .ptype = NULL) {
  out <- slide_period_impl(
    .x,
    .i,
    .period,
    .f,
    ...,
    .every = .every,
    .origin = .origin,
    .before = .before,
    .after = .after,
    .complete = .complete,
    .ptype = list(),
    .constrain = FALSE,
    .atomic = TRUE
  )

  vec_simplify(out, .ptype)
}

slide_period_vec_direct <- function(.x,
                                    .i,
                                    .period,
                                    .f,
                                    ...,
                                    .every,
                                    .origin,
                                    .before,
                                    .after,
                                    .complete,
                                    .ptype) {
  slide_period_impl(
    .x,
    .i,
    .period,
    .f,
    ...,
    .every = .every,
    .origin = .origin,
    .before = .before,
    .after = .after,
    .complete = .complete,
    .ptype = .ptype,
    .constrain = TRUE,
    .atomic = TRUE
  )
}

#' @rdname slide_period
#' @export
slide_period_dbl <- function(.x,
                             .i,
                             .period,
                             .f,
                             ...,
                             .every = 1L,
                             .origin = NULL,
                             .before = 0L,
                             .after = 0L,
                             .complete = FALSE) {
  slide_period_vec_direct(
    .x,
    .i,
    .period,
    .f,
    ...,
    .every = .every,
    .origin = .origin,
    .before = .before,
    .after = .after,
    .complete = .complete,
    .ptype = double()
  )
}

#' @rdname slide_period
#' @export
slide_period_int <- function(.x,
                             .i,
                             .period,
                             .f,
                             ...,
                             .every = 1L,
                             .origin = NULL,
                             .before = 0L,
                             .after = 0L,
                             .complete = FALSE) {
  slide_period_vec_direct(
    .x,
    .i,
    .period,
    .f,
    ...,
    .every = .every,
    .origin = .origin,
    .before = .before,
    .after = .after,
    .complete = .complete,
    .ptype = integer()
  )
}

#' @rdname slide_period
#' @export
slide_period_lgl <- function(.x,
                             .i,
                             .period,
                             .f,
                             ...,
                             .every = 1L,
                             .origin = NULL,
                             .before = 0L,
                             .after = 0L,
                             .complete = FALSE) {
  slide_period_vec_direct(
    .x,
    .i,
    .period,
    .f,
    ...,
    .every = .every,
    .origin = .origin,
    .before = .before,
    .after = .after,
    .complete = .complete,
    .ptype = logical()
  )
}

#' @rdname slide_period
#' @export
slide_period_chr <- function(.x,
                             .i,
                             .period,
                             .f,
                             ...,
                             .every = 1L,
                             .origin = NULL,
                             .before = 0L,
                             .after = 0L,
                             .complete = FALSE) {
  slide_period_vec_direct(
    .x,
    .i,
    .period,
    .f,
    ...,
    .every = .every,
    .origin = .origin,
    .before = .before,
    .after = .after,
    .complete = .complete,
    .ptype = character()
  )
}

#' @rdname slide_period
#' @export
slide_period_dfr <- function(.x,
                             .i,
                             .period,
                             .f,
                             ...,
                             .every = 1L,
                             .origin = NULL,
                             .before = 0L,
                             .after = 0L,
                             .complete = FALSE,
                             .names_to = rlang::zap(),
                             .name_repair = c("unique", "universal", "check_unique")) {
  out <- slide_period(
    .x,
    .i,
    .period,
    .f,
    ...,
    .every = .every,
    .origin = .origin,
    .before = .before,
    .after = .after,
    .complete = .complete
  )

  vec_rbind(!!!out, .names_to = .names_to, .name_repair = .name_repair)
}

#' @rdname slide_period
#' @export
slide_period_dfc <- function(.x,
                             .i,
                             .period,
                             .f,
                             ...,
                             .every = 1L,
                             .origin = NULL,
                             .before = 0L,
                             .after = 0L,
                             .complete = FALSE,
                             .size = NULL,
                             .name_repair = c("unique", "universal", "check_unique", "minimal")) {
  out <- slide_period(
    .x,
    .i,
    .period,
    .f,
    ...,
    .every = .every,
    .origin = .origin,
    .before = .before,
    .after = .after,
    .complete = .complete
  )

  vec_cbind(!!!out, .size = .size, .name_repair = .name_repair)
}

# ------------------------------------------------------------------------------

slide_period_impl <- function(.x,
                              .i,
                              .period,
                              .f,
                              ...,
                              .every,
                              .origin,
                              .before,
                              .after,
                              .complete,
                              .ptype,
                              .constrain,
                              .atomic) {
  vec_assert(.x)

  .f <- as_function(.f)

  f_call <- expr(.f(.x, ...))

  type <- -1L

  slide_period_common(
    x = .x,
    i = .i,
    period = .period,
    f_call = f_call,
    every = .every,
    origin = .origin,
    before = .before,
    after = .after,
    complete = .complete,
    ptype = .ptype,
    constrain = .constrain,
    atomic = .atomic,
    env = environment(),
    type = type
  )
}

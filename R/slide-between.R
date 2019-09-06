#' Slide between boundaries
#'
#' `slide_between()` is the lower level engine that powers [slide_index()]. It
#' has slightly different invariants than `slide_index()`, and is useful when
#' you either need to hand craft boundary values, or want to compute a result
#' with a size that is different from `.x`.
#'
#' @inheritParams slide_index
#'
#' @param .starts,.stops `[vector]`
#'
#'   Vectors of boundary values that make up the windows to bucket `.i` with.
#'   Both `.starts` and `.stops` will be recycled to their common size, and
#'   that common size will be the size of the result. Both vectors should be
#'   the same type as `.i`. These boundaries are both _inclusive_, meaning
#'   that `.i` will be searched for values that fall into the
#'   range of `[start, stop]`.
#'
#' @section Invariants:
#'
#' - `vec_size_common(.starts, .stops) == vec_size(slide_between(.x, ...))`
#'
#' @examples
#' library(vctrs)
#' library(lubridate, warn.conflicts = FALSE)
#'
#' # ---------------------------------------------------------------------------
#' # Returning a size smaller than `.x`
#'
#' i <- as.Date("2019-01-25") + c(0, 1, 2, 3, 10, 20, 35, 42, 45)
#'
#' # slide_index() allows you to slide relative to `i`
#' slide_index(i, i, ~.x, .before = weeks(1))
#'
#' # But you might be more interested in coarser summaries. This groups
#' # by year-month and computes 2 `.f` on 2 month windows.
#' i_yearmonth <- year(i) + (month(i) - 1) / 12
#' slide_index(i, i_yearmonth, ~.x, .before = 1)
#'
#' # ^ This works nicely when working with dplyr if you are trying to create
#' # a new column in a data frame, but you'll notice that there are really only
#' # 3 months, so only 3 values are being calculated. If you only want to return
#' # a vector of those 3 values, you can use `slide_between()`. You'll have to
#' # hand craft the boundaries, but this is a general strategy
#' # I've found useful:
#' first_start <- floor_date(i[1], "months")
#' last_stop <- ceiling_date(i[length(i)], "months")
#' dates <- seq(first_start, last_stop, "1 month")
#' inner <- dates[2:(length(dates) - 1L)]
#' starts <- vec_c(first_start, inner)
#' stops <- vec_c(inner - 1, last_stop)
#'
#' slide_between(i, i, starts, stops, ~.x)
#'
#' # ---------------------------------------------------------------------------
#' # Non-existant dates with `lubridate::months()`
#'
#' # Imagine you want to compute a 1 month rolling average on this
#' # irregular daily data.
#' i <- vec_c(as.Date("2019-02-27") + 0:3, as.Date("2019-03-27") + 0:5)
#' x <- rnorm(vec_seq_along(i))
#'
#' # You might try `slide_index()` like this, but you'd run into this error
#' cnd <- try(slide_index(x, i, mean, .before = months(1)), silent = TRUE)
#' attr(cnd, "condition")[["message"]]
#'
#' # This is because when you actually compute the `.i - .before` sequence,
#' # you hit non-existant dates. i.e. `"2019-03-29" - months(1)` doesn't exist.
#' i - months(1)
#'
#' # To get around this, lubridate provides `add_with_rollback()`,
#' # and the shortcut operation `%m-%`, which subtracts the month, then rolls
#' # forward/backward if it hits an `NA`. You can manually generate boundaries,
#' # then provide them to `slide_between()`.
#' starts <- i %m-% months(1)
#' stops <- i
#'
#' slide_between(x, i, starts, stops, mean)
#'
#' slide_between(i, i, starts, stops, ~.x)
#'
#' # ---------------------------------------------------------------------------
#' # Business calendars with RcppQuantuccia
#'
#' # Imagine you work in a company with daily costs that fall on these dates.
#' # Notice that these are strictly weekdays that cross over a weekend.
#' i <- as.Date("2019-08-15") + c(0:1, 4, 6, 7)
#' wday(i, label = TRUE)
#'
#' # If you are following a "business calendar" and want to compute a rolling
#' # 2 day computation, you might want `"2019-08-19"` (a Monday) to be paired
#' # with `"2019-08-16"` (a Friday) because they are adjacent in the business
#' # week. Using the RcppQuantuccia package, we can use `advanceUnits()` to
#' # compute the correct business day boundaries for use in `slide_between()`.
#' library(RcppQuantuccia)
#' starts <- advanceUnits(i, -1, "Days")
#' stops <- i
#'
#' # "The current day + 1 business day before"
#' slide_between(i, i, starts, stops, ~.x)
#'
#' @inheritSection slide_index The `.i`-ndex
#' @export
slide_between <- function(.x, .i, .starts, .stops, .f, ...) {
  slide_between_impl(
    .x,
    .i,
    .starts,
    .stops,
    .f,
    ...,
    .constrain = FALSE,
    .ptype = list()
  )
}

#' @rdname slide_between
#' @export
slide_between_vec <- function(.x,
                              .i,
                              .starts,
                              .stops,
                              .f,
                              ...,
                              .ptype = list()) {

  if (is.null(.ptype)) {
    out <- slide_between_vec_simplify(
      .x,
      .i,
      .starts,
      .stops,
      .f,
      ...
    )

    return(out)
  }

  slide_between_impl(
    .x,
    .i,
    .starts,
    .stops,
    .f,
    ...,
    .constrain = TRUE,
    .ptype = .ptype
  )
}

slide_between_vec_simplify <- function(.x,
                                       .i,
                                       .starts,
                                       .stops,
                                       .f,
                                       ...) {
  out <- slide_between(
    .x,
    .i,
    .starts,
    .stops,
    .f,
    ...
  )

  check_all_size_one(out)

  vec_c(!!!out)
}

#' @rdname slide_between
#' @export
slide_between_dbl <- function(.x,
                              .i,
                              .starts,
                              .stops,
                              .f,
                              ...) {
  slide_between_vec(
    .x,
    .i,
    .starts,
    .stops,
    .f,
    ...,
    .ptype = double()
  )
}

#' @rdname slide_between
#' @export
slide_between_int <- function(.x,
                              .i,
                              .starts,
                              .stops,
                              .f,
                              ...) {
  slide_between_vec(
    .x,
    .i,
    .starts,
    .stops,
    .f,
    ...,
    .ptype = integer()
  )
}

#' @rdname slide_between
#' @export
slide_between_lgl <- function(.x,
                              .i,
                              .starts,
                              .stops,
                              .f,
                              ...) {
  slide_between_vec(
    .x,
    .i,
    .starts,
    .stops,
    .f,
    ...,
    .ptype = logical()
  )
}

#' @rdname slide_between
#' @export
slide_between_chr <- function(.x,
                              .i,
                              .starts,
                              .stops,
                              .f,
                              ...) {
  slide_between_vec(
    .x,
    .i,
    .starts,
    .stops,
    .f,
    ...,
    .ptype = character()
  )
}

#' @rdname slide_between
#' @export
slide_between_raw <- function(.x,
                              .i,
                              .starts,
                              .stops,
                              .f,
                              ...) {
  slide_between_vec(
    .x,
    .i,
    .starts,
    .stops,
    .f,
    ...,
    .ptype = raw()
  )
}

#' @rdname slide_between
#' @export
slide_between_dfr <- function(.x,
                              .i,
                              .starts,
                              .stops,
                              .f,
                              ...,
                              .names_to = NULL,
                              .name_repair = c("unique", "universal", "check_unique")) {
  out <- slide_between(
    .x,
    .i,
    .starts,
    .stops,
    .f,
    ...
  )

  vec_rbind(!!!out, .names_to = .names_to, .name_repair = .name_repair)
}

#' @rdname slide_between
#' @export
slide_between_dfc <- function(.x,
                              .i,
                              .starts,
                              .stops,
                              .f,
                              ...,
                              .size = NULL,
                              .name_repair = c("unique", "universal", "check_unique", "minimal")) {
  out <- slide_between(
    .x,
    .i,
    .starts,
    .stops,
    .f,
    ...
  )

  vec_cbind(!!!out, .size = .size, .name_repair = .name_repair)
}

# ------------------------------------------------------------------------------

slide_between_impl <- function(.x, .i, .starts, .stops, .f, ..., .constrain, .ptype) {
  vec_assert(.x)

  .f <- as_function(.f)

  f_call <- expr(.f(.x, ...))

  type <- -1L

  slide_between_common(
    x = .x,
    i = .i,
    starts = .starts,
    stops = .stops,
    f_call = f_call,
    constrain = .constrain,
    ptype = .ptype,
    env = environment(),
    type = type
  )
}

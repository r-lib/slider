#' Hop relative to an index
#'
#' `hop_index()` is the lower level engine that powers [slide_index()]. It
#' has slightly different invariants than `slide_index()`, and is useful when
#' you either need to hand craft boundary values, or want to compute a result
#' with a size that is different from `.x`.
#'
#' @inheritParams slide_index
#'
#' @template param-starts-stops-hop-index
#'
#' @return
#' A vector fulfilling the following invariants:
#'
#' \subsection{`hop_index()`}{
#'
#'  * `vec_size(hop_index(.x, .starts, .stops)) == vec_size_common(.starts, .stops)`
#'
#'  * `vec_ptype(hop_index(.x, .starts, .stops)) == list()`
#'
#' }
#'
#' \subsection{`hop_index_vec()`}{
#'
#'  * `vec_size(hop_index_vec(.x, .starts, .stops)) == vec_size_common(.starts, .stops)`
#'
#'  * `vec_size(hop_index_vec(.x, .starts, .stops)[[1]]) == 1L`
#'
#'  * `vec_ptype(hop_index_vec(.x, .starts, .stops, .ptype = ptype)) == ptype`
#'
#' }
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
#' # a vector of those 3 values, you can use `hop_index()`. You'll have to
#' # hand craft the boundaries, but this is a general strategy
#' # I've found useful:
#' first_start <- floor_date(i[1], "months")
#' last_stop <- ceiling_date(i[length(i)], "months")
#' dates <- seq(first_start, last_stop, "1 month")
#' inner <- dates[2:(length(dates) - 1L)]
#' starts <- vec_c(first_start, inner)
#' stops <- vec_c(inner - 1, last_stop)
#'
#' hop_index(i, i, starts, stops, ~.x)
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
#' library(rlang)
#'
#' with_options(
#'   catch_cnd(
#'     slide_index(x, i, mean, .before = months(1))
#'   ),
#'   rlang_backtrace_on_error = current_env()
#' )
#'
#' # This is because when you actually compute the `.i - .before` sequence,
#' # you hit non-existant dates. i.e. `"2019-03-29" - months(1)` doesn't exist.
#' i - months(1)
#'
#' # To get around this, lubridate provides `add_with_rollback()`,
#' # and the shortcut operation `%m-%`, which subtracts the month, then rolls
#' # forward/backward if it hits an `NA`. You can manually generate boundaries,
#' # then provide them to `hop_index()`.
#' starts <- i %m-% months(1)
#' stops <- i
#'
#' hop_index(x, i, starts, stops, mean)
#'
#' hop_index(i, i, starts, stops, ~.x)
#'
#' @seealso [slide()], [slide_index()], [hop_index2()]
#' @export
hop_index <- function(.x, .i, .starts, .stops, .f, ...) {
  hop_index_impl(
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

#' @rdname hop_index
#' @export
hop_index_vec <- function(.x,
                          .i,
                          .starts,
                          .stops,
                          .f,
                          ...,
                          .ptype = NULL) {

  if (is.null(.ptype)) {
    out <- hop_index_vec_simplify(
      .x,
      .i,
      .starts,
      .stops,
      .f,
      ...
    )

    return(out)
  }

  hop_index_impl(
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

hop_index_vec_simplify <- function(.x,
                                   .i,
                                   .starts,
                                   .stops,
                                   .f,
                                   ...) {
  out <- hop_index(
    .x,
    .i,
    .starts,
    .stops,
    .f,
    ...
  )

  check_all_size_one(out)

  vec_simplify(out)
}

# ------------------------------------------------------------------------------

hop_index_impl <- function(.x, .i, .starts, .stops, .f, ..., .constrain, .ptype) {
  vec_assert(.x)

  .f <- as_function(.f)

  f_call <- expr(.f(.x, ...))

  type <- -1L

  hop_index_common(
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

#' Slide relative to an index
#'
#' @description
#' `slide_index()` is similar to `slide()`, but allows a secondary `.i`-ndex
#' vector to be provided.
#'
#' This is often useful in business calculations, when
#' you want to compute a rolling computation looking "3 months back", which
#' is approximately but not equivalent to, 3 * 30 days. `slide_index()` allows
#' for these irregular window sizes.
#'
#' @inheritParams slide
#'
#' @param .i `[vector]`
#'
#'   The index vector that determines the window sizes. The lower bound
#'   of the window range will be computed as `.i - .before`, and the upper
#'   bound as `.i + .after`. It is fairly common to supply a date vector
#'   as the index, but not required.
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
#' @template param-before-after-slide-index
#'
#' @return
#' A vector fulfilling the following invariants:
#'
#' \subsection{`slide_index()`}{
#'
#'  * `vec_size(slide_index(.x)) == vec_size(.x)`
#'
#'  * `vec_ptype(slide_index(.x)) == list()`
#'
#' }
#'
#' \subsection{`slide_index_vec()` and `slide_index_*()` variants}{
#'
#'  * `vec_size(slide_index_vec(.x)) == vec_size(.x)`
#'
#'  * `vec_size(slide_index_vec(.x)[[1]]) == 1L`
#'
#'  * `vec_ptype(slide_index_vec(.x, .ptype = ptype)) == ptype`
#'
#' }
#'
#' @examples
#' x <- 1:5
#'
#' # In some cases, sliding over `x` with a strict window size of 2
#' # will fit your use case.
#' slide(x, ~.x, .before = 1)
#'
#' # However, if this `i` is a date vector paired with `x`, when computing
#' # rolling calculations you might want to iterate over `x` while
#' # respecting the fact that `i` is an irregular sequence.
#' i <- as.Date("2019-08-15") + c(0:1, 4, 6, 7)
#'
#' # For example, a "2 day" window should not pair `"2019-08-19"` and
#' # `"2019-08-21"` together, even though they are next to each other in `x`.
#' # `slide_index()` computes the lookback value from the current date in `.i`,
#' # meaning that if you are currently on `"2019-08-21"` and look back 1 day,
#' # it will correctly not include `"2019-08-19"`.
#' slide_index(i, i, ~.x, .before = 1)
#'
#' # We could have equivalently used a lubridate period object for this as well,
#' # since `i - lubridate::days(1)` is allowed
#' slide_index(i, i, ~.x, .before = lubridate::days(1))
#'
#' # ---------------------------------------------------------------------------
#'
#' # When `.i` has repeated values, they are always grouped together.
#' i <- c(2017, 2017, 2018, 2019, 2020, 2020)
#' slide_index(i, i, ~.x)
#' slide_index(i, i, ~.x, .after = 1)
#'
#' # ---------------------------------------------------------------------------
#' # Rolling regressions
#'
#' # Rolling regressions are easy with `slide_index()` because:
#' # - Data frame `.x` values are iterated over rowwise
#' # - The index is respected by using `.i`
#' set.seed(123)
#'
#' df <- data.frame(
#'   y = rnorm(100),
#'   x = rnorm(100),
#'   i = as.Date("2019-08-15") + c(0, 2, 4, 6:102) # <- irregular
#' )
#'
#' # 20 day rolling regression. Current day + 19 days back.
#' # Additionally, set `.complete = TRUE` to not compute partial results.
#' regr <- slide_index(df, df$i, ~lm(y ~ x, .x), .before = 19, .complete = TRUE)
#'
#' regr[16:18]
#'
#' # The first 16 slots are NULL because there is no possible way to
#' # look back 19 days from the 16th index position and construct a full
#' # window. But on the 17th index position, `""2019-09-03"`, if we look
#' # back 19 days we get to `""2019-08-15"`, which is the same value as
#' # `i[1]` so a full window can be constructed.
#' i[16] - 19 >= i[1] # FALSE
#' i[17] - 19 >= i[1] # TRUE
#'
#' @seealso [slide()], [hop_index()], [slide_index2()]
#' @export
slide_index <- function(.x,
                        .i,
                        .f,
                        ...,
                        .before = 0L,
                        .after = 0L,
                        .complete = FALSE) {
  slide_index_impl(
    .x,
    .i,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .complete = .complete,
    .constrain = FALSE,
    .ptype = list()
  )
}

#' @rdname slide_index
#' @export
slide_index_vec <- function(.x,
                            .i,
                            .f,
                            ...,
                            .before = 0L,
                            .after = 0L,
                            .complete = FALSE,
                            .ptype = NULL) {

  if (is.null(.ptype)) {
    out <- slide_index_simplify(
      .x,
      .i,
      .f,
      ...,
      .before = .before,
      .after = .after,
      .complete = .complete
    )

    return(out)
  }

  slide_index_impl(
    .x,
    .i,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .complete = .complete,
    .constrain = TRUE,
    .ptype = .ptype
  )
}

slide_index_simplify <- function(.x,
                                 .i,
                                 .f,
                                 ...,
                                 .before,
                                 .after,
                                 .complete) {
  out <- slide_index(
    .x,
    .i,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .complete = .complete
  )

  check_all_size_one(out)

  vec_simplify(out)
}

#' @rdname slide_index
#' @export
slide_index_dbl <- function(.x,
                            .i,
                            .f,
                            ...,
                            .before = 0L,
                            .after = 0L,
                            .complete = FALSE) {
  slide_index_vec(
    .x,
    .i,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .complete = .complete,
    .ptype = double()
  )
}

#' @rdname slide_index
#' @export
slide_index_int <- function(.x,
                            .i,
                            .f,
                            ...,
                            .before = 0L,
                            .after = 0L,
                            .complete = FALSE) {
  slide_index_vec(
    .x,
    .i,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .complete = .complete,
    .ptype = integer()
  )
}

#' @rdname slide_index
#' @export
slide_index_lgl <- function(.x,
                            .i,
                            .f,
                            ...,
                            .before = 0L,
                            .after = 0L,
                            .complete = FALSE) {
  slide_index_vec(
    .x,
    .i,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .complete = .complete,
    .ptype = logical()
  )
}

#' @rdname slide_index
#' @export
slide_index_chr <- function(.x,
                            .i,
                            .f,
                            ...,
                            .before = 0L,
                            .after = 0L,
                            .complete = FALSE) {
  slide_index_vec(
    .x,
    .i,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .complete = .complete,
    .ptype = character()
  )
}

#' @inheritParams vctrs::vec_rbind
#' @rdname slide_index
#' @export
slide_index_dfr <- function(.x,
                            .i,
                            .f,
                            ...,
                            .before = 0L,
                            .after = 0L,
                            .complete = FALSE,
                            .names_to = NULL,
                            .name_repair = c("unique", "universal", "check_unique")) {
  out <- slide_index(
    .x,
    .i,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .complete = .complete
  )

  vec_rbind(!!!out, .names_to = .names_to, .name_repair = .name_repair)
}

#' @inheritParams vctrs::vec_cbind
#' @rdname slide_index
#' @export
slide_index_dfc <- function(.x,
                            .i,
                            .f,
                            ...,
                            .before = 0L,
                            .after = 0L,
                            .complete = FALSE,
                            .size = NULL,
                            .name_repair = c("unique", "universal", "check_unique", "minimal")) {
  out <- slide_index(
    .x,
    .i,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .complete = .complete
  )

  vec_cbind(!!!out, .size = .size, .name_repair = .name_repair)
}

# ------------------------------------------------------------------------------

slide_index_impl <- function(.x,
                             .i,
                             .f,
                             ...,
                             .before,
                             .after,
                             .complete,
                             .constrain,
                             .ptype) {
  vec_assert(.x)

  .f <- as_function(.f)

  f_call <- expr(.f(.x, ...))

  type <- -1L

  slide_index_common(
    x = .x,
    i = .i,
    f_call = f_call,
    before = .before,
    after = .after,
    complete = .complete,
    constrain = .constrain,
    ptype = .ptype,
    env = environment(),
    type = type
  )
}

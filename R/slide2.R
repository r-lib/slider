#' Slide over multiple inputs simultaneously
#'
#' These are variants of [slide()] that iterate over multiple inputs in
#' parallel. They are parallel in the sense that each input is processed in
#' parallel with the others, not in the sense of multicore computing. These
#' functions work similarly to `map2()` and `pmap()` from purrr.
#'
#' @inheritParams slide
#'
#' @template param-x-y
#' @template param-l
#' @template param-before-after-slide
#'
#' @return
#' A vector fulfilling the following invariants:
#'
#' \subsection{`slide2()`}{
#'
#'  * `vec_size(slide2(.x, .y)) == vec_size_common(.x, .y)`
#'
#'  * `vec_ptype(slide2(.x, .y)) == list()`
#'
#' }
#'
#' \subsection{`slide2_vec()` and `slide2_*()` variants}{
#'
#'  * `vec_size(slide2_vec(.x, .y)) == vec_size_common(.x, .y)`
#'
#'  * `vec_size(slide2_vec(.x, .y)[[1]]) == 1L`
#'
#'  * `vec_ptype(slide2_vec(.x, .y, .ptype = ptype)) == ptype`
#'
#' }
#'
#' \subsection{`pslide()`}{
#'
#'  * `vec_size(pslide(.l)) == vec_size_common(!!! .l)`
#'
#'  * `vec_ptype(pslide(.l)) == list()`
#'
#' }
#'
#' \subsection{`pslide_vec()` and `pslide_*()` variants}{
#'
#'  * `vec_size(pslide_vec(.l)) == vec_size_common(!!! .l)`
#'
#'  * `vec_size(pslide_vec(.l)[[1]]) == 1L`
#'
#'  * `vec_ptype(pslide_vec(.l, .ptype = ptype)) == ptype`
#'
#' }
#'
#' @examples
#' # Slide along two inputs at once
#' slide2(1:4, 5:8, ~list(.x, .y), .before = 2)
#'
#' # Or, for more than two, use `pslide()`
#' pslide(list(1:4, 5:8, 9:12), ~list(.x, .y, ..3), .before = 2)
#'
#' # You can even slide along the rows of multiple data frames of
#' # equal size at once
#' set.seed(16)
#' x <- data.frame(a = rnorm(5), b = rnorm(5))
#' y <- data.frame(c = letters[1:5], d = letters[6:10])
#'
#' row_return <- function(x_rows, y_rows) {
#'   if (sum(x_rows$a) < 0) {
#'     x_rows
#'   } else {
#'     y_rows
#'   }
#' }
#'
#' slide2(x, y, row_return, .before = 1, .after = 2)
#'
#' @seealso [slide()], [slide_index2()], [hop_index2()]
#' @export
slide2 <- function(.x,
                   .y,
                   .f,
                   ...,
                   .before = 0L,
                   .after = 0L,
                   .step = 1L,
                   .complete = FALSE) {
  slide2_impl(
    .x,
    .y,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .step = .step,
    .complete = .complete,
    .ptype = list(),
    .constrain = FALSE,
    .atomic = FALSE
  )
}

#' @rdname slide2
#' @export
slide2_vec <- function(.x,
                       .y,
                       .f,
                       ...,
                       .before = 0L,
                       .after = 0L,
                       .step = 1L,
                       .complete = FALSE,
                       .ptype = NULL) {
  out <- slide2_impl(
    .x,
    .y,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .step = .step,
    .complete = .complete,
    .ptype = list(),
    .constrain = FALSE,
    .atomic = TRUE
  )

  vec_simplify(out, .ptype)
}

slide2_vec_direct <- function(.x,
                              .y,
                              .f,
                              ...,
                              .before,
                              .after,
                              .step,
                              .complete,
                              .ptype,
                              .slider_error_call = caller_env()) {
  slide2_impl(
    .x,
    .y,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .step = .step,
    .complete = .complete,
    .ptype = .ptype,
    .constrain = TRUE,
    .atomic = TRUE,
    .slider_error_call = .slider_error_call
  )
}

#' @rdname slide2
#' @export
slide2_dbl <- function(.x,
                       .y,
                       .f,
                       ...,
                       .before = 0L,
                       .after = 0L,
                       .step = 1L,
                       .complete = FALSE) {
  slide2_vec_direct(
    .x,
    .y,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .step = .step,
    .complete = .complete,
    .ptype = double()
  )
}

#' @rdname slide2
#' @export
slide2_int <- function(.x,
                       .y,
                       .f,
                       ...,
                       .before = 0L,
                       .after = 0L,
                       .step = 1L,
                       .complete = FALSE) {
  slide2_vec_direct(
    .x,
    .y,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .step = .step,
    .complete = .complete,
    .ptype = integer()
  )
}

#' @rdname slide2
#' @export
slide2_lgl <- function(.x,
                       .y,
                       .f,
                       ...,
                       .before = 0L,
                       .after = 0L,
                       .step = 1L,
                       .complete = FALSE) {
  slide2_vec_direct(
    .x,
    .y,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .step = .step,
    .complete = .complete,
    .ptype = logical()
  )
}

#' @rdname slide2
#' @export
slide2_chr <- function(.x,
                       .y,
                       .f,
                       ...,
                       .before = 0L,
                       .after = 0L,
                       .step = 1L,
                       .complete = FALSE) {
  slide2_vec_direct(
    .x,
    .y,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .step = .step,
    .complete = .complete,
    .ptype = character()
  )
}

#' @inheritParams vctrs::vec_rbind
#' @rdname slide2
#' @export
slide2_dfr <- function(.x,
                       .y,
                       .f,
                       ...,
                       .before = 0L,
                       .after = 0L,
                       .step = 1L,
                       .complete = FALSE,
                       .names_to = rlang::zap(),
                       .name_repair = c("unique", "universal", "check_unique")) {
  out <- slide2(
    .x,
    .y,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .step = .step,
    .complete = .complete
  )

  vec_rbind(!!!out, .names_to = .names_to, .name_repair = .name_repair)
}

#' @inheritParams vctrs::vec_cbind
#' @rdname slide2
#' @export
slide2_dfc <- function(.x,
                       .y,
                       .f,
                       ...,
                       .before = 0L,
                       .after = 0L,
                       .step = 1L,
                       .complete = FALSE,
                       .size = NULL,
                       .name_repair = c("unique", "universal", "check_unique", "minimal")) {
  out <- slide2(
    .x,
    .y,
    .f,
    ...,
    .before = .before,
    .after = .after,
    .step = .step,
    .complete = .complete
  )

  vec_cbind(!!!out, .size = .size, .name_repair = .name_repair)
}

# ------------------------------------------------------------------------------

slide2_impl <- function(.x,
                        .y,
                        .f,
                        ...,
                        .before,
                        .after,
                        .step,
                        .complete,
                        .ptype,
                        .constrain,
                        .atomic,
                        .slider_error_call = caller_env()) {
  vec_assert(.x, call = .slider_error_call)
  vec_assert(.y, call = .slider_error_call)

  args <- vec_recycle_common(.x = .x, .y = .y, .call = .slider_error_call)

  .f <- as_function(.f, call = .slider_error_call)

  f_call <- expr(.f(.x, .y, ...))

  type <- -2L

  params <- list(
    type,
    .constrain,
    .atomic,
    .before,
    .after,
    .step,
    .complete
  )

  slide_common(
    x = args,
    f_call = f_call,
    ptype = .ptype,
    env = environment(),
    params = params
  )
}

#' @export
tile <- function(.x,
                 .f,
                 ...,
                 .before = 0L,
                 .after = 0L,
                 .offset = NULL,
                 .complete = FALSE,
                 .forward = TRUE) {

  .step <- .before + .after + 1L

  slide(
    .x = .x,
    .f = .f,
    ...,
    .before = .before,
    .after = .after,
    .step = .step,
    .offset = .offset,
    .complete = .complete,
    .forward = .forward
  )
}

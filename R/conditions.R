stop_index_incompatible_size <- function(i_size, size, i_arg = "i") {
  stop_slide(
    i_size = i_size,
    size = size,
    i_arg = i_arg,
    class = "slide_error_index_incompatible_size"
  )
}

#' @export
cnd_header.slide_error_index_incompatible_size <- function(cnd, ...) {
  glue_data(cnd, "`{i_arg}` has an incorrect size.")
}

#' @export
cnd_body.slide_error_index_incompatible_size <- function(cnd, ...) {
  glue_data_bullets(cnd, x = "It must have size {size}, not {i_size}.")
}

# ------------------------------------------------------------------------------

stop_slide <- function(message = NULL, class = character(), ...) {
  abort(message, class = c(class, "slide_error"), ...)
}

# ------------------------------------------------------------------------------

glue_data_bullets <- function (.data, ..., .env = caller_env()) {
  glue_data_env <- function(...) glue_data(.data, ..., .envir = .env)
  format_error_bullets(map_chr(chr(...), glue_data_env))
}

map_chr <- function(x, f) {
  vapply(x, f, character(1))
}

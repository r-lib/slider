#' Index arithmetic
#'
#' @description
#' `slider_plus()` and `slider_minus()` are developer functions used to register
#' special double dispatch methods to control how `.before` and `.after` are
#' subtracted from and added to `.i`. These allow developers to overcome some of
#' the restrictions around `+` and `-` when custom S3 types are involved. These
#' should only be used by package authors creating new index types.
#'
#' * `slider_plus()` allows you to override the default behavior of
#'   `.i + .after`. When writing the S3 method, `x` will be `.i`, and `y` will
#'   be `.after`.
#'
#' * `slider_minus()` allows you to override the default behavior of
#'   `.i - .before`. When writing the S3 method, `x` will be `.i`, and `y` will
#'   be `.before`.
#'
#' These generics are a bit special. They work similarly to
#' [vctrs::vec_ptype2()] in that they are _double dispatch_ methods that
#' dispatch off the types of both `x` and `y`. To write an S3 method for these
#' generics, write and export an S3 method of the form:
#'
#' ```
#' slider_plus.x_class.y_class <- function(x, y) {
#'   # My method
#' }
#' ```
#'
#' Inheritance is not considered in the method lookup, and you cannot use
#' `NextMethod()` from within your method.
#'
#' @keywords internal
#' @name index-arithmetic
#'
#' @param x,y `[vector]`
#'
#'   Two vectors to add or subtract.
#'
#'   `x` will always be the index, `.i`.
#'
#'   For `slider_plus()`, `y` will be `.after`.
#'
#'   For `slider_minus()`, `y` will be `.before`.
#'
#' @returns
#' * For `slider_plus()`, `x` after adding `y`.
#'
#' * For `slider_minus()`, `x` after subtracting `y`.
#'
#' The result should always be the same type and size as `x`.
#'
#' @examples
#' slider_plus(1, 2)
#' slider_minus(1, 2)
NULL

#' @export
#' @rdname index-arithmetic
slider_plus <- function(x, y) {
  return(slider_dispatch("slider_plus", x, y, slider_plus_default))
  UseMethod("slider_plus")
}

#' @export
#' @rdname index-arithmetic
slider_minus <- function(x, y) {
  return(slider_dispatch("slider_minus", x, y, slider_minus_default))
  UseMethod("slider_minus")
}

slider_plus_default <- function(x, y) {
  x + y
}
slider_minus_default <- function(x, y) {
  x - y
}

slider_dispatch <- function(generic, x, y, fn_default) {
  fn <- slider_method_get(generic, x, y)

  if (is.null(fn)) {
    fn_default(x, y)
  } else {
    fn(x, y)
  }
}

slider_class <- function(x) {
  if (is.object(x)) {
    out <- class(x)[[1L]]
  } else {
    # Mainly so `1` returns `"double"` not `"numeric"`
    # for method registration purposes
    out <- typeof(x)
  }

  if (!is_string(out)) {
    abort("Encountered object with corrupt class.", .internal = TRUE)
  }

  out
}

slider_method_get <- function(generic, x, y) {
  x_class <- slider_class(x)
  y_class <- slider_class(y)

  name <- paste0(generic, ".", x_class, ".", y_class)

  s3_method_get(name)
}

s3_method_get <- function(name) {
  # Try global env first in case the user registered a method interactively
  env <- global_env()
  fn <- env_get(env, name, default = NULL)

  if (is_function(fn)) {
    return(fn)
  }

  # Then try the slider S3 methods table
  ns <- ns_env("slider")
  env <- ns_methods_table(ns)
  fn <- env_get(env, name, default = NULL)

  if (is_function(fn)) {
    return(fn)
  }

  # Symbol not bound to the `env`, or it was bound to a non-function
  NULL
}

ns_methods_table <- function (ns) {
  ns$.__S3MethodsTable__.
}

# ------------------------------------------------------------------------------
# "Exported" methods for testing the package registration path

#' @export
slider_plus.slider_test_class.double <- function(x, y) {
  new_slider_test_class(x + (y * 2))
}

#' @export
slider_minus.slider_test_class.double <- function(x, y) {
  new_slider_test_class(x - (y * 2))
}

new_slider_test_class <- function(x) {
  structure(x, class = "slider_test_class")
}

foobar <- function(x = list()) {
  structure(x, class = "slider_foobar")
}

local_methods <- function(..., .frame = caller_env()) {
  local_bindings(..., .env = global_env(), .frame = .frame)
}

local_c_foobar <- function(frame = caller_env()) {
  local_methods(.frame = frame,
    c.slider_foobar = function(...) {
      signal("", class = "slider_c_foobar")
      xs <- list(...)
      xs <- lapply(xs, unclass)
      out <- vec_unchop(xs)
      foobar(out)
    }
  )
}

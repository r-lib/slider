#' @keywords internal
#' @aliases slider-package
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
#' @import rlang
#' @import vctrs
#' @importFrom warp warp_boundary
#' @importFrom warp warp_distance
#' @useDynLib slider, .registration = TRUE
## usethis namespace: end
NULL

# So errors from helpers like
# `check_generated_endpoints_cannot_be_na(NA, ".before")`
# don't show both `!` and `i` for the first bullet.
on_load(local_use_cli())

#include "slider.h"
#include "utils.h"

SEXP slider_iteration_impl() {
  if (slider_private_iteration == NA_INTEGER) {
    Rf_errorcall(
      R_NilValue,
      "`slider_iteration()` can only be called from a `slide_*()` function."
    );
  }

  return Rf_ScalarInteger(slider_private_iteration);
}

SEXP slider_iteration_reset_impl() {
  slider_private_iteration = NA_INTEGER;
  return R_NilValue;
}

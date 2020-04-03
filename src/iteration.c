#include "slider.h"
#include "utils.h"

SEXP slider_iteration_impl() {
  const int iteration = *p_slider_private_iteration;

  if (iteration == NA_INTEGER) {
    Rf_errorcall(
      R_NilValue,
      "`slider_iteration()` can only be called from a `slide_*()` function."
    );
  }

  // Must duplicate to ensure each usage has its own copy
  return Rf_ScalarInteger(iteration);
}

SEXP slider_iteration_reset_impl() {
  *p_slider_private_iteration = NA_INTEGER;
  return R_NilValue;
}

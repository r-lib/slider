#include "iteration.h"
#include "utils.h"

// [[ register() ]]
SEXP slider_iteration_impl() {
  if (slider_private_iteration == NA_INTEGER) {
    Rf_errorcall(
      R_NilValue,
      "`slider_iteration()` can only be called from a `slide_*()` function."
    );
  }

  return Rf_ScalarInteger(slider_private_iteration);
}

void slider_iteration_cleanup(void* p_data) {
  slider_private_iteration = slider_private_old_iteration;
  slider_private_old_iteration = NA_INTEGER;
}

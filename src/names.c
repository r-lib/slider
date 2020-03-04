#include "slider-vctrs.h"

// [[ export() ]]
SEXP slider_vec_set_names(SEXP x, SEXP names) {
  return vec_set_names(x, names);
}

// [[ export() ]]
SEXP slider_vec_names(SEXP x) {
  return vec_names(x);
}

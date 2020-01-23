#include <vctrs.h>

// [[ export() ]]
SEXP slide_vec_set_names(SEXP x, SEXP names) {
  return vec_set_names(x, names);
}

// [[ export() ]]
SEXP slide_vec_names(SEXP x) {
  return vec_names(x);
}

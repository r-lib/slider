#include "slurrr.h"

SEXP strings_empty = NULL;

SEXP syms_dot_x = NULL;
SEXP syms_dot_y = NULL;
SEXP syms_dot_l = NULL;

// -----------------------------------------------------------------------------

int r_int_get(SEXP x, int i) {
  return INTEGER(x)[i];
}

bool r_lgl_get(SEXP x, int i) {
  return LOGICAL(x)[i];
}

// -----------------------------------------------------------------------------

// [[register()]]
void slurrr_init_utils() {
  syms_dot_x = Rf_install(".x");
  syms_dot_y = Rf_install(".y");
  syms_dot_l = Rf_install(".l");

  strings_empty = Rf_allocVector(STRSXP, 1);
  R_PreserveObject(strings_empty);
  SET_STRING_ELT(strings_empty, 0, Rf_mkChar(""));
}

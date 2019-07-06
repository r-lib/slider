#include "slurrr.h"

#define r_int Rf_ScalarInteger

SEXP strings_empty;

SEXP syms_slice;
SEXP syms_dot_f;
SEXP syms_dots;

int r_int_get(SEXP x, int i);
bool r_lgl_get(SEXP x, int i);

SEXP r_seq(R_len_t from, R_len_t to);

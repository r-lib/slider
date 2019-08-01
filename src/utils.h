#include "slurrr.h"

#define r_int Rf_ScalarInteger

static inline int min(int x, int y) {
  return x < y ? x : y;
};

static inline int max(int x, int y) {
  return x > y ? x : y;
};

SEXP strings_empty;

SEXP syms_dot_x;
SEXP syms_dot_y;
SEXP syms_dot_l;

int r_int_get(SEXP x, int i);
bool r_lgl_get(SEXP x, int i);

SEXP r_seq(R_len_t from, R_len_t to);

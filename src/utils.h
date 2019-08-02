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
SEXP syms_x;
SEXP syms_names;

SEXP syms_set_names;
SEXP syms_set_rownames;

SEXP fns_set_rownames;
SEXP fns_set_names;

int r_int_get(SEXP x, int i);
bool r_lgl_get(SEXP x, int i);

SEXP r_maybe_duplicate(SEXP x);

SEXP r_new_environment(SEXP parent, R_len_t size);

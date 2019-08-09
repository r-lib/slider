#include "slurrr.h"

#define r_int Rf_ScalarInteger

static inline int min(int x, int y) {
  return x < y ? x : y;
};

static inline int max(int x, int y) {
  return x > y ? x : y;
};

SEXP strings_empty;
SEXP strings_dot_before;
SEXP strings_dot_after;
SEXP strings_dot_step;
SEXP strings_dot_offset;
SEXP strings_dot_complete;
SEXP strings_dot_forward;

SEXP syms_dot_x;
SEXP syms_dot_y;
SEXP syms_dot_l;
SEXP syms_x;
SEXP syms_names;

SEXP syms_set_names;
SEXP syms_set_rownames;

SEXP fns_set_rownames;
SEXP fns_set_names;

extern SEXP slurrr_shared_empty_lgl;
extern SEXP slurrr_shared_empty_int;

int r_int_get(SEXP x, int i);
bool r_lgl_get(SEXP x, int i);
SEXP r_lst_get(SEXP x, int i);

int r_scalar_int_get(SEXP x);
bool r_scalar_lgl_get(SEXP x);
const char* r_scalar_chr_get(SEXP x);

SEXP r_maybe_duplicate(SEXP x);

SEXP r_new_environment(SEXP parent, R_len_t size);

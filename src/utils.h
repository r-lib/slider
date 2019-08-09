#include "slurrr.h"

#define r_int Rf_ScalarInteger

static inline int min(int x, int y) {
  return x < y ? x : y;
};

static inline int max(int x, int y) {
  return x > y ? x : y;
};

static inline SEXP r_lst_get(SEXP x, int i) {
  return VECTOR_ELT(x, i);
}

static inline int r_scalar_int_get(SEXP x) {
  return INTEGER(x)[0];
}

static inline bool r_scalar_lgl_get(SEXP x) {
  return LOGICAL(x)[0];
}

static inline const char* r_scalar_chr_get(SEXP x) {
  return CHAR(STRING_ELT(x, 0));
}

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
SEXP syms_index;

SEXP syms_set_names;
SEXP syms_set_rownames;

SEXP fns_set_rownames;
SEXP fns_set_names;

extern SEXP slurrr_shared_empty_lgl;
extern SEXP slurrr_shared_empty_int;

SEXP r_maybe_duplicate(SEXP x);

SEXP r_new_environment(SEXP parent, R_len_t size);

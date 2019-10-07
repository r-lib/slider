#ifndef SLIDE_UTILS_H
#define SLIDE_UTILS_H

#include "slide.h"

#define r_int Rf_ScalarInteger

static inline int min(int x, int y) {
  return x < y ? x : y;
}

static inline int max(int x, int y) {
  return x > y ? x : y;
}

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
SEXP strings_dot_complete;

SEXP syms_dot_x;
SEXP syms_dot_y;
SEXP syms_dot_l;

extern SEXP slide_shared_empty_lgl;
extern SEXP slide_shared_empty_int;

extern SEXP slide_ns_env;

void stop_not_all_size_one(int iteration, int size);

int compute_size(SEXP x, int type);
int compute_force(int type);

SEXP copy_names(SEXP out, SEXP x, int type);

SEXP make_slice_container(int type);
void slice_and_update_env(SEXP x, SEXP window, SEXP env, int type, SEXP container);

#endif

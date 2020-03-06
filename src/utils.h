#ifndef SLIDER_UTILS_H
#define SLIDER_UTILS_H

#include "slider.h"

#define PROTECT_N(x, n) (++*n, PROTECT(x))

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

extern SEXP strings_dot_before;
extern SEXP strings_dot_after;
extern SEXP strings_dot_step;
extern SEXP strings_dot_complete;

extern SEXP syms_dot_x;
extern SEXP syms_dot_y;
extern SEXP syms_dot_l;

extern SEXP slider_shared_empty_lgl;
extern SEXP slider_shared_empty_int;

extern SEXP slider_ns_env;

void stop_not_all_size_one(int iteration, int size);

void check_slide_starts_not_past_stops(SEXP starts, SEXP stops);
void check_hop_starts_not_past_stops(SEXP starts, SEXP stops);

int compute_size(SEXP x, int type);
int compute_force(int type);

SEXP copy_names(SEXP out, SEXP x, int type);

SEXP make_slice_container(int type);
void slice_and_update_env(SEXP x, SEXP window, SEXP env, int type, SEXP container);

#endif

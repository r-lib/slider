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

static inline SEXP r_force_eval(SEXP call, SEXP env, const int n_force) {
#if defined(R_VERSION) && R_VERSION >= R_Version(3, 2, 3)
  return R_forceAndCall(call, n_force, env);
#else
  return Rf_eval(call, env);
#endif
}

static inline SEXP r_lst_get(SEXP x, int i) {
  return VECTOR_ELT(x, i);
}

static inline int r_scalar_int_get(SEXP x) {
  return INTEGER(x)[0];
}

static inline int r_scalar_lgl_get(SEXP x) {
  return LOGICAL(x)[0];
}

static inline const char* r_scalar_chr_get(SEXP x) {
  return CHAR(STRING_ELT(x, 0));
}

__attribute__((noreturn)) static inline void never_reached(const char* fn) {
  Rf_errorcall(R_NilValue, "Internal error: Reached the unreachable in `%s()`.", fn);
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

extern SEXP slider_shared_na_lgl;

extern SEXP slider_ns_env;

SEXP slider_init(SEXPTYPE type, R_xlen_t size);

void stop_not_all_size_one(int iteration, int size);

void check_slide_starts_not_past_stops(SEXP starts, SEXP stops);
void check_hop_starts_not_past_stops(SEXP starts, SEXP stops);

int compute_size(SEXP x, int type);
int compute_force(int type);

SEXP slider_names(SEXP x, int type);

SEXP make_slice_container(int type);
void slice_and_update_env(SEXP x, SEXP window, SEXP env, int type, SEXP container);

#endif

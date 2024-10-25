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

static inline R_xlen_t min_size(R_xlen_t x, R_xlen_t y) {
  return x < y ? x : y;
}
static inline R_xlen_t max_size(R_xlen_t x, R_xlen_t y) {
  return x > y ? x : y;
}

static inline uint64_t min_u64(uint64_t x, uint64_t y) {
  return x < y ? x : y;
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

static inline bool p_int_any_gt(const int* p_x, const int* p_y, R_xlen_t size) {
  for (R_xlen_t i = 0; i < size; ++i) {
    if (p_x[i] > p_y[i]) {
      return true;
    }
  }
  return false;
}

__attribute__((noreturn)) static inline void never_reached(const char* fn) {
  Rf_errorcall(R_NilValue, "Internal error: Reached the unreachable in `%s()`.", fn);
}

extern SEXP strings_before;
extern SEXP strings_after;
extern SEXP strings_step;
extern SEXP strings_complete;
extern SEXP strings_na_rm;
extern SEXP strings_dot_before;
extern SEXP strings_dot_after;
extern SEXP strings_dot_step;
extern SEXP strings_dot_complete;
extern SEXP strings_dot_na_rm;

extern SEXP syms_dot_x;
extern SEXP syms_dot_y;
extern SEXP syms_dot_l;

extern SEXP slider_shared_empty_lgl;
extern SEXP slider_shared_empty_int;
extern SEXP slider_shared_empty_dbl;

extern SEXP slider_shared_na_lgl;

extern SEXP slider_ns_env;

SEXP slider_init(SEXPTYPE type, R_xlen_t size);

void list_fill(SEXP x, SEXP value);

void stop_not_all_size_one(int iteration, int size);

void check_slide_starts_not_past_stops(SEXP starts,
                                       SEXP stops,
                                       const int* p_starts,
                                       const int* p_stops,
                                       R_xlen_t size);
void check_hop_starts_not_past_stops(SEXP starts,
                                     SEXP stops,
                                     const int* p_starts,
                                     const int* p_stops,
                                     R_xlen_t size);

int compute_size(SEXP x, int type);
int compute_force(int type);

SEXP slider_names(SEXP x, int type);

SEXP make_slice_container(int type);
void slice_and_update_env(SEXP x, SEXP window, SEXP env, int type, SEXP container);

#endif

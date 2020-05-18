#ifndef SLIDER_ASSIGN_H
#define SLIDER_ASSIGN_H

#include "slider.h"
#include "slider-vctrs.h"

// -----------------------------------------------------------------------------

static inline void assign_one_dbl(double* p_out, R_len_t i, SEXP elt, SEXP ptype) {
  elt = vec_cast(elt, ptype);
  p_out[i] = REAL_RO(elt)[0];
}

static inline void assign_one_int(int* p_out, R_len_t i, SEXP elt, SEXP ptype) {
  elt = vec_cast(elt, ptype);
  p_out[i] = INTEGER_RO(elt)[0];
}

static inline void assign_one_lgl(int* p_out, R_len_t i, SEXP elt, SEXP ptype) {
  elt = vec_cast(elt, ptype);
  p_out[i] = LOGICAL_RO(elt)[0];
}

static inline void assign_one_chr(SEXP* p_out, R_len_t i, SEXP elt, SEXP ptype) {
  elt = vec_cast(elt, ptype);
  p_out[i] = STRING_PTR_RO(elt)[0];
}

static inline void assign_one_lst(SEXP out, R_len_t i, SEXP elt, SEXP ptype) {
  SET_VECTOR_ELT(out, i, elt);
}

// -----------------------------------------------------------------------------

#endif

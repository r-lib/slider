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

#define ASSIGN_LOCS(CTYPE, CONST_DEREF) do {                   \
  const R_len_t size = Rf_length(locations);                   \
  const int* p_locations = INTEGER_RO(locations);              \
                                                               \
  elt = PROTECT(vec_cast(elt, ptype));                         \
  const CTYPE value = CONST_DEREF(elt)[0];                     \
                                                               \
  for (R_len_t i = 0; i < size; ++i) {                         \
    /* `locations` are 1-based */                              \
    R_len_t loc = p_locations[i] - 1;                          \
    p_out[loc] = value;                                        \
  }                                                            \
                                                               \
  UNPROTECT(1);                                                \
} while (0)

static inline void assign_locs_dbl(double* p_out, SEXP locations, SEXP elt, SEXP ptype) {
  ASSIGN_LOCS(double, REAL_RO);
}
static inline void assign_locs_int(int* p_out, SEXP locations, SEXP elt, SEXP ptype) {
  ASSIGN_LOCS(int, INTEGER_RO);
}
static inline void assign_locs_lgl(int* p_out, SEXP locations, SEXP elt, SEXP ptype) {
  ASSIGN_LOCS(int, LOGICAL_RO);
}
static inline void assign_locs_chr(SEXP* p_out, SEXP locations, SEXP elt, SEXP ptype) {
  ASSIGN_LOCS(SEXP, STRING_PTR_RO);
}

#undef ASSIGN_LOCS

static inline void assign_locs_lst(SEXP out, SEXP locations, SEXP elt, SEXP ptype) {
  const R_len_t size = Rf_length(locations);
  const int* p_locations = INTEGER_RO(locations);

  for (R_len_t i = 0; i < size; ++i) {
    R_len_t loc = p_locations[i] - 1;
    SET_VECTOR_ELT(out, loc, elt);
  }
}

// -----------------------------------------------------------------------------

#endif

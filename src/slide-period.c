#include "slider.h"

// -----------------------------------------------------------------------------

static SEXP compute_from(SEXP starts, double first, R_xlen_t n, bool before_unbounded);

// [[ export() ]]
SEXP slider_compute_from(SEXP starts, SEXP first, SEXP n, SEXP before_unbounded) {
  double first_ = REAL(first)[0];

  // Support long vectors
  R_xlen_t n_;

  switch(TYPEOF(n)) {
  case REALSXP: n_ = REAL(n)[0]; break;
  case INTSXP: n_ = (double) INTEGER(n)[0]; break;
  default: Rf_errorcall(R_NilValue, "Internal error: `n` should be integer or double.");
  }

  bool before_unbounded_ = LOGICAL(before_unbounded)[0];

  return compute_from(starts, first_, n_, before_unbounded_);
}

static SEXP compute_from(SEXP starts, double first, R_xlen_t n, bool before_unbounded) {
  double* p_starts = REAL(starts);

  R_xlen_t from = 1;

  if (before_unbounded) {
    return(Rf_ScalarReal(from));
  }

  for (R_xlen_t i = 0; i < n; ++i) {
    if (first > p_starts[i]) {
      ++from;
    } else {
      break;
    }
  }

  return Rf_ScalarReal(from);
}

// -----------------------------------------------------------------------------

static SEXP compute_to(SEXP stops, double last, R_xlen_t n, bool after_unbounded);

// [[ export() ]]
SEXP slider_compute_to(SEXP stops, SEXP last, SEXP n, SEXP after_unbounded) {
  double last_ = REAL(last)[0];

  // Support long vectors
  R_xlen_t n_;

  switch(TYPEOF(n)) {
  case REALSXP: n_ = REAL(n)[0]; break;
  case INTSXP: n_ = (double) INTEGER(n)[0]; break;
  default: Rf_errorcall(R_NilValue, "Internal error: `n` should be integer or double.");
  }

  bool after_unbounded_ = LOGICAL(after_unbounded)[0];

  return compute_to(stops, last_, n_, after_unbounded_);
}

static SEXP compute_to(SEXP stops, double last, R_xlen_t n, bool after_unbounded) {
  double* p_stops = REAL(stops);

  R_xlen_t to = n;

  if (after_unbounded) {
    return(Rf_ScalarReal(to));
  }

  for (R_xlen_t i = n - 1; i >= 0; --i) {
    if (last < p_stops[i]) {
      --to;
    } else {
      break;
    }
  }

  return Rf_ScalarReal(to);
}

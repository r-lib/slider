#include "slider.h"
#include "slider-vctrs.h"

// [[ export() ]]
SEXP slider_block(SEXP x, SEXP starts, SEXP stops) {
  R_xlen_t size = Rf_xlength(starts);

  double* p_starts = REAL(starts);
  double* p_stops = REAL(stops);

  SEXP indices = PROTECT(Rf_allocVector(VECSXP, size));

  for (R_xlen_t i = 0; i < size; ++i) {
    int start = p_starts[i];
    int stop = p_stops[i];
    int size = stop - start + 1;

    SEXP seq = compact_seq(start - 1, size, true);
    SET_VECTOR_ELT(indices, i, seq);
  }

  SEXP out = PROTECT(vec_chop(x, indices));

  UNPROTECT(2);
  return out;
}

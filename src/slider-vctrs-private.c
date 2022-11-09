#include "slider-vctrs-private.h"

// Experimental non-public vctrs functions
SEXP (*vec_cast)(SEXP, SEXP) = NULL;
SEXP (*vec_chop)(SEXP, SEXP) = NULL;
SEXP (*vec_slice_impl)(SEXP, SEXP) = NULL;
SEXP (*vec_names)(SEXP) = NULL;
SEXP (*compact_seq)(R_len_t, R_len_t, bool) = NULL;
SEXP (*init_compact_seq)(int*, R_len_t, R_len_t, bool) = NULL;

void slider_initialize_vctrs_private(void) {
  // Experimental non-public vctrs functions
  vec_cast = (SEXP (*)(SEXP, SEXP)) R_GetCCallable("vctrs", "exp_vec_cast");
  vec_chop = (SEXP (*)(SEXP, SEXP)) R_GetCCallable("vctrs", "exp_vec_chop");
  vec_slice_impl = (SEXP (*)(SEXP, SEXP)) R_GetCCallable("vctrs", "exp_vec_slice_impl");
  vec_names = (SEXP (*)(SEXP)) R_GetCCallable("vctrs", "exp_vec_names");
  compact_seq = (SEXP (*)(R_len_t, R_len_t, bool)) R_GetCCallable("vctrs", "exp_short_compact_seq");
  init_compact_seq = (SEXP (*)(int*, R_len_t, R_len_t, bool)) R_GetCCallable("vctrs", "exp_short_init_compact_seq");
}

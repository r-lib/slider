#include "slider-vctrs-private.h"

// Experimental non-public vctrs functions
SEXP (*vec_cast)(SEXP, SEXP) = NULL;
SEXP (*vec_restore)(SEXP, SEXP) = NULL;
SEXP (*vec_proxy)(SEXP) = NULL;
SEXP (*vec_chop)(SEXP, SEXP) = NULL;
SEXP (*vec_proxy_assign)(SEXP, SEXP, SEXP) = NULL;
SEXP (*vec_slice_impl)(SEXP, SEXP) = NULL;
SEXP (*vec_names)(SEXP) = NULL;
SEXP (*vec_set_names)(SEXP, SEXP) = NULL;
SEXP (*vec_init)(SEXP, R_len_t) = NULL;
SEXP (*compact_seq)(R_len_t, R_len_t, bool) = NULL;
SEXP (*init_compact_seq)(int*, R_len_t, R_len_t, bool) = NULL;

void slider_init_vctrs_private() {
  // Experimental non-public vctrs functions
  vec_cast = (SEXP (*)(SEXP, SEXP)) R_GetCCallable("vctrs", "exp_vec_cast");
  vec_restore = (SEXP (*)(SEXP, SEXP)) R_GetCCallable("vctrs", "exp_vec_restore");
  vec_proxy = (SEXP (*)(SEXP)) R_GetCCallable("vctrs", "exp_vec_proxy");
  vec_chop = (SEXP (*)(SEXP, SEXP)) R_GetCCallable("vctrs", "exp_vec_chop");
  vec_proxy_assign = (SEXP (*)(SEXP, SEXP, SEXP)) R_GetCCallable("vctrs", "exp_vec_proxy_assign");
  vec_slice_impl = (SEXP (*)(SEXP, SEXP)) R_GetCCallable("vctrs", "exp_vec_slice_impl");
  vec_names = (SEXP (*)(SEXP)) R_GetCCallable("vctrs", "exp_vec_names");
  vec_set_names = (SEXP (*)(SEXP, SEXP)) R_GetCCallable("vctrs", "exp_vec_set_names");
  vec_init = (SEXP (*)(SEXP, R_len_t)) R_GetCCallable("vctrs", "exp_short_vec_init");
  compact_seq = (SEXP (*)(R_len_t, R_len_t, bool)) R_GetCCallable("vctrs", "exp_short_compact_seq");
  init_compact_seq = (SEXP (*)(int*, R_len_t, R_len_t, bool)) R_GetCCallable("vctrs", "exp_short_init_compact_seq");
}

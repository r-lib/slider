#include <vctrs.c>
#include "slider-vctrs.h"

// Declare the experimental exported but non-exposed vctrs API
SEXP (*vec_cast)(SEXP, SEXP) = NULL;
R_len_t (*vec_size)(SEXP) = NULL;
SEXP (*vec_recycle)(SEXP, R_len_t) = NULL;
SEXP (*vec_init)(SEXP, R_len_t) = NULL;
SEXP (*compact_seq)(R_len_t, R_len_t, bool) = NULL;
SEXP (*init_compact_seq)(int*, R_len_t, R_len_t, bool) = NULL;

void slider_init_vctrs() {
  vctrs_init_api();

  // Initialize the experimental exported but non-exposed vctrs API
  vec_cast = (SEXP (*)(SEXP, SEXP)) R_GetCCallable("vctrs", "exp_vec_cast");
  vec_size = (R_len_t (*)(SEXP)) R_GetCCallable("vctrs", "exp_short_vec_size");
  vec_recycle = (SEXP (*)(SEXP, R_len_t)) R_GetCCallable("vctrs", "exp_short_vec_recycle");
  vec_init = (SEXP (*)(SEXP, R_len_t)) R_GetCCallable("vctrs", "exp_short_vec_init");
  compact_seq = (SEXP (*)(R_len_t, R_len_t, bool)) R_GetCCallable("vctrs", "exp_short_compact_seq");
  init_compact_seq = (SEXP (*)(int*, R_len_t, R_len_t, bool)) R_GetCCallable("vctrs", "exp_short_init_compact_seq");
}

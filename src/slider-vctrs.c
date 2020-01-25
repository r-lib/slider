#include <vctrs.c>
#include "slider-vctrs.h"

SEXP (*vctrs_cast)(SEXP, SEXP, SEXP, SEXP) = NULL;
SEXP (*compact_seq)(R_len_t, R_len_t, bool) = NULL;
SEXP (*init_compact_seq)(int*, R_len_t, R_len_t, bool) = NULL;

// `short_*()` callables
SEXP (*vec_init)(SEXP, R_len_t) = NULL;
R_len_t (*vec_size)(SEXP) = NULL;
SEXP (*vec_recycle)(SEXP, R_len_t) = NULL;

void slider_init_vctrs() {
  vctrs_init_api();

  // Initialize the experimental exported but non-exposed vctrs API
  vctrs_cast = (SEXP (*)(SEXP, SEXP, SEXP, SEXP)) R_GetCCallable("vctrs", "vctrs_cast");
  compact_seq = (SEXP (*)(R_len_t, R_len_t, bool)) R_GetCCallable("vctrs", "compact_seq");
  init_compact_seq = (SEXP (*)(int*, R_len_t, R_len_t, bool)) R_GetCCallable("vctrs", "init_compact_seq");

  // `short_*()` callables
  vec_init = (SEXP (*)(SEXP, R_len_t)) R_GetCCallable("vctrs", "short_vec_init");
  vec_size = (R_len_t (*)(SEXP)) R_GetCCallable("vctrs", "short_vec_size");
  vec_recycle = (SEXP (*)(SEXP, R_len_t)) R_GetCCallable("vctrs", "short_vec_recycle");
}

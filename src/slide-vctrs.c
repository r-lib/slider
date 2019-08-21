#include <vctrs.c>
#include "slide-vctrs.h"

SEXP (*vctrs_cast)(SEXP, SEXP, SEXP, SEXP) = NULL;
SEXP (*compact_seq)(R_len_t, R_len_t, bool) = NULL;
SEXP (*init_compact_seq)(int*, R_len_t, R_len_t, bool) = NULL;

void slide_init_vctrs() {
  vctrs_init_api();

  // Initialize the experimental exported but non-exposed vctrs API
  vctrs_cast = (SEXP (*)(SEXP, SEXP, SEXP, SEXP)) R_GetCCallable("vctrs", "vctrs_cast");
  compact_seq = (SEXP (*)(R_len_t, R_len_t, bool)) R_GetCCallable("vctrs", "compact_seq");
  init_compact_seq = (SEXP (*)(int*, R_len_t, R_len_t, bool)) R_GetCCallable("vctrs", "init_compact_seq");
}

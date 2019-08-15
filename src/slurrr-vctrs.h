#ifndef SLURRR_VCTRS_H
#define SLURRR_VCTRS_H

#include "slurrr.h"

// Experimental exported but non-exposed vctrs API
SEXP (*vctrs_cast)(SEXP, SEXP, SEXP, SEXP);
SEXP (*compact_seq)(R_len_t, R_len_t, bool);
SEXP (*init_compact_seq)(int*, R_len_t, R_len_t, bool);

#endif

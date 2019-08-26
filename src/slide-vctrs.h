#ifndef SLIDE_VCTRS_H
#define SLIDE_VCTRS_H

#include "slide.h"

// Experimental exported but non-exposed vctrs API
SEXP (*vctrs_cast)(SEXP, SEXP, SEXP, SEXP);
SEXP (*compact_seq)(R_len_t, R_len_t, bool);
SEXP (*init_compact_seq)(int*, R_len_t, R_len_t, bool);
R_len_t (*vec_size)(SEXP);

#endif

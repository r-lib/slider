#ifndef SLIDER_VCTRS_H
#define SLIDER_VCTRS_H

#include "slider.h"

// Experimental exported but non-exposed vctrs API
SEXP (*vctrs_cast)(SEXP, SEXP, SEXP, SEXP);
SEXP (*compact_seq)(R_len_t, R_len_t, bool);
SEXP (*init_compact_seq)(int*, R_len_t, R_len_t, bool);

// `short_*()` callables
SEXP (*vec_init)(SEXP, R_len_t);
R_len_t (*vec_size)(SEXP);
SEXP (*vec_recycle)(SEXP, R_len_t);

#endif

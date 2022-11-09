#ifndef SLIDER_VCTRS_PRIVATE_H
#define SLIDER_VCTRS_PRIVATE_H

#include "slider.h"

// Experimental non-public vctrs functions
extern SEXP (*vec_cast)(SEXP, SEXP);
extern SEXP (*vec_chop)(SEXP, SEXP);
extern SEXP (*vec_slice_impl)(SEXP, SEXP);
extern SEXP (*vec_names)(SEXP);
extern SEXP (*compact_seq)(R_len_t, R_len_t, bool);
extern SEXP (*init_compact_seq)(int*, R_len_t, R_len_t, bool);

#endif

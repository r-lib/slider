#ifndef SLURRR_VCTRS_H
#define SLURRR_VCTRS_H

#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <stdbool.h>

SEXP vec_slice(SEXP x, SEXP index);
SEXP vec_init(SEXP x, R_len_t n);
SEXP vec_assign_impl(SEXP x, SEXP i, SEXP value, bool clone);

#endif

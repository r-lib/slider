#ifndef SLIDE_COMPARE_H
#define SLIDE_COMPARE_H

#include "slide.h"

typedef bool (*slide_compare_fn_t)(SEXP, R_len_t, SEXP, R_len_t);

slide_compare_fn_t get_compare_fn_lt(SEXP x);
slide_compare_fn_t get_compare_fn_gt(SEXP x);
slide_compare_fn_t get_compare_fn_lte(SEXP x);

bool vec_any_gt(SEXP x, SEXP y);

#endif

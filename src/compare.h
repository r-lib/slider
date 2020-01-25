#ifndef SLIDER_COMPARE_H
#define SLIDER_COMPARE_H

#include "slider.h"

typedef bool (*slider_compare_fn_t)(SEXP, R_len_t, SEXP, R_len_t);

slider_compare_fn_t get_compare_fn_lt(SEXP x);
slider_compare_fn_t get_compare_fn_gt(SEXP x);
slider_compare_fn_t get_compare_fn_lte(SEXP x);

bool vec_any_gt(SEXP x, SEXP y);

#endif

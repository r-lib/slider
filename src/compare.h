#ifndef SLIDE_COMPARE_H
#define SLIDE_COMPARE_H

#include "slide.h"

bool compare_gt(SEXP x, R_len_t i, SEXP y, R_len_t j);
bool compare_lt(SEXP x, R_len_t i, SEXP y, R_len_t j);
bool compare_lte(SEXP x, R_len_t i, SEXP y, R_len_t j);

bool vec_any_gt(SEXP x, SEXP y);

#endif

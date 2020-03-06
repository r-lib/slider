#ifndef SLIDER_VCTRS_PUBLIC_H
#define SLIDER_VCTRS_PUBLIC_H

#include "slider.h"
#include <vctrs.h>

static inline R_len_t vec_size(SEXP x) {
  return short_vec_size(x);
}

static inline SEXP vec_recycle(SEXP x, R_len_t size) {
  return short_vec_recycle(x, size);
}

#endif

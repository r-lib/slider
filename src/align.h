#ifndef SLIDER_ALIGN_H
#define SLIDER_ALIGN_H

/*
 * Following guidance of:
 * https://stackoverflow.com/questions/227897/how-to-allocate-aligned-memory-only-using-the-standard-library
 *
 * 1) Allocate enough space to shift the pointer
 * 2) Add to the pointer (p_x + buffer)
 * 3) Round down to the closest boundary using `& mask`
 */

#include "slider.h"
#include <stdint.h> // uintptr_t

static
inline
SEXP
aligned_allocate(R_xlen_t n_elements,
                 size_t element_size,
                 size_t element_align) {
  const size_t buffer = element_align - 1;
  const R_xlen_t size = n_elements * element_size + buffer;
  return Rf_allocVector(RAWSXP, size);
}

static
inline
void*
aligned_void_deref(SEXP x, size_t element_align) {
  const size_t buffer = element_align - 1;
  uintptr_t mask = ~ (uintptr_t)buffer;
  uintptr_t p_x = (uintptr_t)RAW(x);
  uintptr_t p_aligned = (p_x + buffer) & mask;
  return (void*) p_aligned;
}

#endif

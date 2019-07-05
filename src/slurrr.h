#ifndef SLURRR_H
#define SLURRR_H

#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

SEXP slurrr_slide(SEXP x, SEXP index);

#endif

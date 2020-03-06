#ifndef SLIDER_H
#define SLIDER_H

#define R_NO_REMAP
#include <R.h>
#include <Rversion.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <stdbool.h>

// Definitions --------------------------------------------------

#define PSLIDE_EMPTY 0
#define SLIDE -1
#define SLIDE2 -2

// Compatibility ------------------------------------------------

#if (R_VERSION < R_Version(3, 5, 0))
  #define LOGICAL_RO(x) ((const int*) LOGICAL(x))
  #define INTEGER_RO(x) ((const int*) INTEGER(x))
  #define REAL_RO(x) ((const double*) REAL(x))
  #define STRING_PTR_RO(x) ((const SEXP*) STRING_PTR(x))
#endif

#endif

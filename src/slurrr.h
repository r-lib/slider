#ifndef SLURRR_H
#define SLURRR_H

#define R_NO_REMAP
#include <R.h>
#include <Rversion.h>
#include <Rinternals.h>
#include <stdbool.h>

// -----------------------------------------------------------------------------
// Parameters

struct slide_params {
  int type;
  int size;
  bool constrain;
  int before;
  int after;
  int step;
  int offset;
  bool complete;
  bool forward;
  bool before_unbounded;
  bool after_unbounded;
};

// Defined in param.c
struct slide_params init_params(SEXP x);

#endif

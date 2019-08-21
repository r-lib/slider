#ifndef SLIDE_H
#define SLIDE_H

#define R_NO_REMAP
#include <R.h>
#include <Rversion.h>
#include <Rinternals.h>
#include <stdbool.h>

#define PSLIDE_EMPTY 0
#define SLIDE -1
#define SLIDE2 -2

// -----------------------------------------------------------------------------
// Parameters

struct slide_params {
  int type;
  bool constrain;
  int before;
  int after;
  int step;
  int offset;
  bool complete;
  bool forward;
  bool before_unbounded;
  bool after_unbounded;
  int size;
};

// Defined in param.c
struct slide_params init_params(SEXP x, SEXP param_list);

#endif

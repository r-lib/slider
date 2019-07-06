#ifndef SLURRR_H
#define SLURRR_H

#define R_NO_REMAP
#include <R.h>
#include <Rversion.h>
#include <Rinternals.h>
#include <stdbool.h>

SEXP slurrr_slide(SEXP env,
                  SEXP x,
                  SEXP before,
                  SEXP after,
                  SEXP step,
                  SEXP offset,
                  SEXP partial,
                  SEXP forward,
                  SEXP ptype,
                  SEXP before_unbounded,
                  SEXP after_unbounded);

#endif

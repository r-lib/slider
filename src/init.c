#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>
#include <vctrs.h>

/* .Call calls */
extern SEXP slurrr_slide(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

// Defined below
SEXP slurrr_init();

static const R_CallMethodDef CallEntries[] = {
  {"slurrr_slide",     (DL_FUNC) &slurrr_slide, 15},
  {"slurrr_init",      (DL_FUNC) &slurrr_init, 0},
  {NULL, NULL, 0}
};

void R_init_slurrr(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}

// utils.c
void slurrr_init_utils();

SEXP slurrr_init() {
  slurrr_init_utils();
  return R_NilValue;
}

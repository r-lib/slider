#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP slurrr_slide(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

// Defined below
SEXP slurrr_init(SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"slurrr_slide", (DL_FUNC) &slurrr_slide, 11},
  {"slurrr_init", (DL_FUNC) &slurrr_init, 1},
  {NULL, NULL, 0}
};

void R_init_slurrr(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}

// utils.c
void slurrr_init_utils(SEXP ns);

SEXP slurrr_init(SEXP ns) {
  slurrr_init_utils(ns);
  return R_NilValue;
}

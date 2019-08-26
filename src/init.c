#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP slide_common_impl(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP slide_between_common_impl(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP slide_index_common_impl(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

// Defined below
SEXP slide_init(SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"slide_common_impl",         (DL_FUNC) &slide_common_impl, 5},
  {"slide_between_common_impl", (DL_FUNC) &slide_between_common_impl, 9},
  {"slide_index_common_impl",   (DL_FUNC) &slide_index_common_impl, 9},
  {"slide_init",                (DL_FUNC) &slide_init, 1},
  {NULL, NULL, 0}
};

void R_init_slide(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}

// slide-vctrs.c
void slide_init_vctrs();

// utils.c
void slide_init_utils(SEXP);

SEXP slide_init(SEXP ns) {
  slide_init_vctrs();
  slide_init_utils(ns);
  return R_NilValue;
}

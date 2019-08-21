#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP slide_core_impl(SEXP, SEXP, SEXP, SEXP, SEXP);

// Defined below
SEXP slide_init();

static const R_CallMethodDef CallEntries[] = {
  {"slide_core_impl",     (DL_FUNC) &slide_core_impl, 5},
  {"slide_init",          (DL_FUNC) &slide_init, 0},
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
void slide_init_utils();

SEXP slide_init() {
  slide_init_vctrs();
  slide_init_utils();
  return R_NilValue;
}

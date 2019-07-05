#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP slurrr_slide(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"slurrr_slide", (DL_FUNC) &slurrr_slide, 2},
  {NULL, NULL, 0}
};

void R_init_slurrr(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}

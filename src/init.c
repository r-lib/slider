#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP slide_common_impl(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP hop_common_impl(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP slide_index_common_impl(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP hop_index_common_impl(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP slider_block(SEXP, SEXP, SEXP);
extern SEXP slider_compute_from(SEXP, SEXP, SEXP, SEXP);
extern SEXP slider_compute_to(SEXP, SEXP, SEXP, SEXP);
extern SEXP slider_vec_set_names(SEXP, SEXP);
extern SEXP slider_vec_names(SEXP);

// Defined below
SEXP slider_initialize(SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"slide_common_impl",         (DL_FUNC) &slide_common_impl, 5},
  {"hop_common_impl",           (DL_FUNC) &hop_common_impl, 7},
  {"slide_index_common_impl",   (DL_FUNC) &slide_index_common_impl, 13},
  {"hop_index_common_impl",     (DL_FUNC) &hop_index_common_impl, 12},
  {"slider_block",              (DL_FUNC) &slider_block, 3},
  {"slider_compute_from",       (DL_FUNC) &slider_compute_from, 4},
  {"slider_compute_to",         (DL_FUNC) &slider_compute_to, 4},
  {"slider_vec_set_names",      (DL_FUNC) &slider_vec_set_names, 2},
  {"slider_vec_names",          (DL_FUNC) &slider_vec_names, 1},
  {"slider_initialize",         (DL_FUNC) &slider_initialize, 1},
  {NULL, NULL, 0}
};

void R_init_slider(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}

// slider-vctrs-private.c
void slider_initialize_vctrs_private();

// slider-vctrs-public.c
void slider_initialize_vctrs_public();

// utils.c
void slider_initialize_utils(SEXP);

SEXP slider_initialize(SEXP ns) {
  slider_initialize_vctrs_private();
  slider_initialize_vctrs_public();
  slider_initialize_utils(ns);
  return R_NilValue;
}

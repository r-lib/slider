#include "slide.h"

SEXP strings_empty = NULL;
SEXP strings_dot_before = NULL;
SEXP strings_dot_after = NULL;
SEXP strings_dot_step = NULL;
SEXP strings_dot_offset = NULL;
SEXP strings_dot_complete = NULL;
SEXP strings_dot_forward = NULL;

SEXP syms_dot_x = NULL;
SEXP syms_dot_y = NULL;
SEXP syms_dot_l = NULL;
SEXP syms_index = NULL;

SEXP slide_shared_empty_lgl = NULL;
SEXP slide_shared_empty_int = NULL;

// -----------------------------------------------------------------------------

// [[register()]]
void slide_init_utils() {
  syms_dot_x = Rf_install(".x");
  syms_dot_y = Rf_install(".y");
  syms_dot_l = Rf_install(".l");
  syms_index = Rf_install("index");

  strings_empty = Rf_allocVector(STRSXP, 1);
  R_PreserveObject(strings_empty);
  SET_STRING_ELT(strings_empty, 0, Rf_mkChar(""));

  strings_dot_before = Rf_allocVector(STRSXP, 1);
  R_PreserveObject(strings_dot_before);
  SET_STRING_ELT(strings_dot_before, 0, Rf_mkChar(".before"));

  strings_dot_after = Rf_allocVector(STRSXP, 1);
  R_PreserveObject(strings_dot_after);
  SET_STRING_ELT(strings_dot_after, 0, Rf_mkChar(".after"));

  strings_dot_step = Rf_allocVector(STRSXP, 1);
  R_PreserveObject(strings_dot_step);
  SET_STRING_ELT(strings_dot_step, 0, Rf_mkChar(".step"));

  strings_dot_offset = Rf_allocVector(STRSXP, 1);
  R_PreserveObject(strings_dot_offset);
  SET_STRING_ELT(strings_dot_offset, 0, Rf_mkChar(".offset"));

  strings_dot_complete = Rf_allocVector(STRSXP, 1);
  R_PreserveObject(strings_dot_complete);
  SET_STRING_ELT(strings_dot_complete, 0, Rf_mkChar(".complete"));

  strings_dot_forward = Rf_allocVector(STRSXP, 1);
  R_PreserveObject(strings_dot_forward);
  SET_STRING_ELT(strings_dot_forward, 0, Rf_mkChar(".forward"));

  slide_shared_empty_lgl = Rf_allocVector(LGLSXP, 0);
  R_PreserveObject(slide_shared_empty_lgl);
  MARK_NOT_MUTABLE(slide_shared_empty_lgl);

  slide_shared_empty_int = Rf_allocVector(INTSXP, 0);
  R_PreserveObject(slide_shared_empty_int);
  MARK_NOT_MUTABLE(slide_shared_empty_int);
}

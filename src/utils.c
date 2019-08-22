#include "slide.h"
#include <vctrs.h>

SEXP strings_empty = NULL;
SEXP strings_dot_before = NULL;
SEXP strings_dot_after = NULL;
SEXP strings_dot_step = NULL;
SEXP strings_dot_complete = NULL;

SEXP syms_dot_x = NULL;
SEXP syms_dot_y = NULL;
SEXP syms_dot_l = NULL;

SEXP slide_shared_empty_lgl = NULL;
SEXP slide_shared_empty_int = NULL;

SEXP slide_ns_env = NULL;

// -----------------------------------------------------------------------------

void stop_not_all_size_one(int iteration, int size) {
  SEXP call = PROTECT(
    Rf_lang3(
      Rf_install("stop_not_all_size_one"),
      PROTECT(Rf_ScalarInteger(iteration)),
      PROTECT(Rf_ScalarInteger(size))
    )
  );

  Rf_eval(call, slide_ns_env);
  Rf_error("Internal error: `stop_not_all_size_one()` should have jumped earlier");
}

// -----------------------------------------------------------------------------

SEXP copy_names(SEXP out, SEXP x, int type) {
  SEXP names;
  if (type == SLIDE) {
    names = PROTECT(vec_names(x));
  } else {
    names = PROTECT(vec_names(VECTOR_ELT(x, 0)));
  }

  UNPROTECT(1);
  return vec_set_names(out, names);
}

// -----------------------------------------------------------------------------

// update_slices() works by repeatedly overwriting the `slices` SEXP with the
// slices from `x`. If we are calling slide() or slide2(), it just overwrites
// `slices` directly and immediately assigns the result into an environment.
// If we are calling pslide(), then `slices` is a list and each element of the
// list is overwritten with the current slice of the i-th pslide element.
// Then that entire list is defined in the environment.

SEXP make_slice_container(int type) {
  if (type == SLIDE || type == SLIDE2) {
    return R_NilValue;
  }

  return Rf_allocVector(VECSXP, type);
}

void slice_and_update_env(SEXP x, SEXP window, SEXP env, int type, SEXP container) {
  // slide()
  if (type == SLIDE) {
    container = vec_slice_impl(x, window);
    Rf_defineVar(syms_dot_x, container, env);
    return;
  }

  // slide2()
  if (type == SLIDE2) {
    container = vec_slice_impl(VECTOR_ELT(x, 0), window);
    Rf_defineVar(syms_dot_x, container, env);
    container = vec_slice_impl(VECTOR_ELT(x, 1), window);
    Rf_defineVar(syms_dot_y, container, env);
    return;
  }

  SEXP slice;

  // pslide()
  for (int i = 0; i < type; ++i) {
    slice = vec_slice_impl(VECTOR_ELT(x, i), window);
    SET_VECTOR_ELT(container, i, slice);
  }

  Rf_defineVar(syms_dot_l, container, env);
}

// -----------------------------------------------------------------------------

// [[register()]]
void slide_init_utils(SEXP ns) {
  slide_ns_env = ns;

  syms_dot_x = Rf_install(".x");
  syms_dot_y = Rf_install(".y");
  syms_dot_l = Rf_install(".l");

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

  strings_dot_complete = Rf_allocVector(STRSXP, 1);
  R_PreserveObject(strings_dot_complete);
  SET_STRING_ELT(strings_dot_complete, 0, Rf_mkChar(".complete"));

  slide_shared_empty_lgl = Rf_allocVector(LGLSXP, 0);
  R_PreserveObject(slide_shared_empty_lgl);
  MARK_NOT_MUTABLE(slide_shared_empty_lgl);

  slide_shared_empty_int = Rf_allocVector(INTSXP, 0);
  R_PreserveObject(slide_shared_empty_int);
  MARK_NOT_MUTABLE(slide_shared_empty_int);
}

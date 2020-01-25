#include "slider.h"
#include "utils.h"
#include "compare.h"
#include "slider-vctrs.h"
#include <vctrs.h>

SEXP strings_empty = NULL;
SEXP strings_dot_before = NULL;
SEXP strings_dot_after = NULL;
SEXP strings_dot_step = NULL;
SEXP strings_dot_complete = NULL;

SEXP syms_dot_x = NULL;
SEXP syms_dot_y = NULL;
SEXP syms_dot_l = NULL;

SEXP slider_shared_empty_lgl = NULL;
SEXP slider_shared_empty_int = NULL;

SEXP slider_ns_env = NULL;

// -----------------------------------------------------------------------------

void stop_not_all_size_one(int iteration, int size) {
  SEXP call = PROTECT(
    Rf_lang3(
      Rf_install("stop_not_all_size_one"),
      PROTECT(Rf_ScalarInteger(iteration)),
      PROTECT(Rf_ScalarInteger(size))
    )
  );

  Rf_eval(call, slider_ns_env);
  Rf_error("Internal error: `stop_not_all_size_one()` should have jumped earlier");
}

// -----------------------------------------------------------------------------

static void stop_slide_start_past_stop(SEXP starts, SEXP stops) {
  SEXP call = PROTECT(
    Rf_lang3(
      Rf_install("stop_slide_start_past_stop"),
      starts,
      stops
    )
  );

  Rf_eval(call, slider_ns_env);
  Rf_error("Internal error: `stop_slide_start_past_stop()` should have jumped earlier");
}

static void stop_hop_start_past_stop(SEXP starts, SEXP stops) {
  SEXP call = PROTECT(
    Rf_lang3(
      Rf_install("stop_hop_start_past_stop"),
      starts,
      stops
    )
  );

  Rf_eval(call, slider_ns_env);
  Rf_error("Internal error: `stop_hop_start_past_stop()` should have jumped earlier");
}

void check_slide_starts_not_past_stops(SEXP starts, SEXP stops) {
  if (vec_any_gt(starts, stops)) {
    stop_slide_start_past_stop(starts, stops);
  }
}

void check_hop_starts_not_past_stops(SEXP starts, SEXP stops) {
  if (vec_any_gt(starts, stops)) {
    stop_hop_start_past_stop(starts, stops);
  }
}

// -----------------------------------------------------------------------------

int compute_size(SEXP x, int type) {
  if (type == SLIDE) {
    return vec_size(x);
  } else if (type == PSLIDE_EMPTY) {
    return 0;
  } else {
    return vec_size(r_lst_get(x, 0));
  }
}

// -----------------------------------------------------------------------------

int compute_force(int type) {
  if (type == SLIDE) {
    return 1;
  } else if (type == SLIDE2) {
    return 2;
  } else {
    return type;
  }
}

// -----------------------------------------------------------------------------

SEXP copy_names(SEXP out, SEXP x, int type) {
  SEXP names;
  if (type == SLIDE) {
    names = PROTECT(vec_names(x));
  } else if (type == PSLIDE_EMPTY) {
    names = PROTECT(R_NilValue);
  } else {
    names = PROTECT(vec_names(r_lst_get(x, 0)));
  }

  UNPROTECT(1);
  return vec_set_names(out, names);
}

// -----------------------------------------------------------------------------

// `slice_and_update_env()` works by repeatedly overwriting the
// `container` with the results of slicing into `x`. This is mainly important
// for performance with `pslide()`, where `container` is a list the same size
// as `.l`. By repeatedly overwriting 1 list, we don't have to reallocate one
// every time we call `slice_and_update_env()`. For `slide()` and `slide2()`,
// `container` is just `NULL`.

// slide()
// - Slice `x` directly
// - Immediately define `container` as `.x` in `env`

// slide2()
// - Slice `x[[1]]`
// - Define `container` as `.x` in `env`
// - Slice `x[[2]]`
// - Define `container` as `.y` in `env`

// pslide()
// - For i in 1:length(.l)
//  - Slice `x[[i]]`
//  - Set the slice result as `container[[i]]`
// - Define `container` as `.l` in `env`

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
void slider_init_utils(SEXP ns) {
  slider_ns_env = ns;

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

  slider_shared_empty_lgl = Rf_allocVector(LGLSXP, 0);
  R_PreserveObject(slider_shared_empty_lgl);
  MARK_NOT_MUTABLE(slider_shared_empty_lgl);

  slider_shared_empty_int = Rf_allocVector(INTSXP, 0);
  R_PreserveObject(slider_shared_empty_int);
  MARK_NOT_MUTABLE(slider_shared_empty_int);
}

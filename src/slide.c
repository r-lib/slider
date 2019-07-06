#include "slurrr.h"
#include "utils.h"
#include <vctrs.h>

// -----------------------------------------------------------------------------

// Defined below
int iterations(int n, int before, int after, int step, int offset, bool partial, bool forward);

// -----------------------------------------------------------------------------

// just to test if it works
SEXP slurrr_slide(SEXP env,
                  SEXP x,
                  SEXP before_,
                  SEXP after_,
                  SEXP step_,
                  SEXP offset_,
                  SEXP partial_,
                  SEXP forward_,
                  SEXP ptype,
                  SEXP before_unbounded_,
                  SEXP after_unbounded_) {

  int before = r_int_get(before_, 0);
  int after = r_int_get(after_, 0);
  int step = r_int_get(step_, 0);
  int offset = r_int_get(offset_, 0);
  bool partial = r_lgl_get(partial_, 0);
  bool forward = r_lgl_get(forward_, 0);
  bool before_unbounded = r_lgl_get(before_unbounded_, 0);
  bool after_unbounded = r_lgl_get(after_unbounded_, 0);

  R_len_t x_n = vec_size(x);
  SEXP out = PROTECT(vec_init(ptype, x_n));

  // Determines assignment function used
  bool ptype_is_list = TYPEOF(ptype) == VECSXP;

  bool partial_unbounded = false;
  if (forward && after_unbounded | !forward && before_unbounded) {
    partial_unbounded = true;
  }

  // Number of "complete" iterations
  int complete_iterations_n = iterations(x_n, before, after, step, offset, partial_unbounded, forward);

  // Compute this before adjustments are made to .step/.offset
  int partial_iterations_n;
  if (partial) {
    int max_iterations_n = iterations(x_n, before, after, step, offset, partial, forward);
    partial_iterations_n = max_iterations_n - complete_iterations_n;
  }

  int startpoint;
  int endpoint;
  int start;
  int stop;

  // R indices for `vec_assign()`
  if (forward) {
    startpoint = 1;
    endpoint = x_n;
    start = startpoint - before + offset;
    stop = start + (before + after);
  }
  else {
    startpoint = x_n;
    endpoint = 1;
    start = startpoint - offset + after;
    stop = start - (before + after);
    step = -step;
    offset = -offset;
  }

  SEXP entry = PROTECT(r_int(startpoint + offset));
  int* entry_data = INTEGER(entry);

  int start_step = step;
  int stop_step = step;

  if (forward) {
    if (before_unbounded) {
        start_step = 0;
    }
    if (after_unbounded) {
        stop_step = 0;
    }
  } else {
    if (before_unbounded) {
        stop_step = 0;
    }
    if (after_unbounded) {
        start_step = 0;
    }
  }

  SEXP f_call = PROTECT(Rf_lang3(syms_dot_f, syms_slice, syms_dots));

  PROTECT_INDEX elt_prot_idx;
  SEXP elt = R_NilValue;
  PROTECT_WITH_INDEX(elt, &elt_prot_idx);

  for (int j = 0; j < complete_iterations_n; ++j) {
    // must be inside the loop since unbounded-ness could affect the length of i
    SEXP i = PROTECT(r_seq(start, stop));

    SEXP x_slice = PROTECT(vec_slice(x, i));
    Rf_defineVar(syms_slice, x_slice, env);

    SEXP elt = Rf_eval(f_call, env);
    REPROTECT(elt, elt_prot_idx);

    // will be way more efficient at the C level with `copy = FALSE`
    if (ptype_is_list) {
      SET_VECTOR_ELT(out, entry_data[0] - 1, elt);
    } else {
      elt = vctrs_cast(elt, ptype, strings_empty, strings_empty);
      REPROTECT(elt, elt_prot_idx);

      if (vec_size(elt) != 1) {
        Rf_errorcall(R_NilValue, "The size of each element must be 1 if .ptype is not a list");
      }

      vec_assign_impl(out, entry, elt, false);
    }

    start += start_step;
    stop += stop_step;
    *entry_data += step;
    UNPROTECT(2);
  }

  // Done if no `.partial`
  if (!partial) {
    UNPROTECT(4);
    return out;
  }

  // can't compute any partial iterations
  if (partial_iterations_n == 0) {
    UNPROTECT(4);
    return out;
  }

  for (int j = 0; j < partial_iterations_n; ++j) {
    // must be inside the loop since unbounded-ness could affect the length of i
    SEXP i = PROTECT(r_seq(start, endpoint));

    SEXP x_slice = PROTECT(vec_slice(x, i));
    Rf_defineVar(syms_slice, x_slice, env);

    SEXP elt = Rf_eval(f_call, env);
    REPROTECT(elt, elt_prot_idx);

    // will be way more efficient at the C level with `copy = FALSE`
    if (ptype_is_list) {
      SET_VECTOR_ELT(out, entry_data[0] - 1, elt);
    } else {
      elt = vctrs_cast(elt, ptype, strings_empty, strings_empty);
      REPROTECT(elt, elt_prot_idx);

      if (vec_size(elt) != 1) {
        Rf_errorcall(R_NilValue, "The size of each element must be 1 if .ptype is not a list");
      }

      vec_assign_impl(out, entry, elt, false);
    }

    start += start_step;
    *entry_data += step;
    UNPROTECT(2);
  }

  UNPROTECT(4);
  return out;
}

// -----------------------------------------------------------------------------

int adjust_partial(bool partial, bool forward, int before, int after) {
  if (partial) {
    if (forward) {
      return after + 1;
    } else {
      return before + 1;
    }
  } else {
    return 1;
  }
}

// number of positions lost to `.offset`
int adjust_n(bool forward, int offset, int before, int after) {
  if (forward) {
    return offset - before;
  } else {
    return offset - after;
  }
}

int iterations(int n, int before, int after, int step, int offset, bool partial, bool forward) {
  int width = before + after + 1;
  int adjust = adjust_partial(partial, forward, before, after);
  n = n - adjust_n(forward, offset, before, after);
  return ceil((n - width + adjust) / step);
}

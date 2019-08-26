#include "slide.h"
#include "slide-vctrs.h"
#include "utils.h"
#include "params.h"
#include <vctrs.h>

// -----------------------------------------------------------------------------

// [[ register() ]]
SEXP slide_common_impl(SEXP x,
                       SEXP f_call,
                       SEXP ptype,
                       SEXP env,
                       SEXP params) {

  int type = pull_type(params);

  int size = compute_size(x, type);

  // Bail early if inputs are size 0
  if (size == 0) {
    return vec_init(ptype, 0);
  }

  bool before_unbounded = false;
  bool after_unbounded = false;

  bool constrain = pull_constrain(params);
  int before = pull_before(params, &before_unbounded);
  int after = pull_after(params, &after_unbounded);
  int step = pull_step(params);
  bool complete = pull_complete(params);

  bool before_positive = before >= 0;
  bool after_positive = after >= 0;

  check_double_negativeness(before, after, before_positive, after_positive);
  check_before_negativeness(before, after, before_positive, after_unbounded);
  check_after_negativeness(after, before, after_positive, before_unbounded);

  // 1 based for usage as the index in `vec_assign()`
  SEXP iteration = PROTECT(r_int(1));
  int* p_iteration_val = INTEGER(iteration);
  int iteration_max = size;

  // Iteration adjustment
  if (complete) {
    if (before_positive) {
      *p_iteration_val += before;
    }
    if (after_positive) {
      iteration_max -= after;
    }
  } else {
    if (!before_positive) {
      iteration_max -= abs(before);
    }
    if (!after_positive) {
      *p_iteration_val += abs(after);
    }
  }

  // Forward adjustment to match the number of iterations
  int offset = 0;
  if (complete) {
    if (before_positive) {
      offset = before;
    }
  } else {
    if (!after_positive) {
      offset = abs(after);
    }
  }

  int start;
  int start_step;
  if (before_unbounded) {
    start = 0;
    start_step = 0;
  } else {
    start = offset - before;
    start_step = step;
  }

  int stop;
  int stop_step;
  if (after_unbounded) {
    stop = size - 1;
    stop_step = 0;
  } else {
    stop = offset + after;
    stop_step = step;
  }

  // Init and proxy the `out` container
  PROTECT_INDEX out_prot_idx;
  SEXP out = vec_init(ptype, size);
  PROTECT_WITH_INDEX(out, &out_prot_idx);
  out = vec_proxy(out);
  REPROTECT(out, out_prot_idx);

  // The indices to slice x with
  SEXP window = PROTECT(compact_seq(0, 0, true));
  int* p_window_val = INTEGER(window);

  // The result of each function call
  PROTECT_INDEX elt_prot_idx;
  SEXP elt = R_NilValue;
  PROTECT_WITH_INDEX(elt, &elt_prot_idx);

  // Mutable container for the results of slicing x
  SEXP container = PROTECT(make_slice_container(type));

  int window_start;
  int window_stop;
  int window_size;

  for (;
       *p_iteration_val <= iteration_max;
       *p_iteration_val += step, start += start_step, stop += stop_step) {

    if (*p_iteration_val % 1024 == 0) {
      R_CheckUserInterrupt();
    }

    window_start = max(start, 0);
    window_stop = min(stop, size - 1);
    window_size = window_stop - window_start + 1;

    init_compact_seq(p_window_val, window_start, window_size, true);

    slice_and_update_env(x, window, env, type, container);

    elt = Rf_eval(f_call, env);
    REPROTECT(elt, elt_prot_idx);

    // TODO - Worry about needing fallback method when no proxy is defined / is a matrix
    // https://github.com/r-lib/vctrs/blob/8d12bfc0e29e056966e0549af619253253752a64/src/slice-assign.c#L46

    if (constrain) {
      elt = vctrs_cast(elt, ptype, strings_empty, strings_empty);
      REPROTECT(elt, elt_prot_idx);
      elt = vec_proxy(elt);
      REPROTECT(elt, elt_prot_idx);

      if (vec_size(elt) != 1) {
        stop_not_all_size_one(*p_iteration_val, vec_size(elt));
      }

      vec_assign_impl(out, iteration, elt, false);
    } else {
      SET_VECTOR_ELT(out, *p_iteration_val - 1, elt);
    }
  }

  out = vec_restore(out, ptype, r_int(size));
  REPROTECT(out, out_prot_idx);

  out = copy_names(out, x, type);
  REPROTECT(out, out_prot_idx);

  UNPROTECT(5);
  return out;
}

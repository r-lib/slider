#include "slider.h"
#include "slider-vctrs.h"
#include "utils.h"
#include "params.h"

// -----------------------------------------------------------------------------

// [[ register() ]]
SEXP slide_common_impl(SEXP x,
                       SEXP f_call,
                       SEXP ptype,
                       SEXP env,
                       SEXP params) {

  const int type = pull_type(params);

  const int size = compute_size(x, type);

  // Bail early if inputs are size 0
  if (size == 0) {
    return vec_init(ptype, 0);
  }

  const int force = compute_force(type);

  bool before_unbounded = false;
  bool after_unbounded = false;

  const bool constrain = pull_constrain(params);
  const bool atomic = pull_atomic(params);
  const int before = pull_before(params, &before_unbounded);
  const int after = pull_after(params, &after_unbounded);
  const int step = pull_step(params);
  const bool complete = pull_complete(params);

  const bool before_positive = before >= 0;
  const bool after_positive = after >= 0;

  check_double_negativeness(before, after, before_positive, after_positive);
  check_before_negativeness(before, after, before_positive, after_unbounded);
  check_after_negativeness(after, before, after_positive, before_unbounded);

  // 1 based index for `vec_assign()`
  SEXP index;
  int* p_index;

  if (constrain) {
    index = PROTECT(r_int(0));
    p_index = INTEGER(index);
  } else {
    index = PROTECT(R_NilValue);
  }

  int iteration_min = 0;
  int iteration_max = size;

  // Iteration adjustment
  if (complete) {
    if (before_positive) {
      iteration_min += before;
    }
    if (after_positive) {
      iteration_max -= after;
    }
  }

  // Forward adjustment to match the number of iterations
  int offset = 0;
  if (complete && before_positive) {
    offset = before;
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

  // Proxy and init the `out` container
  PROTECT_INDEX out_prot_idx;
  SEXP out = vec_proxy(ptype);
  PROTECT_WITH_INDEX(out, &out_prot_idx);
  out = vec_init(out, size);
  REPROTECT(out, out_prot_idx);

  // Initialize with `NA`, not `NULL`, for size stability when auto-simplifying
  if (atomic && !constrain) {
    for (R_len_t i = 0; i < size; ++i) {
      SET_VECTOR_ELT(out, i, slider_shared_na_lgl);
    }
  }

  // The indices to slice x with
  SEXP window = PROTECT(compact_seq(0, 0, true));
  int* p_window = INTEGER(window);

  // The result of each function call
  PROTECT_INDEX elt_prot_idx;
  SEXP elt = R_NilValue;
  PROTECT_WITH_INDEX(elt, &elt_prot_idx);

  // Mutable container for the results of slicing x
  SEXP container = PROTECT(make_slice_container(type));

  for (int i = iteration_min; i < iteration_max; i += step, start += start_step, stop += stop_step) {
    if (i % 1024 == 0) {
      R_CheckUserInterrupt();
    }

    int window_start = max(start, 0);
    int window_stop = min(stop, size - 1);
    int window_size = window_stop - window_start + 1;

    // Happens when the entire window is OOB, we take a 0-slice of `x`.
    if (window_stop < window_start) {
      window_start = 0;
      window_size = 0;
    }

    init_compact_seq(p_window, window_start, window_size, true);

    slice_and_update_env(x, window, env, type, container);

#if defined(R_VERSION) && R_VERSION >= R_Version(3, 2, 3)
    elt = R_forceAndCall(f_call, force, env);
#else
    elt = Rf_eval(f_call, env);
#endif
    REPROTECT(elt, elt_prot_idx);

    if (atomic && vec_size(elt) != 1) {
      stop_not_all_size_one(i + 1, vec_size(elt));
    }

    if (constrain) {
      *p_index = i + 1;

      elt = vec_cast(elt, ptype);
      REPROTECT(elt, elt_prot_idx);

      out = vec_proxy_assign(out, index, elt);
      REPROTECT(out, out_prot_idx);
    } else {
      SET_VECTOR_ELT(out, i, elt);
    }
  }

  out = vec_restore(out, ptype);
  REPROTECT(out, out_prot_idx);

  out = copy_names(out, x, type);
  REPROTECT(out, out_prot_idx);

  UNPROTECT(5);
  return out;
}

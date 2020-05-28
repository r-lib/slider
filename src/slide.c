#include "slider.h"
#include "slider-vctrs.h"
#include "utils.h"
#include "params.h"
#include "assign.h"

// -----------------------------------------------------------------------------

#define SLIDE_LOOP(ASSIGN_ONE) do {                                            \
  for (int i = iteration_min;                                                  \
       i < iteration_max;                                                      \
       i += step, start += start_step, stop += stop_step) {                    \
                                                                               \
    if (i % 1024 == 0) {                                                       \
      R_CheckUserInterrupt();                                                  \
    }                                                                          \
                                                                               \
    int window_start = max(start, 0);                                          \
    int window_stop = min(stop, size - 1);                                     \
    int window_size = window_stop - window_start + 1;                          \
                                                                               \
    /* Happens when the entire window is OOB, we take a 0-slice of `x`. */     \
    if (window_stop < window_start) {                                          \
      window_start = 0;                                                        \
      window_size = 0;                                                         \
    }                                                                          \
                                                                               \
    init_compact_seq(p_window, window_start, window_size, true);               \
                                                                               \
    slice_and_update_env(x, window, env, type, container);                     \
                                                                               \
    SEXP elt = PROTECT(r_force_eval(f_call, env, force));                      \
                                                                               \
    if (atomic && vec_size(elt) != 1) {                                        \
      stop_not_all_size_one(i + 1, vec_size(elt));                             \
    }                                                                          \
                                                                               \
    ASSIGN_ONE(p_out, i, elt, ptype);                                          \
    UNPROTECT(1);                                                              \
  }                                                                            \
} while(0)

#define SLIDE_LOOP_ATOMIC(CTYPE, DEREF, ASSIGN_ONE) do { \
  CTYPE* p_out = DEREF(out);                             \
  SLIDE_LOOP(ASSIGN_ONE);                                \
} while (0)                                              \

#define SLIDE_LOOP_BARRIER(ASSIGN_ONE) do {                    \
  SEXP p_out = out;                                            \
                                                               \
  /* Initialize with `NA`, not `NULL` */                       \
  /* for size stability when auto-simplifying */               \
  if (atomic && !constrain) {                                  \
    for (R_len_t i = 0; i < size; ++i) {                       \
      SET_VECTOR_ELT(p_out, i, slider_shared_na_lgl);          \
    }                                                          \
  }                                                            \
                                                               \
  SLIDE_LOOP(ASSIGN_ONE);                                      \
} while (0)

// -----------------------------------------------------------------------------

// [[ register() ]]
SEXP slide_common_impl(SEXP x,
                       SEXP f_call,
                       SEXP ptype,
                       SEXP env,
                       SEXP params) {

  const int type = pull_type(params);
  const int force = compute_force(type);
  const int size = compute_size(x, type);

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

  // The indices to slice x with
  SEXP window = PROTECT(compact_seq(0, 0, true));
  int* p_window = INTEGER(window);

  // Mutable container for the results of slicing x
  SEXP container = PROTECT(make_slice_container(type));

  SEXPTYPE out_type = TYPEOF(ptype);
  SEXP out = PROTECT(slider_init(out_type, size));

  switch (out_type) {
  case INTSXP:  SLIDE_LOOP_ATOMIC(int, INTEGER, assign_one_int); break;
  case REALSXP: SLIDE_LOOP_ATOMIC(double, REAL, assign_one_dbl); break;
  case LGLSXP:  SLIDE_LOOP_ATOMIC(int, LOGICAL, assign_one_lgl); break;
  case STRSXP:  SLIDE_LOOP_ATOMIC(SEXP, STRING_PTR, assign_one_chr); break;
  case VECSXP:  SLIDE_LOOP_BARRIER(assign_one_lst); break;
  default:      never_reached("slide_common_impl");
  }

  SEXP names = slider_names(x, type);
  Rf_setAttrib(out, R_NamesSymbol, names);

  UNPROTECT(3);
  return out;
}

// -----------------------------------------------------------------------------

#undef SLIDE_LOOP
#undef SLIDE_LOOP_ATOMIC
#undef SLIDE_LOOP_BARRIER

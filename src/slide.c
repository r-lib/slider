#include "slider.h"
#include "slider-vctrs.h"
#include "utils.h"
#include "params.h"
#include "assign.h"
#include "opts-slide.h"

// -----------------------------------------------------------------------------

#define SLIDE_LOOP(ASSIGN_ONE) do {                                            \
  for (int i = iter_min; i < iter_max; i += iter_step) {                       \
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
    start += start_step;                                                       \
    stop += stop_step;                                                         \
                                                                               \
    init_compact_seq(p_window, window_start, window_size, true);               \
                                                                               \
    slice_and_update_env(x, window, env, type, container);                     \
                                                                               \
    SEXP elt = PROTECT(R_forceAndCall(f_call, force, env));                    \
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

  const int type = validate_type(r_lst_get(params, 0));
  const bool constrain = validate_constrain(r_lst_get(params, 1));
  const bool atomic = validate_atomic(r_lst_get(params, 2));

  const int force = compute_force(type);
  const int size = compute_size(x, type);

  SEXP before = r_lst_get(params, 3);
  SEXP after = r_lst_get(params, 4);
  SEXP step = r_lst_get(params, 5);
  SEXP complete = r_lst_get(params, 6);

  const bool dot = true;

  const struct slide_opts opts = new_slide_opts(
    before,
    after,
    step,
    complete,
    dot
  );

  const struct iter_opts iopts = new_iter_opts(opts, size);

  int iter_min = iopts.iter_min;
  int iter_max = iopts.iter_max;
  int iter_step = iopts.iter_step;

  int start = iopts.start;
  int stop = iopts.stop;

  int start_step = iopts.start_step;
  int stop_step = iopts.stop_step;

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

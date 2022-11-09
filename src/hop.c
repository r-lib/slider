#include "slider.h"
#include "slider-vctrs.h"
#include "utils.h"
#include "params.h"
#include "assign.h"

// -----------------------------------------------------------------------------

#define HOP_LOOP(ASSIGN_ONE) do {                                 \
  for (R_len_t i = 0; i < size; ++i) {                            \
    if (i % 1024 == 0) {                                          \
      R_CheckUserInterrupt();                                     \
    }                                                             \
                                                                  \
    int window_start = max(p_starts[i] - 1, 0);                   \
    int window_stop = min(p_stops[i] - 1, x_size - 1);            \
    int window_size = window_stop - window_start + 1;             \
                                                                  \
    /* This can happen if both `window_start` and */              \
    /* `window_stop` are outside the range of `x`. */             \
    /* We return a 0-size slice of `x`. */                        \
    if (window_stop < window_start) {                             \
      window_start = 0;                                           \
      window_size = 0;                                            \
    }                                                             \
                                                                  \
    init_compact_seq(p_window, window_start, window_size, true);  \
                                                                  \
    slice_and_update_env(x, window, env, type, container);        \
                                                                  \
    SEXP elt = PROTECT(R_forceAndCall(f_call, force, env));       \
                                                                  \
    if (atomic && vec_size(elt) != 1) {                           \
      stop_not_all_size_one(i + 1, vec_size(elt));                \
    }                                                             \
                                                                  \
    ASSIGN_ONE(p_out, i, elt, ptype);                             \
    UNPROTECT(1);                                                 \
  }                                                               \
} while (0)

#define HOP_LOOP_ATOMIC(CTYPE, DEREF, ASSIGN_ONE) do {         \
  CTYPE* p_out = DEREF(out);                                   \
  HOP_LOOP(ASSIGN_ONE);                                        \
} while (0)

#define HOP_LOOP_BARRIER(ASSIGN_ONE) do {                      \
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
  HOP_LOOP(ASSIGN_ONE);                                        \
} while (0)

// -----------------------------------------------------------------------------

// [[ register() ]]
SEXP hop_common_impl(SEXP x,
                     SEXP starts,
                     SEXP stops,
                     SEXP f_call,
                     SEXP ptype,
                     SEXP env,
                     SEXP params) {

  const int type = validate_type(r_lst_get(params, 0));
  const int force = compute_force(type);
  const bool constrain = validate_constrain(r_lst_get(params, 1));
  const bool atomic = validate_atomic(r_lst_get(params, 2));

  const R_len_t x_size = compute_size(x, type);
  const R_len_t size = vec_size(starts);

  const int* p_starts = INTEGER_RO(starts);
  const int* p_stops = INTEGER_RO(stops);

  check_hop_starts_not_past_stops(starts, stops, p_starts, p_stops, size);

  // The indices to slice x with
  SEXP window = PROTECT(compact_seq(0, 0, true));
  int* p_window = INTEGER(window);

  // Mutable container for the results of slicing x
  SEXP container = PROTECT(make_slice_container(type));

  SEXPTYPE out_type = TYPEOF(ptype);
  SEXP out = PROTECT(slider_init(out_type, size));

  switch (out_type) {
  case INTSXP:  HOP_LOOP_ATOMIC(int, INTEGER, assign_one_int); break;
  case REALSXP: HOP_LOOP_ATOMIC(double, REAL, assign_one_dbl); break;
  case LGLSXP:  HOP_LOOP_ATOMIC(int, LOGICAL, assign_one_lgl); break;
  case STRSXP:  HOP_LOOP_ATOMIC(SEXP, STRING_PTR, assign_one_chr); break;
  case VECSXP:  HOP_LOOP_BARRIER(assign_one_lst); break;
  default:      never_reached("hop_common_impl");
  }

  UNPROTECT(3);
  return out;
}

// -----------------------------------------------------------------------------

#undef HOP_LOOP
#undef HOP_LOOP_ATOMIC
#undef HOP_LOOP_BARRIER

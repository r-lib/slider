#include "slider.h"
#include "slider-vctrs.h"
#include "utils.h"
#include "params.h"

// -----------------------------------------------------------------------------

// [[ register() ]]
SEXP hop_common_impl(SEXP x,
                     SEXP starts,
                     SEXP stops,
                     SEXP f_call,
                     SEXP ptype,
                     SEXP env,
                     SEXP params) {

  int type = pull_type(params);

  int force = compute_force(type);

  bool constrain = pull_constrain(params);

  check_hop_starts_not_past_stops(starts, stops);

  R_len_t x_size = compute_size(x, type);
  R_len_t size = vec_size(starts);

  // 1 based index for `vec_assign()`
  SEXP index;
  int* p_index;

  if (constrain) {
    index = PROTECT(r_int(0));
    p_index = INTEGER(index);
  } else {
    index = PROTECT(R_NilValue);
  }

  // Proxy and init the `out` container
  PROTECT_INDEX out_prot_idx;
  SEXP out = vec_proxy(ptype);
  PROTECT_WITH_INDEX(out, &out_prot_idx);
  out = vec_init(out, size);
  REPROTECT(out, out_prot_idx);


  // The indices to slice x with
  SEXP window = PROTECT(compact_seq(0, 0, true));
  int* p_window = INTEGER(window);

  // The result of each function call
  PROTECT_INDEX elt_prot_idx;
  SEXP elt = R_NilValue;
  PROTECT_WITH_INDEX(elt, &elt_prot_idx);

  // Mutable container for the results of slicing x
  SEXP container = PROTECT(make_slice_container(type));

  int* p_starts = INTEGER(starts);
  int* p_stops = INTEGER(stops);

  for (R_len_t i = 0; i < size; ++i) {
    if (i % 1024 == 0) {
      R_CheckUserInterrupt();
    }

    int window_start = max(p_starts[i] - 1, 0);
    int window_stop = min(p_stops[i] - 1, x_size - 1);
    int window_size = window_stop - window_start + 1;

    // This can happen if both `window_start` and `window_stop` are outside
    // the range of `x`. i.e. `n = 3` but `window_start = 4`, `window_stop = 5`.
    // The clamp of `max(p_stops[i] - 1, size - 1)` above will make
    // `window_stop = 3`, then this adjustment is applied so we return a 0-size
    // slice of `x`
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

    if (constrain) {
      elt = vec_cast(elt, ptype);
      REPROTECT(elt, elt_prot_idx);

      if (vec_size(elt) != 1) {
        stop_not_all_size_one(i + 1, vec_size(elt));
      }

      *p_index = i + 1;

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

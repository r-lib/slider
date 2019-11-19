#include "slide.h"
#include "slide-between.h"
#include "slide-vctrs.h"
#include "utils.h"
#include "compare.h"
#include <vctrs.h>

// -----------------------------------------------------------------------------
// All defined below

static void compute_window_sizes(int*, SEXP, int);
static void compute_window_starts(int*, int*, int);
static void compute_window_stops(int*, int*, int*, int);

static struct out_info new_out_info(SEXP, SEXP, int);
static struct window_info new_window_info(int*, int*, int);
static struct index_info new_index_info(SEXP);
static struct last_info new_last_info(struct index_info);
static struct range_info new_range_info(SEXP, SEXP, int);
static struct iteration_info new_iteration_info(struct index_info, struct range_info, bool);

static void eval_loop(SEXP,
                      SEXP,
                      SEXP,
                      struct out_info,
                      struct index_info,
                      struct window_info,
                      struct range_info,
                      int,
                      bool,
                      bool);

// -----------------------------------------------------------------------------

// [[ register() ]]
SEXP slide_index_common_impl(SEXP x,
                             SEXP i,
                             SEXP starts,
                             SEXP stops,
                             SEXP f_call,
                             SEXP ptype,
                             SEXP env,
                             SEXP indices,
                             SEXP params) {
  int n_prot = 0;

  int type = r_scalar_int_get(r_lst_get(params, 0));
  bool constrain = r_scalar_lgl_get(r_lst_get(params, 1));
  bool complete = r_scalar_lgl_get(r_lst_get(params, 2));
  int out_size = r_scalar_int_get(r_lst_get(params, 3));

  struct index_info index = new_index_info(i);
  PROTECT_INDEX_INFO(&index, &n_prot);

  int* window_sizes = (int*) R_alloc(index.size, sizeof(int));
  int* window_starts = (int*) R_alloc(index.size, sizeof(int));
  int* window_stops = (int*) R_alloc(index.size, sizeof(int));

  compute_window_sizes(window_sizes, indices, index.size);
  compute_window_starts(window_starts, window_sizes, index.size);
  compute_window_stops(window_stops, window_sizes, window_starts, index.size);

  struct window_info window = new_window_info(window_starts, window_stops, index.size);
  PROTECT_WINDOW_INFO(&window, &n_prot);

  struct range_info range = new_range_info(starts, stops, index.size);
  PROTECT_RANGE_INFO(&range, &n_prot);

  struct out_info out = new_out_info(ptype, indices, out_size);
  PROTECT_OUT_INFO(&out, &n_prot);

  eval_loop(x, env, f_call, out, index, window, range, type, constrain, complete);

  out.data = vec_restore(out.data, out.ptype, r_int(out.size));
  REPROTECT(out.data, out.data_pidx);

  out.data = copy_names(out.data, x, type);
  REPROTECT(out.data, out.data_pidx);

  UNPROTECT(n_prot);
  return out.data;
}

// [[ register() ]]
SEXP slide_between_common_impl(SEXP x,
                               SEXP i,
                               SEXP starts,
                               SEXP stops,
                               SEXP f_call,
                               SEXP ptype,
                               SEXP env,
                               SEXP window_indices,
                               SEXP params) {
  int n_prot = 0;

  int type = r_scalar_int_get(r_lst_get(params, 0));
  bool constrain = r_scalar_lgl_get(r_lst_get(params, 1));
  int out_size = r_scalar_int_get(r_lst_get(params, 2));

  // `complete` is always FALSE for `slide_between()`
  bool complete = false;

  // `out_indices` are not required for `slide_between()` because the
  // starts/stops that a user provides are considered to be unique.
  // We special case this with NULL, which optimizes further calculations in
  // the `eval_loop()`
  SEXP out_indices = R_NilValue;

  struct index_info index = new_index_info(i);
  PROTECT_INDEX_INFO(&index, &n_prot);

  int* window_sizes = (int*) R_alloc(index.size, sizeof(int));
  int* window_starts = (int*) R_alloc(index.size, sizeof(int));
  int* window_stops = (int*) R_alloc(index.size, sizeof(int));

  compute_window_sizes(window_sizes, window_indices, index.size);
  compute_window_starts(window_starts, window_sizes, index.size);
  compute_window_stops(window_stops, window_sizes, window_starts, index.size);

  struct window_info window = new_window_info(window_starts, window_stops, index.size);
  PROTECT_WINDOW_INFO(&window, &n_prot);

  struct range_info range = new_range_info(starts, stops, out_size);
  PROTECT_RANGE_INFO(&range, &n_prot);

  struct out_info out = new_out_info(ptype, out_indices, out_size);
  PROTECT_OUT_INFO(&out, &n_prot);

  eval_loop(x, env, f_call, out, index, window, range, type, constrain, complete);

  out.data = vec_restore(out.data, out.ptype, r_int(out.size));
  REPROTECT(out.data, out.data_pidx);

  out.data = copy_names(out.data, x, type);
  REPROTECT(out.data, out.data_pidx);

  UNPROTECT(n_prot);
  return out.data;
}

// -----------------------------------------------------------------------------

static struct out_info new_out_info(SEXP ptype, SEXP indices, int size) {
  struct out_info out;

  out.data = PROTECT(vec_init(ptype, size));
  out.data = PROTECT(vec_proxy(out.data));

  out.ptype = ptype;
  out.size = size;

  out.indices = indices;
  out.has_indices = (indices != R_NilValue);

  out.index = PROTECT(r_int(0));
  out.p_index_val = INTEGER(out.index);

  out.index_size = 1;

  UNPROTECT(3);
  return out;
}

// -----------------------------------------------------------------------------

static struct window_info new_window_info(int* window_starts, int* window_stops, int size) {
  struct window_info window;

  window.starts = window_starts;
  window.stops = window_stops;

  window.start = 0;
  window.stop = window_stops[size - 1];

  window.seq = PROTECT(compact_seq(0, 0, true));
  window.p_seq_val = INTEGER(window.seq);

  UNPROTECT(1);
  return window;
}

static void init_window_seq(struct window_info window) {
  window.size = window.stop - window.start + 1;
  init_compact_seq(window.p_seq_val, window.start, window.size, true);
}

// -----------------------------------------------------------------------------

static struct index_info new_index_info(SEXP i) {
  struct index_info index;

  index.data = i;
  index.size = vec_size(i);

  index.first = PROTECT(vec_slice_impl(i, r_int(1)));
  index.last = PROTECT(vec_slice_impl(i, r_int(index.size)));

  index.compare_lt = get_compare_fn_lt(i);
  index.compare_gt = get_compare_fn_gt(i);
  index.compare_lte = get_compare_fn_lte(i);

  UNPROTECT(2);
  return index;
}

// -----------------------------------------------------------------------------

static struct last_info new_last_info(struct index_info index) {
  struct last_info last;

  last.start_loc = PROTECT(r_int(1));
  last.stop_loc = PROTECT(r_int(1));

  last.p_start_loc_val = INTEGER(last.start_loc);
  last.p_stop_loc_val = INTEGER(last.stop_loc);

  last.start_index = index.first;
  last.stop_index = index.first;

  // last.p_start_index and last.p_stop_index are initialized
  // after last.start_index and last.stop_index have been protected
  // inside PROTECT_LAST_INFO

  UNPROTECT(2);
  return last;
}

// -----------------------------------------------------------------------------

static void check_starts_not_past_stops(SEXP starts, SEXP stops);


static struct range_info new_range_info(SEXP starts, SEXP stops, int count) {
  struct range_info range;

  range.starts = starts;
  range.stops = stops;

  range.start = R_NilValue;
  range.stop = R_NilValue;

  range.start_unbounded = (starts == R_NilValue);
  range.stop_unbounded = (stops == R_NilValue);

  if (!range.start_unbounded && !range.stop_unbounded) {
    check_starts_not_past_stops(starts, stops);
  }

  range.count = count;

  return range;
}

static void stop_range_start_past_stop(SEXP starts, SEXP stops) {
  SEXP call = PROTECT(
    Rf_lang3(
      Rf_install("stop_range_start_past_stop"),
      starts,
      stops
    )
  );

  Rf_eval(call, slide_ns_env);
  Rf_error("Internal error: `stop_range_start_past_stop()` should have jumped earlier");
}

static void check_starts_not_past_stops(SEXP starts, SEXP stops) {
  bool any_gt = vec_any_gt(starts, stops);

  if (any_gt) {
    stop_range_start_past_stop(starts, stops);
  }
}

// -----------------------------------------------------------------------------

static int iteration_min_adjustment(struct index_info index, SEXP range, int size);
static int iteration_max_adjustment(struct index_info index, SEXP range, int size);

static struct iteration_info new_iteration_info(struct index_info index, struct range_info range, bool complete) {
  struct iteration_info iteration;

  int iteration_min = 1;
  int iteration_max = range.count;

  if (complete) {
    if (!range.start_unbounded) {
      iteration_min += iteration_min_adjustment(index, range.starts, range.count);
    }
    if (!range.stop_unbounded) {
      iteration_max -= iteration_max_adjustment(index, range.stops, range.count);
    }
  } else {
    if (!range.start_unbounded) {
      iteration_max -= iteration_max_adjustment(index, range.starts, range.count);
    }
    if (!range.stop_unbounded) {
      iteration_min += iteration_min_adjustment(index, range.stops, range.count);
    }
  }

  iteration.data = PROTECT(r_int(iteration_min));
  iteration.p_data_val = INTEGER(iteration.data);

  iteration.max = iteration_max;

  UNPROTECT(1);
  return iteration;
}

static int iteration_min_adjustment(struct index_info index, SEXP range, int size) {
  int forward_adjustment = 0;

  for (int j = 0; j < size; ++j) {
    if (index.compare_gt(index.first, 0, range, j)) {
      forward_adjustment++;
    } else {
      break;
    }
  }

  return forward_adjustment;
}

static int iteration_max_adjustment(struct index_info index, SEXP range, int size) {
  int backward_adjustment = 0;

  for (int j = size - 1; j >= 0; --j) {
    if (index.compare_lt(index.last, 0, range, j)) {
      backward_adjustment++;
    } else {
      break;
    }
  }

  return backward_adjustment;
}

// -----------------------------------------------------------------------------

// map_int(x, vec_size)
static void compute_window_sizes(int* window_sizes,
                                 SEXP window_indices,
                                 int size) {
  for (int i = 0; i < size; ++i) {
    window_sizes[i] = Rf_length(VECTOR_ELT(window_indices, i));
  }
}

static void compute_window_starts(int* window_starts,
                                  int* window_sizes,
                                  int size) {
  // First start is always 0
  window_starts[0] = 0;

  int sum = 0;

  // Then we do a cumsum() to get the rest of the starts
  for (int i = 1; i < size; ++i) {
    sum += window_sizes[i - 1];
    window_starts[i] = sum;
  }
}

static void compute_window_stops(int* window_stops,
                                 int* window_sizes,
                                 int* window_starts,
                                 int size) {
  for (int i = 0; i < size; ++i) {
    window_stops[i] = window_starts[i] + window_sizes[i] - 1;
  }
}

// -----------------------------------------------------------------------------

static int locate_window_start_index(struct index_info index,
                                     struct range_info range,
                                     struct last_info last) {

  while(index.compare_lt(*last.p_start_index, 0, range.start, 0)) {
    if (*last.p_start_loc_val == index.size) {
      return(index.size - 1);
    }

    (*last.p_start_loc_val)++;
    *last.p_start_index = vec_slice_impl(index.data, last.start_loc);
  }

  return *last.p_start_loc_val - 1;
}

static int locate_window_stop_index(struct index_info index,
                                    struct range_info range,
                                    struct last_info last) {

  while(index.compare_lte(*last.p_stop_index, 0, range.stop, 0)) {
    if (*last.p_stop_loc_val == index.size) {
      return(index.size - 1);
    }

    (*last.p_stop_loc_val)++;
    *last.p_stop_index = vec_slice_impl(index.data, last.stop_loc);
  }

  return *last.p_stop_loc_val - 2;
}

// -----------------------------------------------------------------------------

static void increment_window(struct window_info window,
                             struct index_info index,
                             struct iteration_info iteration,
                             struct range_info range,
                             struct last_info last) {
  if (!range.start_unbounded) {
    range.start = vec_slice_impl(range.starts, iteration.data);
    REPROTECT(range.start, range.start_pidx);

    window.start_idx = locate_window_start_index(index, range, last);
    window.start = window.starts[window.start_idx];
  }

  if (!range.stop_unbounded) {
    range.stop = vec_slice_impl(range.stops, iteration.data);
    REPROTECT(range.stop, range.stop_pidx);

    window.stop_idx = locate_window_stop_index(index, range, last);
    window.stop = window.stops[window.stop_idx];
  }

  // This can happen with an irregular index, and is a sign of the full window
  // being between two index points and means we select nothing
  if (window.stop < window.start) {
    window.start = 0;
    window.stop = -1;
  }

  init_window_seq(window);
}

// -----------------------------------------------------------------------------

static void eval_loop(SEXP x,
                      SEXP env,
                      SEXP f_call,
                      struct out_info out,
                      struct index_info index,
                      struct window_info window,
                      struct range_info range,
                      int type,
                      bool constrain,
                      bool complete) {
  int n_prot = 0;

  struct last_info last = new_last_info(index);
  PROTECT_LAST_INFO(&last, &n_prot);

  struct iteration_info iteration = new_iteration_info(index, range, complete);
  PROTECT_ITERATION_INFO(&iteration, &n_prot);

  // The result of each function call
  PROTECT_INDEX elt_prot_idx;
  SEXP elt = R_NilValue;
  PROTECT_WITH_INDEX(elt, &elt_prot_idx);
  ++n_prot;

  R_len_t elt_size;

  SEXP container = PROTECT(make_slice_container(type));
  ++n_prot;

  int force = compute_force(type);

  for (; *iteration.p_data_val <= iteration.max; ++(*iteration.p_data_val)) {
    if (*iteration.p_data_val % 1024 == 0) {
      R_CheckUserInterrupt();
    }

    increment_window(window, index, iteration, range, last);

    slice_and_update_env(x, window.seq, env, type, container);

#if defined(R_VERSION) && R_VERSION >= R_Version(3, 2, 3)
    elt = R_forceAndCall(f_call, force, env);
#else
    elt = Rf_eval(f_call, env);
#endif
    REPROTECT(elt, elt_prot_idx);

    if (out.has_indices) {
      out.index = VECTOR_ELT(out.indices, *iteration.p_data_val - 1);
      out.index_size = vec_size(out.index);
    } else {
      *out.p_index_val = *iteration.p_data_val;
    }

    // TODO - Worry about needing fallback method when no proxy is defined / is a matrix
    // https://github.com/r-lib/vctrs/blob/8d12bfc0e29e056966e0549af619253253752a64/src/slice-assign.c#L46

    if (constrain) {
      elt = vctrs_cast(elt, out.ptype, strings_empty, strings_empty);
      REPROTECT(elt, elt_prot_idx);
      elt = vec_proxy(elt);
      REPROTECT(elt, elt_prot_idx);

      elt_size = vec_size(elt);

      if (elt_size != 1) {
        stop_not_all_size_one(*iteration.p_data_val, elt_size);
      }

      if (out.index_size != 1) {
        elt = vec_recycle(elt, out.index_size);
        REPROTECT(elt, elt_prot_idx);
      }

      vec_assign_impl(out.data, out.index, elt, false);
      continue;
    }

    if (!out.has_indices) {
      SET_VECTOR_ELT(out.data, *out.p_index_val - 1, elt);
      continue;
    }

    out.p_index_val = INTEGER(out.index);

    for (int i = 0; i < out.index_size; ++i) {
      SET_VECTOR_ELT(out.data, out.p_index_val[i] - 1, elt);
    }
  }

  UNPROTECT(n_prot);
}

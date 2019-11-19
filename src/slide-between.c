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

static struct window_info new_window_info(int*, int*, int);
static struct index_info new_index_info(SEXP);
static struct range_info new_range_info(SEXP, SEXP, int);
static struct iteration_info new_iteration_info(struct index_info, struct range_info, bool);

static void increment_window(struct window_info window,
                             struct index_info* index,
                             struct range_info range,
                             int pos);

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
                             SEXP type_,
                             SEXP constrain_,
                             SEXP size_,
                             SEXP complete_) {
  int n_prot = 0;

  int type = r_scalar_int_get(type_);
  bool constrain = r_scalar_lgl_get(constrain_);
  int size = r_scalar_int_get(size_);
  bool complete = r_scalar_lgl_get(complete_);

  int force = compute_force(type);

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

  struct iteration_info iteration = new_iteration_info(index, range, complete);

  SEXP container = PROTECT_N(make_slice_container(type), &n_prot);

  SEXP out = PROTECT_N(vec_init(ptype, size), &n_prot);
  out = PROTECT_N(vec_proxy(out), &n_prot);

  for (int i = iteration.min; i < iteration.max; ++i) {
    if (i % 1024 == 0) {
      R_CheckUserInterrupt();
    }

    increment_window(window, &index, range, i);
    slice_and_update_env(x, window.seq, env, type, container);

#if defined(R_VERSION) && R_VERSION >= R_Version(3, 2, 3)
    SEXP elt = PROTECT(R_forceAndCall(f_call, force, env));
#else
    SEXP elt = PROTECT(Rf_eval(f_call, env));
#endif

    SEXP out_index = VECTOR_ELT(indices, i);
    int out_index_size = vec_size(out_index);

    // TODO - Worry about needing fallback method when no proxy is defined / is a matrix
    // https://github.com/r-lib/vctrs/blob/8d12bfc0e29e056966e0549af619253253752a64/src/slice-assign.c#L46

    if (constrain) {
      elt = PROTECT(vctrs_cast(elt, ptype, strings_empty, strings_empty));
      elt = PROTECT(vec_proxy(elt));

      R_len_t elt_size = vec_size(elt);

      if (elt_size != 1) {
        stop_not_all_size_one(i + 1, elt_size);
      }

      int recycled = 0;
      if (out_index_size != 1) {
        elt = PROTECT(vec_recycle(elt, out_index_size));
        recycled = 1;
      }

      vec_assign_impl(out, out_index, elt, false);
      UNPROTECT(2 + recycled);
    } else {
      int* p_out_index = INTEGER(out_index);

      for (int j = 0; j < out_index_size; ++j) {
        SET_VECTOR_ELT(out, p_out_index[j] - 1, elt);
      }
    }

    UNPROTECT(1);
  }

  out = PROTECT_N(vec_restore(out, ptype, size_), &n_prot);
  out = PROTECT_N(copy_names(out, x, type), &n_prot);

  UNPROTECT(n_prot);
  return out;
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
                               SEXP type_,
                               SEXP constrain_,
                               SEXP size_) {
  int n_prot = 0;

  int type = r_scalar_int_get(type_);
  bool constrain = r_scalar_lgl_get(constrain_);
  int size = r_scalar_int_get(size_);

  int force = compute_force(type);

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

  struct range_info range = new_range_info(starts, stops, size);
  PROTECT_RANGE_INFO(&range, &n_prot);

  // `complete = false` for `slide_between()`
  struct iteration_info iteration = new_iteration_info(index, range, false);

  SEXP container = PROTECT_N(make_slice_container(type), &n_prot);

  SEXP out = PROTECT_N(vec_init(ptype, size), &n_prot);
  out = PROTECT_N(vec_proxy(out), &n_prot);

  // 1 based index for `vec_assign()`
  SEXP out_index;
  int* p_out_index;

  if (constrain) {
    out_index = PROTECT_N(r_int(0), &n_prot);
    p_out_index = INTEGER(out_index);
  }

  for (int i = iteration.min; i < iteration.max; ++i) {
    if (i % 1024 == 0) {
      R_CheckUserInterrupt();
    }

    increment_window(window, &index, range, i);
    slice_and_update_env(x, window.seq, env, type, container);

#if defined(R_VERSION) && R_VERSION >= R_Version(3, 2, 3)
    SEXP elt = PROTECT(R_forceAndCall(f_call, force, env));
#else
    SEXP elt = PROTECT(Rf_eval(f_call, env));
#endif

    // TODO - Worry about needing fallback method when no proxy is defined / is a matrix
    // https://github.com/r-lib/vctrs/blob/8d12bfc0e29e056966e0549af619253253752a64/src/slice-assign.c#L46

    if (constrain) {
      elt = PROTECT(vctrs_cast(elt, ptype, strings_empty, strings_empty));
      elt = PROTECT(vec_proxy(elt));

      R_len_t elt_size = vec_size(elt);

      if (elt_size != 1) {
        stop_not_all_size_one(i + 1, elt_size);
      }

      *p_out_index = i + 1;

      vec_assign_impl(out, out_index, elt, false);
      UNPROTECT(2);
    } else {
      SET_VECTOR_ELT(out, i, elt);
    }

    UNPROTECT(1);
  }

  out = PROTECT_N(vec_restore(out, ptype, size_), &n_prot);
  out = PROTECT_N(copy_names(out, x, type), &n_prot);

  UNPROTECT(n_prot);
  return out;
}

// -----------------------------------------------------------------------------

static struct window_info new_window_info(int* window_starts, int* window_stops, int size) {
  struct window_info window;

  window.starts = window_starts;
  window.stops = window_stops;

  // Initialized to values that are correct if
  // either start or stops is unbounded
  window.starts_pos = 0;
  window.stops_pos = size - 1;

  window.seq = PROTECT(compact_seq(0, 0, true));
  window.p_seq_val = INTEGER(window.seq);

  UNPROTECT(1);
  return window;
}

static void init_window_seq(struct window_info window) {
  int start = window.starts[window.starts_pos];
  int stop = window.stops[window.stops_pos];

  // This can happen with an irregular index, and is a sign of the full window
  // being between two index points and means we select nothing
  if (stop < start) {
    start = 0;
    stop = -1;
  }

  int size = stop - start + 1;
  init_compact_seq(window.p_seq_val, start, size, true);
}

// -----------------------------------------------------------------------------

static struct index_info new_index_info(SEXP i) {
  struct index_info index;

  index.data = i;
  index.size = vec_size(i);
  index.last_pos = index.size - 1;

  index.current_start_pos = 0;
  index.current_stop_pos = 0;

  index.compare_lt = get_compare_fn_lt(i);
  index.compare_gt = get_compare_fn_gt(i);
  index.compare_lte = get_compare_fn_lte(i);

  return index;
}

// -----------------------------------------------------------------------------

static void check_starts_not_past_stops(SEXP starts, SEXP stops);

static struct range_info new_range_info(SEXP starts, SEXP stops, int size) {
  struct range_info range;

  range.starts = starts;
  range.stops = stops;

  range.start_unbounded = (starts == R_NilValue);
  range.stop_unbounded = (stops == R_NilValue);

  if (!range.start_unbounded && !range.stop_unbounded) {
    check_starts_not_past_stops(starts, stops);
  }

  range.size = size;

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
  if (vec_any_gt(starts, stops)) {
    stop_range_start_past_stop(starts, stops);
  }
}

// -----------------------------------------------------------------------------

static int iteration_min_adjustment(struct index_info index, SEXP range, int size);
static int iteration_max_adjustment(struct index_info index, SEXP range, int size);

static struct iteration_info new_iteration_info(struct index_info index, struct range_info range, bool complete) {
  struct iteration_info iteration;

  iteration.min = 0;
  iteration.max = range.size;

  if (complete) {
    if (!range.start_unbounded) {
      iteration.min += iteration_min_adjustment(index, range.starts, range.size);
    }
    if (!range.stop_unbounded) {
      iteration.max -= iteration_max_adjustment(index, range.stops, range.size);
    }
  } else {
    if (!range.start_unbounded) {
      iteration.max -= iteration_max_adjustment(index, range.starts, range.size);
    }
    if (!range.stop_unbounded) {
      iteration.min += iteration_min_adjustment(index, range.stops, range.size);
    }
  }

  return iteration;
}

static int iteration_min_adjustment(struct index_info index, SEXP range, int size) {
  int forward_adjustment = 0;

  for (int j = 0; j < size; ++j) {
    if (index.compare_gt(index.data, 0, range, j)) {
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
    if (index.compare_lt(index.data, index.last_pos, range, j)) {
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
// `index` is passed by pointer so we can permanently
// update the current start/stop position

static int locate_window_starts_pos(struct index_info* index, struct range_info range, int pos) {
  while(index->compare_lt(index->data, index->current_start_pos, range.starts, pos)) {
    if (index->current_start_pos == index->last_pos) {
      return index->current_start_pos;
    }
    index->current_start_pos++;
  }

  return index->current_start_pos;
}

static int locate_window_stops_pos(struct index_info* index, struct range_info range, int pos) {
  while(index->compare_lte(index->data, index->current_stop_pos, range.stops, pos)) {
    if (index->current_stop_pos == index->last_pos) {
      return index->current_stop_pos;
    }
    index->current_stop_pos++;
  }

  return index->current_stop_pos - 1;
}

// -----------------------------------------------------------------------------

static void increment_window(struct window_info window,
                             struct index_info* index,
                             struct range_info range,
                             int pos) {
  if (!range.start_unbounded) {
    window.starts_pos = locate_window_starts_pos(index, range, pos);
  }

  if (!range.stop_unbounded) {
    window.stops_pos = locate_window_stops_pos(index, range, pos);
  }

  init_window_seq(window);
}

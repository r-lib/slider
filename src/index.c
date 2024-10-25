#include "slider.h"
#include "index.h"
#include "slider-vctrs.h"
#include "utils.h"
#include "assign.h"

// -----------------------------------------------------------------------------

#define SLIDE_INDEX_LOOP(ASSIGN_LOCS) do {                     \
  for (int i = min_iteration; i < max_iteration; ++i) {        \
    if (i % 1024 == 0) {                                       \
      R_CheckUserInterrupt();                                  \
    }                                                          \
                                                               \
    increment_window(window, &index, range, i);                \
    slice_and_update_env(x, window.seq, env, type, container); \
                                                               \
    SEXP elt = PROTECT(R_forceAndCall(f_call, force, env));    \
                                                               \
    if (atomic && vec_size(elt) != 1) {                        \
      stop_not_all_size_one(i + 1, vec_size(elt));             \
    }                                                          \
                                                               \
    int peer_start = window.p_peer_starts[i];                  \
    int peer_size = window.p_peer_sizes[i];                    \
                                                               \
    ASSIGN_LOCS(p_out, peer_start, peer_size, elt, ptype);     \
    UNPROTECT(1);                                              \
  }                                                            \
} while (0)

#define SLIDE_INDEX_LOOP_ATOMIC(CTYPE, DEREF, ASSIGN_LOCS) do { \
  CTYPE* p_out = DEREF(out);                                    \
  SLIDE_INDEX_LOOP(ASSIGN_LOCS);                                \
} while (0)

#define SLIDE_INDEX_LOOP_BARRIER(ASSIGN_LOCS) do { \
  SEXP p_out = out;                                \
  SLIDE_INDEX_LOOP(ASSIGN_LOCS);                   \
} while (0)

// -----------------------------------------------------------------------------

// [[ register() ]]
SEXP slide_index_common_impl(SEXP x,
                             SEXP i,
                             SEXP starts,
                             SEXP stops,
                             SEXP f_call,
                             SEXP ptype,
                             SEXP env,
                             SEXP peer_sizes,
                             SEXP type_,
                             SEXP constrain_,
                             SEXP atomic_,
                             SEXP size_,
                             SEXP complete_) {
  int n_prot = 0;

  const int type = r_scalar_int_get(type_);
  const int force = compute_force(type);
  const bool constrain = r_scalar_lgl_get(constrain_);
  const bool atomic = r_scalar_lgl_get(atomic_);
  const int size = r_scalar_int_get(size_);
  const bool complete = r_scalar_lgl_get(complete_);

  struct index_info index = new_index_info(i);
  PROTECT_INDEX_INFO(&index, &n_prot);

  const int* p_peer_sizes = INTEGER_RO(peer_sizes);
  int* p_peer_starts = (int*) R_alloc(index.size, sizeof(int));
  int* p_peer_stops = (int*) R_alloc(index.size, sizeof(int));
  fill_peer_info(p_peer_sizes, index.size, p_peer_starts, p_peer_stops);

  struct window_info window = new_window_info(p_peer_sizes, p_peer_starts, p_peer_stops);
  PROTECT_WINDOW_INFO(&window, &n_prot);

  struct range_info range = new_range_info(starts, stops, index.size);
  PROTECT_RANGE_INFO(&range, &n_prot);

  const int min_iteration = compute_min_iteration(index, range, complete);
  const int max_iteration = compute_max_iteration(index, range, complete);

  SEXP container = PROTECT_N(make_slice_container(type), &n_prot);

  SEXPTYPE out_type = TYPEOF(ptype);
  SEXP out = PROTECT_N(slider_init(out_type, size), &n_prot);

  if (atomic && !constrain && out_type == VECSXP) {
    // Initialize with `NA`, not `NULL`, for size stability when auto simplifying
    list_fill(out, slider_shared_na_lgl);
  }

  switch (out_type) {
  case INTSXP:  SLIDE_INDEX_LOOP_ATOMIC(int, INTEGER, assign_locs_int); break;
  case REALSXP: SLIDE_INDEX_LOOP_ATOMIC(double, REAL, assign_locs_dbl); break;
  case LGLSXP:  SLIDE_INDEX_LOOP_ATOMIC(int, LOGICAL, assign_locs_lgl); break;
  case STRSXP:  SLIDE_INDEX_LOOP_BARRIER(assign_locs_chr); break;
  case VECSXP:  SLIDE_INDEX_LOOP_BARRIER(assign_locs_lst); break;
  default:      never_reached("slide_index_common_impl");
  }

  SEXP names = slider_names(x, type);
  Rf_setAttrib(out, R_NamesSymbol, names);

  UNPROTECT(n_prot);
  return out;
}

#undef SLIDE_INDEX_LOOP
#undef SLIDE_INDEX_LOOP_ATOMIC
#undef SLIDE_INDEX_LOOP_BARRIER

// -----------------------------------------------------------------------------

#define HOP_INDEX_LOOP(ASSIGN_ONE) do {                        \
  for (int i = 0; i < range.size; ++i) {                       \
    if (i % 1024 == 0) {                                       \
      R_CheckUserInterrupt();                                  \
    }                                                          \
                                                               \
    increment_window(window, &index, range, i);                \
    slice_and_update_env(x, window.seq, env, type, container); \
                                                               \
    SEXP elt = PROTECT(R_forceAndCall(f_call, force, env));    \
                                                               \
    if (atomic && vec_size(elt) != 1) {                        \
      stop_not_all_size_one(i + 1, vec_size(elt));             \
    }                                                          \
                                                               \
    ASSIGN_ONE(p_out, i, elt, ptype);                          \
    UNPROTECT(1);                                              \
  }                                                            \
} while (0)

#define HOP_INDEX_LOOP_ATOMIC(CTYPE, DEREF, ASSIGN_ONE) do {  \
  CTYPE* p_out = DEREF(out);                                  \
  HOP_INDEX_LOOP(ASSIGN_ONE);                                 \
} while (0)

#define HOP_INDEX_LOOP_BARRIER(ASSIGN_ONE) do {               \
  SEXP p_out = out;                                           \
  HOP_INDEX_LOOP(ASSIGN_ONE);                                 \
} while (0)

// -----------------------------------------------------------------------------

// [[ register() ]]
SEXP hop_index_common_impl(SEXP x,
                           SEXP i,
                           SEXP starts,
                           SEXP stops,
                           SEXP f_call,
                           SEXP ptype,
                           SEXP env,
                           SEXP peer_sizes,
                           SEXP type_,
                           SEXP constrain_,
                           SEXP atomic_,
                           SEXP size_) {
  int n_prot = 0;

  const int type = r_scalar_int_get(type_);
  const int force = compute_force(type);
  const bool constrain = r_scalar_lgl_get(constrain_);
  const bool atomic = r_scalar_lgl_get(atomic_);
  const int size = r_scalar_int_get(size_);

  struct index_info index = new_index_info(i);
  PROTECT_INDEX_INFO(&index, &n_prot);

  const int* p_peer_sizes = INTEGER_RO(peer_sizes);
  int* p_peer_starts = (int*) R_alloc(index.size, sizeof(int));
  int* p_peer_stops = (int*) R_alloc(index.size, sizeof(int));
  fill_peer_info(p_peer_sizes, index.size, p_peer_starts, p_peer_stops);

  struct window_info window = new_window_info(p_peer_sizes, p_peer_starts, p_peer_stops);
  PROTECT_WINDOW_INFO(&window, &n_prot);

  struct range_info range = new_range_info(starts, stops, size);
  PROTECT_RANGE_INFO(&range, &n_prot);

  SEXP container = PROTECT_N(make_slice_container(type), &n_prot);

  SEXPTYPE out_type = TYPEOF(ptype);
  SEXP out = PROTECT_N(slider_init(out_type, size), &n_prot);

  if (atomic && !constrain && out_type == VECSXP) {
    // Initialize with `NA`, not `NULL`, for size stability when auto simplifying
    list_fill(out, slider_shared_na_lgl);
  }

  switch (out_type) {
  case INTSXP:  HOP_INDEX_LOOP_ATOMIC(int, INTEGER, assign_one_int); break;
  case REALSXP: HOP_INDEX_LOOP_ATOMIC(double, REAL, assign_one_dbl); break;
  case LGLSXP:  HOP_INDEX_LOOP_ATOMIC(int, LOGICAL, assign_one_lgl); break;
  case STRSXP:  HOP_INDEX_LOOP_BARRIER(assign_one_chr); break;
  case VECSXP:  HOP_INDEX_LOOP_BARRIER(assign_one_lst); break;
  default:      never_reached("hop_index_common_impl");
  }

  UNPROTECT(n_prot);
  return out;
}

#undef HOP_INDEX_LOOP
#undef HOP_INDEX_LOOP_ATOMIC
#undef HOP_INDEX_LOOP_BARRIER

// -----------------------------------------------------------------------------

// [[ include("index.h") ]]
struct window_info new_window_info(const int* p_peer_sizes,
                                   const int* p_peer_starts,
                                   const int* p_peer_stops) {
  struct window_info window;

  window.p_peer_sizes = p_peer_sizes;
  window.p_peer_starts = p_peer_starts;
  window.p_peer_stops = p_peer_stops;

  window.seq = PROTECT(compact_seq(0, 0, true));
  window.p_seq_val = INTEGER(window.seq);

  UNPROTECT(1);
  return window;
}

// -----------------------------------------------------------------------------

// [[ include("index.h") ]]
struct index_info new_index_info(SEXP i) {
  struct index_info index;

  index.data = i;
  index.p_data = INTEGER_RO(i);
  index.size = vec_size(i);
  index.last_pos = index.size - 1;

  index.current_start_pos = 0;
  index.current_stop_pos = 0;

  return index;
}

// -----------------------------------------------------------------------------

// [[ include("index.h") ]]
struct range_info new_range_info(SEXP starts, SEXP stops, int size) {
  struct range_info range;

  range.starts = starts;
  range.stops = stops;

  range.start_unbounded = (starts == R_NilValue);
  range.stop_unbounded = (stops == R_NilValue);

  range.size = size;

  if (!range.start_unbounded) {
    range.p_starts = INTEGER_RO(starts);
  }
  if (!range.stop_unbounded) {
    range.p_stops = INTEGER_RO(stops);
  }
  if (!range.start_unbounded && !range.stop_unbounded) {
    check_slide_starts_not_past_stops(
      range.starts,
      range.stops,
      range.p_starts,
      range.p_stops,
      range.size
    );
  }

  return range;
}

// -----------------------------------------------------------------------------

static int iteration_min_adjustment(struct index_info index, const int* p_range, int size);
static int iteration_max_adjustment(struct index_info index, const int* p_range, int size);

// [[ include("index.h") ]]
int compute_min_iteration(struct index_info index, struct range_info range, bool complete) {
  int out = 0;

  if (!complete || range.start_unbounded) {
    return out;
  }

  out += iteration_min_adjustment(index, range.p_starts, range.size);

  return out;
}

// [[ include("index.h") ]]
int compute_max_iteration(struct index_info index, struct range_info range, bool complete) {
  int out = range.size;

  if (!complete || range.stop_unbounded) {
    return out;
  }

  out -= iteration_max_adjustment(index, range.p_stops, range.size);

  return out;
}

static int iteration_min_adjustment(struct index_info index, const int* p_range, int size) {
  int forward_adjustment = 0;

  if (size == 0) {
    return forward_adjustment;
  }

  const int first_index = index.p_data[0];

  for (int j = 0; j < size; ++j) {
    if (first_index > p_range[j]) {
      ++forward_adjustment;
    } else {
      break;
    }
  }

  return forward_adjustment;
}

static int iteration_max_adjustment(struct index_info index, const int* p_range, int size) {
  int backward_adjustment = 0;

  if (size == 0) {
    return backward_adjustment;
  }

  const int last_index = index.p_data[index.last_pos];

  for (int j = size - 1; j >= 0; --j) {
    if (last_index < p_range[j]) {
      ++backward_adjustment;
    } else {
      break;
    }
  }

  return backward_adjustment;
}

// -----------------------------------------------------------------------------

// [[ include("index.h") ]]
void fill_peer_info(const int* p_peer_sizes,
                    int size,
                    int* p_peer_starts,
                    int* p_peer_stops) {
  int peer_start = 0;

  for (int i = 0; i < size; ++i) {
    const int peer_size = p_peer_sizes[i];

    p_peer_starts[i] = peer_start;
    p_peer_stops[i] = peer_start + peer_size - 1;

    peer_start += peer_size;
  }
}

// -----------------------------------------------------------------------------
// `index` is passed by pointer so we can permanently
// update the current start/stop position

// [[ include("index.h") ]]
int locate_peer_starts_pos(struct index_info* index, struct range_info range, int pos) {
  // Pin to the start
  if (range.start_unbounded) {
    return 0;
  }

  // Past the end? Signal OOB with `last_pos + 1`.
  // This also handles size zero `.i` with `.starts` / `.stops` that have size.
  // Current pos will be 0, but `last_pos` will be -1.
  if (index->current_start_pos > index->last_pos) {
    return index->last_pos + 1;
  }

  while (index->p_data[index->current_start_pos] < range.p_starts[pos]) {
    ++index->current_start_pos;

    // Past the end? Signal OOB with `last_pos + 1`.
    if (index->current_start_pos > index->last_pos) {
      return index->last_pos + 1;
    }
  }

  return index->current_start_pos;
}

// [[ include("index.h") ]]
int locate_peer_stops_pos(struct index_info* index, struct range_info range, int pos) {
  // Pin to the end
  if (range.stop_unbounded) {
    return index->last_pos;
  }

  // Past the end? Pin to end.
  // This also handles size zero `.i` with `.starts` / `.stops` that have size.
  // Current pos will be 0, but `last_pos` will be -1.
  if (index->current_stop_pos > index->last_pos) {
    return index->last_pos;
  }

  while (index->p_data[index->current_stop_pos] <= range.p_stops[pos]) {
    ++index->current_stop_pos;

    // Past the end? Pin to end.
    if (index->current_stop_pos > index->last_pos) {
      return index->last_pos;
    }
  }

  return index->current_stop_pos - 1;
}

// -----------------------------------------------------------------------------

// [[ include("index.h") ]]
void increment_window(struct window_info window,
                      struct index_info* index,
                      struct range_info range,
                      int pos) {
  int peer_starts_pos = locate_peer_starts_pos(index, range, pos);
  int peer_stops_pos = locate_peer_stops_pos(index, range, pos);

  if (peer_stops_pos < peer_starts_pos) {
    init_compact_seq(window.p_seq_val, 0, 0, true);
    return;
  }

  int window_start = window.p_peer_starts[peer_starts_pos];
  int window_stop = window.p_peer_stops[peer_stops_pos];
  int window_size = window_stop - window_start + 1;

  init_compact_seq(window.p_seq_val, window_start, window_size, true);
}

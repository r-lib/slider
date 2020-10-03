#include "slider.h"
#include "slider-vctrs.h"
#include "utils.h"
#include "params.h"
#include "index.h"
#include "segment-tree.h"
#include "summary-core.h"

// -----------------------------------------------------------------------------

typedef SEXP (*summary_index_fn)(SEXP x,
                                 SEXP i,
                                 SEXP starts,
                                 SEXP stops,
                                 SEXP indices,
                                 bool complete,
                                 bool na_rm);

typedef void (*summary_index_impl_fn)(const double* p_x,
                                      R_xlen_t size,
                                      int iter_min,
                                      int iter_max,
                                      const struct range_info range,
                                      const int* window_sizes,
                                      const int* window_starts,
                                      const int* window_stops,
                                      SEXP indices,
                                      bool na_rm,
                                      struct index_info* p_index,
                                      double* p_out);

static SEXP slider_index_summary(SEXP x,
                                 SEXP i,
                                 SEXP starts,
                                 SEXP stops,
                                 SEXP indices,
                                 SEXP complete,
                                 SEXP na_rm,
                                 summary_index_fn fn) {
  bool dot = false;
  bool c_complete = validate_complete(complete, dot);
  bool c_na_rm = validate_na_rm(na_rm, dot);
  return fn(x, i, starts, stops, indices, c_complete, c_na_rm);
}

static SEXP slide_index_summary(SEXP x,
                                SEXP i,
                                SEXP starts,
                                SEXP stops,
                                SEXP indices,
                                bool complete,
                                bool na_rm,
                                summary_index_impl_fn fn) {
  int n_prot = 0;

  // Before `vec_cast()`, which may drop names
  SEXP names = PROTECT_N(slider_names(x, SLIDE), &n_prot);

  x = PROTECT_N(vec_cast(x, slider_shared_empty_dbl), &n_prot);
  const double* p_x = REAL(x);

  const R_xlen_t size = Rf_xlength(x);

  SEXP out = PROTECT_N(slider_init(REALSXP, size), &n_prot);
  double* p_out = REAL(out);
  Rf_setAttrib(out, R_NamesSymbol, names);

  struct index_info index = new_index_info(i);
  PROTECT_INDEX_INFO(&index, &n_prot);

  int* window_sizes = (int*) R_alloc(index.size, sizeof(int));
  int* window_starts = (int*) R_alloc(index.size, sizeof(int));
  int* window_stops = (int*) R_alloc(index.size, sizeof(int));

  fill_window_info(window_sizes, window_starts, window_stops, indices, index.size);

  struct range_info range = new_range_info(starts, stops, index.size);
  PROTECT_RANGE_INFO(&range, &n_prot);

  const int iter_min = compute_min_iteration(index, range, complete);
  const int iter_max = compute_max_iteration(index, range, complete);

  fn(
    p_x,
    size,
    iter_min,
    iter_max,
    range,
    window_sizes,
    window_starts,
    window_stops,
    indices,
    na_rm,
    &index,
    p_out
  );

  UNPROTECT(n_prot);
  return out;
}

static inline void slide_index_summary_loop(const struct segment_tree* p_tree,
                                            int iter_min,
                                            int iter_max,
                                            const struct range_info range,
                                            const int* window_sizes,
                                            const int* window_starts,
                                            const int* window_stops,
                                            SEXP indices,
                                            struct index_info* p_index,
                                            double* p_out) {
  double result = 0;

  for (int i = iter_min; i < iter_max; ++i) {
    if (i % 1024 == 0) {
      R_CheckUserInterrupt();
    }

    int starts_pos = locate_window_starts_pos(p_index, range, i);
    int stops_pos = locate_window_stops_pos(p_index, range, i);

    int window_start;
    int window_stop;

    if (stops_pos < starts_pos) {
      // Signal that the window selection was completely OOB
      window_start = 0;
      window_stop = 0;
    } else {
      window_start = window_starts[starts_pos];
      window_stop = window_stops[stops_pos] + 1;
    }

    segment_tree_aggregate(p_tree, window_start, window_stop, &result);

    SEXP locations = VECTOR_ELT(indices, i);
    const int* p_locations = INTEGER(locations);
    const R_len_t size_locations = window_sizes[i];

    for (R_len_t j = 0; j < size_locations; ++j) {
      R_len_t loc = p_locations[j] - 1;
      p_out[loc] = result;
    }
  }
}

// -----------------------------------------------------------------------------

static void slider_index_sum_core_impl(const double* p_x,
                                       R_xlen_t size,
                                       int iter_min,
                                       int iter_max,
                                       const struct range_info range,
                                       const int* window_sizes,
                                       const int* window_starts,
                                       const int* window_stops,
                                       SEXP indices,
                                       bool na_rm,
                                       struct index_info* p_index,
                                       double* p_out) {
  int n_prot = 0;

  long double state = 0;

  struct segment_tree tree = new_segment_tree(
    size,
    p_x,
    &state,
    sum_state_reset,
    sum_state_finalize,
    sum_nodes_increment,
    sum_nodes_initialize,
    na_rm ? sum_na_rm_aggregate_from_leaves : sum_na_keep_aggregate_from_leaves,
    na_rm ? sum_na_rm_aggregate_from_nodes : sum_na_keep_aggregate_from_nodes
  );
  PROTECT_SEGMENT_TREE(&tree, &n_prot);

  slide_index_summary_loop(
    &tree,
    iter_min,
    iter_max,
    range,
    window_sizes,
    window_starts,
    window_stops,
    indices,
    p_index,
    p_out
  );

  UNPROTECT(n_prot);
}

static SEXP slide_index_sum_core(SEXP x,
                                 SEXP i,
                                 SEXP starts,
                                 SEXP stops,
                                 SEXP indices,
                                 bool complete,
                                 bool na_rm) {
  return slide_index_summary(
    x,
    i,
    starts,
    stops,
    indices,
    complete,
    na_rm,
    slider_index_sum_core_impl
  );
}

// [[ register() ]]
SEXP slider_index_sum_core(SEXP x,
                           SEXP i,
                           SEXP starts,
                           SEXP stops,
                           SEXP indices,
                           SEXP complete,
                           SEXP na_rm) {
  return slider_index_summary(
    x,
    i,
    starts,
    stops,
    indices,
    complete,
    na_rm,
    slide_index_sum_core
  );
}

// -----------------------------------------------------------------------------

static void slider_index_prod_core_impl(const double* p_x,
                                        R_xlen_t size,
                                        int iter_min,
                                        int iter_max,
                                        const struct range_info range,
                                        const int* window_sizes,
                                        const int* window_starts,
                                        const int* window_stops,
                                        SEXP indices,
                                        bool na_rm,
                                        struct index_info* p_index,
                                        double* p_out) {
  int n_prot = 0;

  long double state = 1;

  struct segment_tree tree = new_segment_tree(
    size,
    p_x,
    &state,
    prod_state_reset,
    prod_state_finalize,
    prod_nodes_increment,
    prod_nodes_initialize,
    na_rm ? prod_na_rm_aggregate_from_leaves : prod_na_keep_aggregate_from_leaves,
    na_rm ? prod_na_rm_aggregate_from_nodes : prod_na_keep_aggregate_from_nodes
  );
  PROTECT_SEGMENT_TREE(&tree, &n_prot);

  slide_index_summary_loop(
    &tree,
    iter_min,
    iter_max,
    range,
    window_sizes,
    window_starts,
    window_stops,
    indices,
    p_index,
    p_out
  );

  UNPROTECT(n_prot);
}

static SEXP slide_index_prod_core(SEXP x,
                                  SEXP i,
                                  SEXP starts,
                                  SEXP stops,
                                  SEXP indices,
                                  bool complete,
                                  bool na_rm) {
  return slide_index_summary(
    x,
    i,
    starts,
    stops,
    indices,
    complete,
    na_rm,
    slider_index_prod_core_impl
  );
}

// [[ register() ]]
SEXP slider_index_prod_core(SEXP x,
                            SEXP i,
                            SEXP starts,
                            SEXP stops,
                            SEXP indices,
                            SEXP complete,
                            SEXP na_rm) {
  return slider_index_summary(
    x,
    i,
    starts,
    stops,
    indices,
    complete,
    na_rm,
    slide_index_prod_core
  );
}

// -----------------------------------------------------------------------------

static void slider_index_mean_core_impl(const double* p_x,
                                        R_xlen_t size,
                                        int iter_min,
                                        int iter_max,
                                        const struct range_info range,
                                        const int* window_sizes,
                                        const int* window_starts,
                                        const int* window_stops,
                                        SEXP indices,
                                        bool na_rm,
                                        struct index_info* p_index,
                                        double* p_out) {
  int n_prot = 0;

  struct mean_state_t state = { .sum = 0, .count = 0 };

  struct segment_tree tree = new_segment_tree(
    size,
    p_x,
    &state,
    mean_state_reset,
    mean_state_finalize,
    mean_nodes_increment,
    mean_nodes_initialize,
    na_rm ? mean_na_rm_aggregate_from_leaves : mean_na_keep_aggregate_from_leaves,
    na_rm ? mean_na_rm_aggregate_from_nodes : mean_na_keep_aggregate_from_nodes
  );
  PROTECT_SEGMENT_TREE(&tree, &n_prot);

  slide_index_summary_loop(
    &tree,
    iter_min,
    iter_max,
    range,
    window_sizes,
    window_starts,
    window_stops,
    indices,
    p_index,
    p_out
  );

  UNPROTECT(n_prot);
}

static SEXP slide_index_mean_core(SEXP x,
                                  SEXP i,
                                  SEXP starts,
                                  SEXP stops,
                                  SEXP indices,
                                  bool complete,
                                  bool na_rm) {
  return slide_index_summary(
    x,
    i,
    starts,
    stops,
    indices,
    complete,
    na_rm,
    slider_index_mean_core_impl
  );
}

// [[ register() ]]
SEXP slider_index_mean_core(SEXP x,
                            SEXP i,
                            SEXP starts,
                            SEXP stops,
                            SEXP indices,
                            SEXP complete,
                            SEXP na_rm) {
  return slider_index_summary(
    x,
    i,
    starts,
    stops,
    indices,
    complete,
    na_rm,
    slide_index_mean_core
  );
}

// -----------------------------------------------------------------------------

static void slider_index_min_core_impl(const double* p_x,
                                       R_xlen_t size,
                                       int iter_min,
                                       int iter_max,
                                       const struct range_info range,
                                       const int* window_sizes,
                                       const int* window_starts,
                                       const int* window_stops,
                                       SEXP indices,
                                       bool na_rm,
                                       struct index_info* p_index,
                                       double* p_out) {
  int n_prot = 0;

  long double state = 1;

  struct segment_tree tree = new_segment_tree(
    size,
    p_x,
    &state,
    min_state_reset,
    min_state_finalize,
    min_nodes_increment,
    min_nodes_initialize,
    na_rm ? min_na_rm_aggregate_from_leaves : min_na_keep_aggregate_from_leaves,
    na_rm ? min_na_rm_aggregate_from_nodes : min_na_keep_aggregate_from_nodes
  );
  PROTECT_SEGMENT_TREE(&tree, &n_prot);

  slide_index_summary_loop(
    &tree,
    iter_min,
    iter_max,
    range,
    window_sizes,
    window_starts,
    window_stops,
    indices,
    p_index,
    p_out
  );

  UNPROTECT(n_prot);
}

static SEXP slide_index_min_core(SEXP x,
                                 SEXP i,
                                 SEXP starts,
                                 SEXP stops,
                                 SEXP indices,
                                 bool complete,
                                 bool na_rm) {
  return slide_index_summary(
    x,
    i,
    starts,
    stops,
    indices,
    complete,
    na_rm,
    slider_index_min_core_impl
  );
}

// [[ register() ]]
SEXP slider_index_min_core(SEXP x,
                           SEXP i,
                           SEXP starts,
                           SEXP stops,
                           SEXP indices,
                           SEXP complete,
                           SEXP na_rm) {
  return slider_index_summary(
    x,
    i,
    starts,
    stops,
    indices,
    complete,
    na_rm,
    slide_index_min_core
  );
}

// -----------------------------------------------------------------------------

static void slider_index_max_core_impl(const double* p_x,
                                       R_xlen_t size,
                                       int iter_min,
                                       int iter_max,
                                       const struct range_info range,
                                       const int* window_sizes,
                                       const int* window_starts,
                                       const int* window_stops,
                                       SEXP indices,
                                       bool na_rm,
                                       struct index_info* p_index,
                                       double* p_out) {
  int n_prot = 0;

  long double state = 1;

  struct segment_tree tree = new_segment_tree(
    size,
    p_x,
    &state,
    max_state_reset,
    max_state_finalize,
    max_nodes_increment,
    max_nodes_initialize,
    na_rm ? max_na_rm_aggregate_from_leaves : max_na_keep_aggregate_from_leaves,
    na_rm ? max_na_rm_aggregate_from_nodes : max_na_keep_aggregate_from_nodes
  );
  PROTECT_SEGMENT_TREE(&tree, &n_prot);

  slide_index_summary_loop(
    &tree,
    iter_min,
    iter_max,
    range,
    window_sizes,
    window_starts,
    window_stops,
    indices,
    p_index,
    p_out
  );

  UNPROTECT(n_prot);
}

static SEXP slide_index_max_core(SEXP x,
                                 SEXP i,
                                 SEXP starts,
                                 SEXP stops,
                                 SEXP indices,
                                 bool complete,
                                 bool na_rm) {
  return slide_index_summary(
    x,
    i,
    starts,
    stops,
    indices,
    complete,
    na_rm,
    slider_index_max_core_impl
  );
}

// [[ register() ]]
SEXP slider_index_max_core(SEXP x,
                           SEXP i,
                           SEXP starts,
                           SEXP stops,
                           SEXP indices,
                           SEXP complete,
                           SEXP na_rm) {
  return slider_index_summary(
    x,
    i,
    starts,
    stops,
    indices,
    complete,
    na_rm,
    slide_index_max_core
  );
}

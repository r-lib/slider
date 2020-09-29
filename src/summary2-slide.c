#include "slider.h"
#include "slider-vctrs.h"
#include "opts-slide.h"
#include "utils.h"
#include "segment-tree.h"

// -----------------------------------------------------------------------------

void sum_state_reset(void* p_state) {
  double* p_state_ = (double*) p_state;
  *p_state_ = 0;
}

void sum_state_finalize(void* p_state) {
  return;
}

void* sum_nodes_increment(void* p_nodes) {
  return (void*) (((double*) p_nodes) + 1);
}

SEXP sum_nodes_initialize(uint64_t n) {
  SEXP nodes = PROTECT(Rf_allocVector(REALSXP, n));
  double* p_nodes = REAL(nodes);

  for (uint64_t i = 0; i < n; ++i) {
    p_nodes[i] = 0;
  }

  UNPROTECT(1);
  return nodes;
}

void sum_aggregate_from_leaves(const void* p_source, uint64_t begin, uint64_t end, void* p_dest) {
  const double* p_source_ = (const double*) p_source;
  double* p_dest_ = (double*) p_dest;

  for (uint64_t i = begin; i < end; ++i) {
    *p_dest_ += p_source_[i];
  }
}

// For sum, nodes are the same type as the leaves
void sum_aggregate_from_nodes(const void* p_source, uint64_t begin, uint64_t end, void* p_dest) {
  sum_aggregate_from_leaves(p_source, begin, end, p_dest);
}

// -----------------------------------------------------------------------------

static SEXP slide_sum(SEXP x, struct slide_opts opts, bool na_rm);

// [[ register() ]]
SEXP slider_sum2(SEXP x, SEXP before, SEXP after, SEXP step, SEXP complete, SEXP na_rm) {
  bool dot = false;
  struct slide_opts opts = new_slide_opts(before, after, step, complete, dot);
  bool c_na_rm = validate_na_rm(na_rm, dot);
  return slide_sum(x, opts, c_na_rm);
}

static inline void slide_sum_na_keep(SEXP x, struct iter_opts opts, double* p_out);

static SEXP slide_sum(SEXP x, struct slide_opts opts, bool na_rm) {
  // Before `vec_cast()`, which may drop names
  SEXP names = PROTECT(slider_names(x, SLIDE));

  x = PROTECT(vec_cast(x, slider_shared_empty_dbl));

  const R_xlen_t size = Rf_xlength(x);
  struct iter_opts iopts = new_iter_opts(opts, size);

  SEXP out = PROTECT(slider_init(REALSXP, size));
  double* p_out = REAL(out);
  Rf_setAttrib(out, R_NamesSymbol, names);

  slide_sum_na_keep(x, iopts, p_out);

  UNPROTECT(3);
  return out;
}

static inline void slide_sum_na_keep(SEXP x, struct iter_opts opts, double* p_out) {
  int n_prot = 0;

  struct segment_tree tree = new_segment_tree(
    x,
    sum_state_reset,
    sum_state_finalize,
    sum_nodes_increment,
    sum_nodes_initialize,
    sum_aggregate_from_leaves,
    sum_aggregate_from_nodes
  );
  PROTECT_SEGMENT_TREE(&tree, &n_prot);

  double state = 0;

  for (R_xlen_t i = opts.iter_min; i < opts.iter_max; i += opts.iter_step) {
    if (i % 1024 == 0) {
      R_CheckUserInterrupt();
    }

    R_xlen_t window_start = max(opts.start, 0);
    R_xlen_t window_stop = min(opts.stop + 1, opts.size);

    opts.start += opts.start_step;
    opts.stop += opts.stop_step;

    segment_tree_aggregate(&tree, window_start, window_stop, &state);

    p_out[i] = state;
  }

  UNPROTECT(n_prot);
}






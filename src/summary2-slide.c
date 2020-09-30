#include "slider.h"
#include "slider-vctrs.h"
#include "opts-slide.h"
#include "utils.h"
#include "segment-tree.h"
#include "summary2-core.h"

// -----------------------------------------------------------------------------

static SEXP slider_summary(SEXP x,
                           SEXP before,
                           SEXP after,
                           SEXP step,
                           SEXP complete,
                           SEXP na_rm,
                           SEXP (*fn)(SEXP x, struct slide_opts opts, bool na_rm)) {
  bool dot = false;
  struct slide_opts opts = new_slide_opts(before, after, step, complete, dot);
  bool c_na_rm = validate_na_rm(na_rm, dot);
  return fn(x, opts, c_na_rm);
}

static SEXP slide_summary(SEXP x,
                          struct slide_opts opts,
                          bool na_rm,
                          void (*fn)(const double* p_x, R_xlen_t size, const struct iter_opts* p_opts, bool na_rm, double* p_out)) {
  // Before `vec_cast()`, which may drop names
  SEXP names = PROTECT(slider_names(x, SLIDE));

  x = PROTECT(vec_cast(x, slider_shared_empty_dbl));
  const double* p_x = REAL(x);

  const R_xlen_t size = Rf_xlength(x);
  const struct iter_opts iopts = new_iter_opts(opts, size);

  SEXP out = PROTECT(slider_init(REALSXP, size));
  double* p_out = REAL(out);
  Rf_setAttrib(out, R_NamesSymbol, names);

  fn(p_x, size, &iopts, na_rm, p_out);

  UNPROTECT(3);
  return out;
}

static inline void slide_summary_loop(const struct segment_tree* p_tree,
                                      const struct iter_opts* p_opts,
                                      double* p_out) {
  double result = 0;

  R_xlen_t iter_min = p_opts->iter_min;
  R_xlen_t iter_max = p_opts->iter_max;
  R_xlen_t iter_step = p_opts->iter_step;

  R_xlen_t start = p_opts->start;
  R_xlen_t stop = p_opts->stop;

  for (R_xlen_t i = iter_min; i < iter_max; i += iter_step) {
    if (i % 1024 == 0) {
      R_CheckUserInterrupt();
    }

    R_xlen_t window_start = max(start, 0);
    R_xlen_t window_stop = min(stop + 1, p_opts->size);

    start += p_opts->start_step;
    stop += p_opts->stop_step;

    segment_tree_aggregate(p_tree, window_start, window_stop, &result);

    p_out[i] = result;
  }
}

// -----------------------------------------------------------------------------

static SEXP slide_sum(SEXP x, struct slide_opts opts, bool na_rm);

// [[ register() ]]
SEXP slider_sum2(SEXP x, SEXP before, SEXP after, SEXP step, SEXP complete, SEXP na_rm) {
  return slider_summary(x, before, after, step, complete, na_rm, slide_sum);
}

static inline void slide_sum_impl(const double* p_x, R_xlen_t size, const struct iter_opts* p_opts, bool na_rm, double* p_out);

static SEXP slide_sum(SEXP x, struct slide_opts opts, bool na_rm) {
  return slide_summary(x, opts, na_rm, slide_sum_impl);
}

static inline void slide_sum_impl(const double* p_x, R_xlen_t size, const struct iter_opts* p_opts, bool na_rm, double* p_out) {
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

  slide_summary_loop(&tree, p_opts, p_out);

  UNPROTECT(n_prot);
}

// -----------------------------------------------------------------------------

static SEXP slide_prod(SEXP x, struct slide_opts opts, bool na_rm);

// [[ register() ]]
SEXP slider_prod2(SEXP x, SEXP before, SEXP after, SEXP step, SEXP complete, SEXP na_rm) {
  return slider_summary(x, before, after, step, complete, na_rm, slide_prod);
}

static inline void slide_prod_impl(const double* p_x, R_xlen_t size, const struct iter_opts* p_opts, bool na_rm, double* p_out);

static SEXP slide_prod(SEXP x, struct slide_opts opts, bool na_rm) {
  return slide_summary(x, opts, na_rm, slide_prod_impl);
}

static inline void slide_prod_impl(const double* p_x, R_xlen_t size, const struct iter_opts* p_opts, bool na_rm, double* p_out) {
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

  slide_summary_loop(&tree, p_opts, p_out);

  UNPROTECT(n_prot);
}

// -----------------------------------------------------------------------------

static SEXP slide_mean(SEXP x, struct slide_opts opts, bool na_rm);

// [[ register() ]]
SEXP slider_mean2(SEXP x, SEXP before, SEXP after, SEXP step, SEXP complete, SEXP na_rm) {
  return slider_summary(x, before, after, step, complete, na_rm, slide_mean);
}

static inline void slide_mean_impl(const double* p_x, R_xlen_t size, const struct iter_opts* p_opts, bool na_rm, double* p_out);

static SEXP slide_mean(SEXP x, struct slide_opts opts, bool na_rm) {
  return slide_summary(x, opts, na_rm, slide_mean_impl);
}

static inline void slide_mean_impl(const double* p_x, R_xlen_t size, const struct iter_opts* p_opts, bool na_rm, double* p_out) {
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

  slide_summary_loop(&tree, p_opts, p_out);

  UNPROTECT(n_prot);
}

// -----------------------------------------------------------------------------

static SEXP slide_min(SEXP x, struct slide_opts opts, bool na_rm);

// [[ register() ]]
SEXP slider_min2(SEXP x, SEXP before, SEXP after, SEXP step, SEXP complete, SEXP na_rm) {
  return slider_summary(x, before, after, step, complete, na_rm, slide_min);
}

static inline void slide_min_impl(const double* p_x, R_xlen_t size, const struct iter_opts* p_opts, bool na_rm, double* p_out);

static SEXP slide_min(SEXP x, struct slide_opts opts, bool na_rm) {
  return slide_summary(x, opts, na_rm, slide_min_impl);
}

static inline void slide_min_impl(const double* p_x, R_xlen_t size, const struct iter_opts* p_opts, bool na_rm, double* p_out) {
  int n_prot = 0;

  double state = R_PosInf;

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

  slide_summary_loop(&tree, p_opts, p_out);

  UNPROTECT(n_prot);
}

// -----------------------------------------------------------------------------

static SEXP slide_max(SEXP x, struct slide_opts opts, bool na_rm);

// [[ register() ]]
SEXP slider_max2(SEXP x, SEXP before, SEXP after, SEXP step, SEXP complete, SEXP na_rm) {
  return slider_summary(x, before, after, step, complete, na_rm, slide_max);
}

static inline void slide_max_impl(const double* p_x, R_xlen_t size, const struct iter_opts* p_opts, bool na_rm, double* p_out);

static SEXP slide_max(SEXP x, struct slide_opts opts, bool na_rm) {
  return slide_summary(x, opts, na_rm, slide_max_impl);
}

static inline void slide_max_impl(const double* p_x, R_xlen_t size, const struct iter_opts* p_opts, bool na_rm, double* p_out) {
  int n_prot = 0;

  double state = R_NegInf;

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

  slide_summary_loop(&tree, p_opts, p_out);

  UNPROTECT(n_prot);
}

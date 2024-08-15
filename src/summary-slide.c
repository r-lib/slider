#include "slider.h"
#include "slider-vctrs.h"
#include "opts-slide.h"
#include "utils.h"
#include "segment-tree.h"
#include "summary-core.h"

// -----------------------------------------------------------------------------

typedef SEXP (*summary_fn)(SEXP x, struct slide_opts opts, bool na_rm);

static SEXP slider_summary(SEXP x,
                           SEXP before,
                           SEXP after,
                           SEXP step,
                           SEXP complete,
                           SEXP na_rm,
                           summary_fn fn) {
  bool dot = false;
  struct slide_opts opts = new_slide_opts(before, after, step, complete, dot);
  bool c_na_rm = validate_na_rm(na_rm, dot);
  return fn(x, opts, c_na_rm);
}

// -----------------------------------------------------------------------------

typedef void (*summary_impl_dbl_fn)(const double* p_x,
                                    R_xlen_t size,
                                    const struct iter_opts* p_opts,
                                    bool na_rm,
                                    double* p_out);

typedef void (*summary_impl_lgl_fn)(const int* p_x,
                                    R_xlen_t size,
                                    const struct iter_opts* p_opts,
                                    bool na_rm,
                                    int* p_out);

#define SLIDE_SUMMARY(PTYPE, CTYPE, SEXPTYPE, CONST_DEREF, DEREF) do { \
  /* Before `vec_cast()`, which may drop names */                      \
  SEXP names = PROTECT(slider_names(x, SLIDE));                        \
                                                                       \
  x = PROTECT(vec_cast(x, PTYPE));                                     \
  const CTYPE* p_x = CONST_DEREF(x);                                   \
                                                                       \
  const R_xlen_t size = Rf_xlength(x);                                 \
  const struct iter_opts iopts = new_iter_opts(opts, size);            \
                                                                       \
  SEXP out = PROTECT(slider_init(SEXPTYPE, size));                     \
  CTYPE* p_out = DEREF(out);                                           \
  Rf_setAttrib(out, R_NamesSymbol, names);                             \
                                                                       \
  fn(p_x, size, &iopts, na_rm, p_out);                                 \
                                                                       \
  UNPROTECT(3);                                                        \
  return out;                                                          \
} while (0)


static SEXP slide_summary_dbl(SEXP x,
                              struct slide_opts opts,
                              bool na_rm,
                              summary_impl_dbl_fn fn) {
  SLIDE_SUMMARY(slider_shared_empty_dbl, double, REALSXP, REAL_RO, REAL);
}

static SEXP slide_summary_lgl(SEXP x,
                              struct slide_opts opts,
                              bool na_rm,
                              summary_impl_lgl_fn fn) {
  SLIDE_SUMMARY(slider_shared_empty_lgl, int, LGLSXP, LOGICAL_RO, LOGICAL);
}

#undef SLIDE_SUMMARY

// -----------------------------------------------------------------------------

#define SLIDE_SUMMARY_LOOP(CTYPE, INIT) do {                   \
  R_xlen_t iter_min = p_opts->iter_min;                        \
  R_xlen_t iter_max = p_opts->iter_max;                        \
  R_xlen_t iter_step = p_opts->iter_step;                      \
                                                               \
  R_xlen_t start = p_opts->start;                              \
  R_xlen_t stop = p_opts->stop;                                \
                                                               \
  R_xlen_t start_step = p_opts->start_step;                    \
  R_xlen_t stop_step = p_opts->stop_step;                      \
                                                               \
  for (R_xlen_t i = iter_min; i < iter_max; i += iter_step) {  \
    if (i % 1024 == 0) {                                       \
      R_CheckUserInterrupt();                                  \
    }                                                          \
                                                               \
    R_xlen_t window_start = max_size(start, 0);                \
    R_xlen_t window_stop = min_size(stop + 1, p_opts->size);   \
                                                               \
    /* Happens when the entire window is OOB */                \
    /* essentially take a 0-slice */                           \
    if (window_stop < window_start) {                          \
      window_start = 0;                                        \
      window_stop = 0;                                         \
    }                                                          \
                                                               \
    start += start_step;                                       \
    stop += stop_step;                                         \
                                                               \
    CTYPE result = INIT;                                       \
                                                               \
    segment_tree_aggregate(                                    \
      p_tree,                                                  \
      window_start,                                            \
      window_stop,                                             \
      &result                                                  \
    );                                                         \
                                                               \
    p_out[i] = result;                                         \
  }                                                            \
} while (0)


static inline void slide_summary_loop_dbl(const struct segment_tree* p_tree,
                                          const struct iter_opts* p_opts,
                                          double* p_out) {
  SLIDE_SUMMARY_LOOP(double, 0);
}

static inline void slide_summary_loop_lgl(const struct segment_tree* p_tree,
                                          const struct iter_opts* p_opts,
                                          int* p_out) {
  SLIDE_SUMMARY_LOOP(int, 0);
}

#undef SLIDE_SUMMARY_LOOP

// -----------------------------------------------------------------------------

static inline void slide_sum_impl(const double* p_x,
                                  R_xlen_t size,
                                  const struct iter_opts* p_opts,
                                  bool na_rm,
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
    sum_nodes_void_deref,
    na_rm ? sum_na_rm_aggregate_from_leaves : sum_na_keep_aggregate_from_leaves,
    na_rm ? sum_na_rm_aggregate_from_nodes : sum_na_keep_aggregate_from_nodes
  );
  PROTECT_SEGMENT_TREE(&tree, &n_prot);

  slide_summary_loop_dbl(&tree, p_opts, p_out);

  UNPROTECT(n_prot);
}

static SEXP slide_sum(SEXP x, struct slide_opts opts, bool na_rm) {
  return slide_summary_dbl(x, opts, na_rm, slide_sum_impl);
}

// [[ register() ]]
SEXP slider_sum(SEXP x, SEXP before, SEXP after, SEXP step, SEXP complete, SEXP na_rm) {
  return slider_summary(x, before, after, step, complete, na_rm, slide_sum);
}

// -----------------------------------------------------------------------------

static inline void slide_prod_impl(const double* p_x,
                                   R_xlen_t size,
                                   const struct iter_opts* p_opts,
                                   bool na_rm,
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
    prod_nodes_void_deref,
    na_rm ? prod_na_rm_aggregate_from_leaves : prod_na_keep_aggregate_from_leaves,
    na_rm ? prod_na_rm_aggregate_from_nodes : prod_na_keep_aggregate_from_nodes
  );
  PROTECT_SEGMENT_TREE(&tree, &n_prot);

  slide_summary_loop_dbl(&tree, p_opts, p_out);

  UNPROTECT(n_prot);
}

static SEXP slide_prod(SEXP x, struct slide_opts opts, bool na_rm) {
  return slide_summary_dbl(x, opts, na_rm, slide_prod_impl);
}

// [[ register() ]]
SEXP slider_prod(SEXP x, SEXP before, SEXP after, SEXP step, SEXP complete, SEXP na_rm) {
  return slider_summary(x, before, after, step, complete, na_rm, slide_prod);
}

// -----------------------------------------------------------------------------

static inline void slide_mean_impl(const double* p_x,
                                   R_xlen_t size,
                                   const struct iter_opts* p_opts,
                                   bool na_rm,
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
    mean_nodes_void_deref,
    na_rm ? mean_na_rm_aggregate_from_leaves : mean_na_keep_aggregate_from_leaves,
    na_rm ? mean_na_rm_aggregate_from_nodes : mean_na_keep_aggregate_from_nodes
  );
  PROTECT_SEGMENT_TREE(&tree, &n_prot);

  slide_summary_loop_dbl(&tree, p_opts, p_out);

  UNPROTECT(n_prot);
}

static SEXP slide_mean(SEXP x, struct slide_opts opts, bool na_rm) {
  return slide_summary_dbl(x, opts, na_rm, slide_mean_impl);
}

// [[ register() ]]
SEXP slider_mean(SEXP x, SEXP before, SEXP after, SEXP step, SEXP complete, SEXP na_rm) {
  return slider_summary(x, before, after, step, complete, na_rm, slide_mean);
}

// -----------------------------------------------------------------------------

static inline void slide_min_impl(const double* p_x,
                                  R_xlen_t size,
                                  const struct iter_opts* p_opts,
                                  bool na_rm,
                                  double* p_out) {
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
    min_nodes_void_deref,
    na_rm ? min_na_rm_aggregate_from_leaves : min_na_keep_aggregate_from_leaves,
    na_rm ? min_na_rm_aggregate_from_nodes : min_na_keep_aggregate_from_nodes
  );
  PROTECT_SEGMENT_TREE(&tree, &n_prot);

  slide_summary_loop_dbl(&tree, p_opts, p_out);

  UNPROTECT(n_prot);
}

static SEXP slide_min(SEXP x, struct slide_opts opts, bool na_rm) {
  return slide_summary_dbl(x, opts, na_rm, slide_min_impl);
}

// [[ register() ]]
SEXP slider_min(SEXP x, SEXP before, SEXP after, SEXP step, SEXP complete, SEXP na_rm) {
  return slider_summary(x, before, after, step, complete, na_rm, slide_min);
}

// -----------------------------------------------------------------------------

static inline void slide_max_impl(const double* p_x,
                                  R_xlen_t size,
                                  const struct iter_opts* p_opts,
                                  bool na_rm,
                                  double* p_out) {
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
    max_nodes_void_deref,
    na_rm ? max_na_rm_aggregate_from_leaves : max_na_keep_aggregate_from_leaves,
    na_rm ? max_na_rm_aggregate_from_nodes : max_na_keep_aggregate_from_nodes
  );
  PROTECT_SEGMENT_TREE(&tree, &n_prot);

  slide_summary_loop_dbl(&tree, p_opts, p_out);

  UNPROTECT(n_prot);
}

static SEXP slide_max(SEXP x, struct slide_opts opts, bool na_rm) {
  return slide_summary_dbl(x, opts, na_rm, slide_max_impl);
}

// [[ register() ]]
SEXP slider_max(SEXP x, SEXP before, SEXP after, SEXP step, SEXP complete, SEXP na_rm) {
  return slider_summary(x, before, after, step, complete, na_rm, slide_max);
}

// -----------------------------------------------------------------------------

static inline void slide_all_impl(const int* p_x,
                                  R_xlen_t size,
                                  const struct iter_opts* p_opts,
                                  bool na_rm,
                                  int* p_out) {
  int n_prot = 0;

  int state = 1;

  struct segment_tree tree = new_segment_tree(
    size,
    p_x,
    &state,
    all_state_reset,
    all_state_finalize,
    all_nodes_increment,
    all_nodes_initialize,
    all_nodes_void_deref,
    na_rm ? all_na_rm_aggregate_from_leaves : all_na_keep_aggregate_from_leaves,
    na_rm ? all_na_rm_aggregate_from_nodes : all_na_keep_aggregate_from_nodes
  );
  PROTECT_SEGMENT_TREE(&tree, &n_prot);

  slide_summary_loop_lgl(&tree, p_opts, p_out);

  UNPROTECT(n_prot);
}

static SEXP slide_all(SEXP x, struct slide_opts opts, bool na_rm) {
  return slide_summary_lgl(x, opts, na_rm, slide_all_impl);
}

// [[ register() ]]
SEXP slider_all(SEXP x, SEXP before, SEXP after, SEXP step, SEXP complete, SEXP na_rm) {
  return slider_summary(x, before, after, step, complete, na_rm, slide_all);
}

// -----------------------------------------------------------------------------

static inline void slide_any_impl(const int* p_x,
                                  R_xlen_t size,
                                  const struct iter_opts* p_opts,
                                  bool na_rm,
                                  int* p_out) {
  int n_prot = 0;

  int state = 0;

  struct segment_tree tree = new_segment_tree(
    size,
    p_x,
    &state,
    any_state_reset,
    any_state_finalize,
    any_nodes_increment,
    any_nodes_initialize,
    any_nodes_void_deref,
    na_rm ? any_na_rm_aggregate_from_leaves : any_na_keep_aggregate_from_leaves,
    na_rm ? any_na_rm_aggregate_from_nodes : any_na_keep_aggregate_from_nodes
  );
  PROTECT_SEGMENT_TREE(&tree, &n_prot);

  slide_summary_loop_lgl(&tree, p_opts, p_out);

  UNPROTECT(n_prot);
}

static SEXP slide_any(SEXP x, struct slide_opts opts, bool na_rm) {
  return slide_summary_lgl(x, opts, na_rm, slide_any_impl);
}

// [[ register() ]]
SEXP slider_any(SEXP x, SEXP before, SEXP after, SEXP step, SEXP complete, SEXP na_rm) {
  return slider_summary(x, before, after, step, complete, na_rm, slide_any);
}

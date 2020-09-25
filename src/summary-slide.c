#include "slider.h"
#include "slider-vctrs.h"
#include "opts-slide.h"
#include "utils.h"

// -----------------------------------------------------------------------------

typedef SEXP (*summary_fn)(SEXP x, struct slide_opts opts, bool na_rm);
typedef void (*summary_impl_fn)(const double* p_x, struct iter_opts opts, double* p_out);

static SEXP slider_summary(SEXP x,
                           SEXP before,
                           SEXP after,
                           SEXP step,
                           SEXP complete,
                           SEXP na_rm, summary_fn fn) {
  bool dot = false;
  struct slide_opts opts = new_slide_opts(before, after, step, complete, dot);
  bool c_na_rm = validate_na_rm(na_rm, dot);
  return fn(x, opts, c_na_rm);
}

static SEXP slide_summary(SEXP x,
                          struct slide_opts opts,
                          bool na_rm,
                          summary_impl_fn fn_na_keep,
                          summary_impl_fn fn_na_rm) {
  // Before `vec_cast()`, which may drop names
  SEXP names = PROTECT(slider_names(x, SLIDE));

  x = PROTECT(vec_cast(x, slider_shared_empty_dbl));
  const double* p_x = REAL(x);

  const R_xlen_t size = Rf_xlength(x);
  struct iter_opts iopts = new_iter_opts(opts, size);

  SEXP out = PROTECT(slider_init(REALSXP, size));
  double* p_out = REAL(out);
  Rf_setAttrib(out, R_NamesSymbol, names);

  if (na_rm) {
    fn_na_rm(p_x, iopts, p_out);
  } else {
    fn_na_keep(p_x, iopts, p_out);
  }

  UNPROTECT(3);
  return out;
}

// -----------------------------------------------------------------------------

#define SLIDE_SUMMARY_LOOP(IMPL) do {                                        \
  for (R_xlen_t i = opts.iter_min; i < opts.iter_max; i += opts.iter_step) { \
    if (i % 1024 == 0) {                                                     \
      R_CheckUserInterrupt();                                                \
    }                                                                        \
                                                                             \
    R_xlen_t window_start = max(opts.start, 0);                              \
    R_xlen_t window_stop = min(opts.stop + 1, opts.size);                    \
                                                                             \
    opts.start += opts.start_step;                                           \
    opts.stop += opts.stop_step;                                             \
                                                                             \
    IMPL;                                                                    \
  }                                                                          \
} while (0)

// -----------------------------------------------------------------------------

static SEXP slide_sum(SEXP x, struct slide_opts opts, bool na_rm);

// [[ register() ]]
SEXP slider_sum(SEXP x, SEXP before, SEXP after, SEXP step, SEXP complete, SEXP na_rm) {
  return slider_summary(x, before, after, step, complete, na_rm, slide_sum);
}

static inline void slide_sum_na_keep(const double* p_x, struct iter_opts opts, double* p_out);
static inline void slide_sum_na_rm(const double* p_x, struct iter_opts opts, double* p_out);

static SEXP slide_sum(SEXP x, struct slide_opts opts, bool na_rm) {
  return slide_summary(x, opts, na_rm, slide_sum_na_keep, slide_sum_na_rm);
}

#define SUM_IMPL_NA_KEEP {                                     \
  double val = 0.0;                                            \
                                                               \
  for (R_xlen_t j = window_start; j < window_stop; ++j) {      \
    val += p_x[j];                                             \
  }                                                            \
                                                               \
  p_out[i] = val;                                              \
}

#define SUM_IMPL_NA_RM {                                       \
  double val = 0.0;                                            \
                                                               \
  for (R_xlen_t j = window_start; j < window_stop; ++j) {      \
    const double elt = p_x[j];                                 \
                                                               \
    if (!isnan(elt)) {                                         \
      val += elt;                                              \
    }                                                          \
  }                                                            \
                                                               \
  p_out[i] = val;                                              \
}

static inline void slide_sum_na_keep(const double* p_x, struct iter_opts opts, double* p_out) {
  SLIDE_SUMMARY_LOOP(SUM_IMPL_NA_KEEP);
}
static inline void slide_sum_na_rm(const double* p_x, struct iter_opts opts, double* p_out) {
  SLIDE_SUMMARY_LOOP(SUM_IMPL_NA_RM);
}

#undef SUM_IMPL_NA_KEEP
#undef SUM_IMPL_NA_RM

// -----------------------------------------------------------------------------

static SEXP slide_mean(SEXP x, struct slide_opts opts, bool na_rm);

// [[ register() ]]
SEXP slider_mean(SEXP x, SEXP before, SEXP after, SEXP step, SEXP complete, SEXP na_rm) {
  return slider_summary(x, before, after, step, complete, na_rm, slide_mean);
}

static inline void slide_mean_na_keep(const double* p_x, struct iter_opts opts, double* p_out);
static inline void slide_mean_na_rm(const double* p_x, struct iter_opts opts, double* p_out);

static SEXP slide_mean(SEXP x, struct slide_opts opts, bool na_rm) {
  return slide_summary(x, opts, na_rm, slide_mean_na_keep, slide_mean_na_rm);
}

#define MEAN_IMPL_NA_KEEP {                                    \
  long double val = 0.0;                                       \
  R_xlen_t window_size = window_stop - window_start;           \
                                                               \
  for (R_xlen_t j = window_start; j < window_stop; ++j) {      \
    val += p_x[j];                                             \
  }                                                            \
  val /= window_size;                                          \
                                                               \
  /* No second pass required if known to be NA/NaN/Inf/-Inf */ \
  if (!R_FINITE((double) val)) {                               \
    p_out[i] = (double) val;                                   \
    continue;                                                  \
  }                                                            \
                                                               \
  long double adjustment = 0.0;                                \
                                                               \
  for (R_xlen_t j = window_start; j < window_stop; ++j) {      \
    adjustment += (p_x[j] - val);                              \
  }                                                            \
  adjustment /= window_size;                                   \
                                                               \
  val += adjustment;                                           \
                                                               \
  p_out[i] = (double) val;                                     \
}

#define MEAN_IMPL_NA_RM {                                        \
  long double val = 0.0;                                         \
  R_xlen_t window_size = 0;                                      \
                                                                 \
  for (R_xlen_t j = window_start; j < window_stop; ++j) {        \
    double elt = p_x[j];                                         \
                                                                 \
    if (!isnan(elt)) {                                           \
      val += elt;                                                \
      ++window_size;                                             \
    }                                                            \
  }                                                              \
  val /= window_size;                                            \
                                                                 \
  /* No second pass required if known to be NA/NaN/Inf/-Inf */   \
  if (!R_FINITE((double) val)) {                                 \
    p_out[i] = (double) val;                                     \
    continue;                                                    \
  }                                                              \
                                                                 \
  long double adjustment = 0.0;                                  \
                                                                 \
  for (R_xlen_t j = window_start; j < window_stop; ++j) {        \
    double elt = p_x[j];                                         \
                                                                 \
    if (!isnan(elt)) {                                           \
      adjustment += (elt - val);                                 \
    }                                                            \
  }                                                              \
  adjustment /= window_size;                                     \
                                                                 \
  val += adjustment;                                             \
                                                                 \
  p_out[i] = (double) val;                                       \
}

static inline void slide_mean_na_keep(const double* p_x, struct iter_opts opts, double* p_out) {
  SLIDE_SUMMARY_LOOP(MEAN_IMPL_NA_KEEP);
}
static inline void slide_mean_na_rm(const double* p_x, struct iter_opts opts, double* p_out) {
  SLIDE_SUMMARY_LOOP(MEAN_IMPL_NA_RM);
}

#undef MEAN_IMPL_NA_KEEP
#undef MEAN_IMPL_NA_RM

// -----------------------------------------------------------------------------

#undef SLIDE_SUMMARY_LOOP

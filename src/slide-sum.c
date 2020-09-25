#include "slider.h"
#include "slider-vctrs.h"
#include "opts-slide.h"
#include "utils.h"

// -----------------------------------------------------------------------------

static SEXP slide_sum(SEXP x, struct slide_opts opts, bool na_rm);

// [[ register() ]]
SEXP slider_sum(SEXP x, SEXP before, SEXP after, SEXP step, SEXP complete, SEXP na_rm) {
  bool dot = false;
  struct slide_opts opts = new_slide_opts(before, after, step, complete, dot);
  bool c_na_rm = validate_na_rm(na_rm, dot);
  return slide_sum(x, opts, c_na_rm);
}

static inline void slide_sum_na_keep(const double* p_x, struct iter_opts opts, double* p_out);
static inline void slide_sum_na_rm(const double* p_x, struct iter_opts opts, double* p_out);

static SEXP slide_sum(SEXP x, struct slide_opts opts, bool na_rm) {
  x = PROTECT(vec_cast(x, slider_shared_empty_dbl));
  const double* p_x = REAL(x);

  const R_xlen_t size = Rf_xlength(x);
  struct iter_opts iopts = new_iter_opts(opts, size);

  SEXP out = PROTECT(slider_init(REALSXP, size));
  double* p_out = REAL(out);

  if (na_rm) {
    slide_sum_na_rm(p_x, iopts, p_out);
  } else {
    slide_sum_na_keep(p_x, iopts, p_out);
  }

  UNPROTECT(2);
  return out;
}

static inline void slide_sum_na_keep(const double* p_x, struct iter_opts opts, double* p_out) {
  for (R_xlen_t i = opts.iter_min; i < opts.iter_max; i += opts.iter_step) {
    if (i % 1024 == 0) {
      R_CheckUserInterrupt();
    }

    R_xlen_t window_start = max(opts.start, 0);
    R_xlen_t window_stop = min(opts.stop + 1, opts.size);

    opts.start += opts.start_step;
    opts.stop += opts.stop_step;

    double val = 0.0;

    for (R_xlen_t j = window_start; j < window_stop; ++j) {
      val += p_x[j];
    }

    p_out[i] = val;
  }
}

static inline void slide_sum_na_rm(const double* p_x, struct iter_opts opts, double* p_out) {
  for (R_xlen_t i = opts.iter_min; i < opts.iter_max; i += opts.iter_step) {
    if (i % 1024 == 0) {
      R_CheckUserInterrupt();
    }

    R_xlen_t window_start = max(opts.start, 0);
    R_xlen_t window_stop = min(opts.stop + 1, opts.size);

    opts.start += opts.start_step;
    opts.stop += opts.stop_step;

    double val = 0.0;

    for (R_xlen_t j = window_start; j < window_stop; ++j) {
      const double elt = p_x[j];

      if (!isnan(elt)) {
        val += elt;
      }
    }

    p_out[i] = val;
  }
}

// -----------------------------------------------------------------------------

static SEXP slide_mean(SEXP x, struct slide_opts opts, bool na_rm);

// [[ register() ]]
SEXP slider_mean(SEXP x, SEXP before, SEXP after, SEXP step, SEXP complete, SEXP na_rm) {
  bool dot = false;
  struct slide_opts opts = new_slide_opts(before, after, step, complete, dot);
  bool c_na_rm = validate_na_rm(na_rm, dot);
  return slide_mean(x, opts, c_na_rm);
}

static inline void slide_mean_na_keep(const double* p_x, struct iter_opts opts, double* p_out);
static inline void slide_mean_na_rm(const double* p_x, struct iter_opts opts, double* p_out);

static SEXP slide_mean(SEXP x, struct slide_opts opts, bool na_rm) {
  x = PROTECT(vec_cast(x, slider_shared_empty_dbl));
  const double* p_x = REAL(x);

  const R_xlen_t size = Rf_xlength(x);
  struct iter_opts iopts = new_iter_opts(opts, size);

  SEXP out = PROTECT(slider_init(REALSXP, size));
  double* p_out = REAL(out);

  if (na_rm) {
    slide_mean_na_rm(p_x, iopts, p_out);
  } else {
    slide_mean_na_keep(p_x, iopts, p_out);
  }

  UNPROTECT(2);
  return out;
}

#define SLIDE_MEAN_LOOP(IMPL, ADJUST_IMPL) do {                               \
  for (R_xlen_t i = opts.iter_min; i < opts.iter_max; i += opts.iter_step) {  \
    if (i % 1024 == 0) {                                                      \
      R_CheckUserInterrupt();                                                 \
    }                                                                         \
                                                                              \
    R_xlen_t window_start = max(opts.start, 0);                               \
    R_xlen_t window_stop = min(opts.stop + 1, opts.size);                     \
                                                                              \
    opts.start += opts.start_step;                                            \
    opts.stop += opts.stop_step;                                              \
                                                                              \
    R_xlen_t n = 0;                                                           \
    long double val = 0.0;                                                    \
                                                                              \
    for (R_xlen_t j = window_start; j < window_stop; ++j) {                   \
      IMPL;                                                                   \
    }                                                                         \
                                                                              \
    val /= n;                                                                 \
                                                                              \
    /* No second pass required if known to be NA/NaN/Inf/-Inf */              \
    if (!R_FINITE((double) val)) {                                            \
      p_out[i] = (double) val;                                                \
      continue;                                                               \
    }                                                                         \
                                                                              \
    long double adjustment = 0.0;                                             \
                                                                              \
    for (R_xlen_t j = window_start; j < window_stop; ++j) {                   \
      ADJUST_IMPL;                                                            \
    }                                                                         \
                                                                              \
    adjustment /= n;                                                          \
                                                                              \
    val += adjustment;                                                        \
                                                                              \
    p_out[i] = (double) val;                                                  \
  }                                                                           \
} while (0)

#define SLIDE_MEAN_IMPL_NA_KEEP {                              \
  val += p_x[j];                                               \
  ++n;                                                         \
}

#define SLIDE_MEAN_IMPL_NA_RM {                                \
  double elt = p_x[j];                                         \
                                                               \
  if (!isnan(elt)) {                                           \
    val += elt;                                                \
    ++n;                                                       \
  }                                                            \
}

#define SLIDE_MEAN_ADJUST_IMPL_NA_KEEP {                       \
  adjustment += (p_x[j] - val);                                \
}

#define SLIDE_MEAN_ADJUST_IMPL_NA_RM {                         \
  double elt = p_x[j];                                         \
                                                               \
  if (!isnan(elt)) {                                           \
    adjustment += (elt - val);                                 \
  }                                                            \
}

static inline void slide_mean_na_keep(const double* p_x, struct iter_opts opts, double* p_out) {
  SLIDE_MEAN_LOOP(SLIDE_MEAN_IMPL_NA_KEEP, SLIDE_MEAN_ADJUST_IMPL_NA_KEEP);
}
static inline void slide_mean_na_rm(const double* p_x, struct iter_opts opts, double* p_out) {
  SLIDE_MEAN_LOOP(SLIDE_MEAN_IMPL_NA_RM, SLIDE_MEAN_ADJUST_IMPL_NA_RM);
}

#undef SLIDE_MEAN_LOOP
#undef SLIDE_MEAN_IMPL_NA_KEEP
#undef SLIDE_MEAN_ADJUST_IMPL_NA_KEEP
#undef SLIDE_MEAN_IMPL_NA_RM
#undef SLIDE_MEAN_ADJUST_IMPL_NA_RM

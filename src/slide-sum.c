#include "slider.h"
#include "slider-vctrs.h"
#include "opts-slide.h"
#include "utils.h"

// -----------------------------------------------------------------------------

static SEXP slide_sum(SEXP x, struct slide_opts opts, bool na_rm);

// [[ register() ]]
SEXP slider_sum(SEXP x, SEXP before, SEXP after, SEXP step, SEXP complete, SEXP na_rm) {
  struct slide_opts opts = new_slide_opts(before, after, step, complete);
  bool c_na_rm = validate_na_rm(na_rm);
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

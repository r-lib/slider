#include "slider.h"
#include "slider-vctrs.h"
#include "utils.h"
#include "params.h"

SEXP slide_sum(SEXP x, SEXP params) {
  x = PROTECT(vec_cast(x, slider_shared_empty_dbl));
  const double* p_x = REAL(x);

  const R_xlen_t size = Rf_xlength(x);

  bool before_unbounded = false;
  bool after_unbounded = false;

  const int before = INTEGER(r_lst_get(params, 0))[0];
  const int after = INTEGER(r_lst_get(params, 1))[0];
  const int step = INTEGER(r_lst_get(params, 2))[0];
  const bool complete = (bool) LOGICAL(r_lst_get(params, 3))[0];
  const bool na_rm = (bool) LOGICAL(r_lst_get(params, 4))[0];

  const bool before_positive = before >= 0;
  const bool after_positive = after >= 0;

  check_double_negativeness(before, after, before_positive, after_positive);
  check_before_negativeness(before, after, before_positive, after_unbounded);
  check_after_negativeness(after, before, after_positive, before_unbounded);

  int iteration_min = 0;
  int iteration_max = size;

  // Iteration adjustment
  if (complete) {
    if (before_positive) {
      iteration_min += before;
    }
    if (after_positive) {
      iteration_max -= after;
    }
  }

  // Forward adjustment to match the number of iterations
  int offset = 0;
  if (complete && before_positive) {
    offset = before;
  }

  int start;
  int start_step;
  if (before_unbounded) {
    start = 0;
    start_step = 0;
  } else {
    start = offset - before;
    start_step = step;
  }

  int stop;
  int stop_step;
  if (after_unbounded) {
    stop = size - 1;
    stop_step = 0;
  } else {
    stop = offset + after;
    stop_step = step;
  }

  SEXP out = PROTECT(slider_init(REALSXP, size));
  double* p_out = REAL(out);

  for (int i = iteration_min;
       i < iteration_max;
       i += step, start += start_step, stop += stop_step) {

    if (i % 1024 == 0) {
      R_CheckUserInterrupt();
    }

    int window_start = max(start, 0);
    int window_stop = min(stop + 1, size);

    double val = 0.0;

    for (int j = window_start; j < window_stop; ++j) {
      val += p_x[j];
    }

    p_out[i] = val;
  }

  UNPROTECT(2);
  return out;
}

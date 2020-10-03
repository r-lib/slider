#ifndef SLIDER_OPTS_SLIDE_H
#define SLIDER_OPTS_SLIDE_H

#include "slider.h"
#include "params.h"

// -----------------------------------------------------------------------------

struct slide_opts {
  int before;
  bool before_unbounded;
  bool before_positive;

  int after;
  bool after_unbounded;
  bool after_positive;

  int step;
  bool complete;
};

static inline struct slide_opts new_slide_opts(SEXP before, SEXP after, SEXP step, SEXP complete, bool dot) {
  bool c_before_unbounded = false;
  bool c_after_unbounded = false;

  int c_before = validate_before(before, &c_before_unbounded, dot);
  bool c_before_positive = c_before >= 0;

  int c_after = validate_after(after, &c_after_unbounded, dot);
  bool c_after_positive = c_after >= 0;

  check_double_negativeness(c_before, c_after, c_before_positive, c_after_positive);
  check_before_negativeness(c_before, c_after, c_before_positive, c_after_unbounded);
  check_after_negativeness(c_after, c_before, c_after_positive, c_before_unbounded);

  int c_step = validate_step(step, dot);
  bool c_complete = validate_complete(complete, dot);

  return (struct slide_opts) {
    .before = c_before,
    .before_unbounded = c_before_unbounded,
    .before_positive = c_before_positive,
    .after = c_after,
    .after_unbounded = c_after_unbounded,
    .after_positive = c_after_positive,
    .step = c_step,
    .complete = c_complete
  };
}

// -----------------------------------------------------------------------------

struct iter_opts {
  R_xlen_t iter_min;
  R_xlen_t iter_max;
  R_xlen_t iter_step;

  R_xlen_t start;
  R_xlen_t start_step;

  R_xlen_t stop;
  R_xlen_t stop_step;

  R_xlen_t size;
};

static inline struct iter_opts new_iter_opts(struct slide_opts opts, R_xlen_t size) {
  R_xlen_t iter_min = 0;
  R_xlen_t iter_max = size;
  R_xlen_t iter_step = opts.step;

  // Iteration adjustment
  if (opts.complete) {
    if (opts.before_positive) {
      iter_min += opts.before;
    }
    if (opts.after_positive) {
      iter_max -= opts.after;
    }
  }

  // Forward adjustment to match the number of iterations
  R_xlen_t offset = 0;
  if (opts.complete && opts.before_positive) {
    offset = opts.before;
  }

  R_xlen_t start;
  R_xlen_t start_step;
  if (opts.before_unbounded) {
    start = 0;
    start_step = 0;
  } else {
    start = offset - opts.before;
    start_step = opts.step;
  }

  R_xlen_t stop;
  R_xlen_t stop_step;
  if (opts.after_unbounded) {
    stop = size - 1;
    stop_step = 0;
  } else {
    stop = offset + opts.after;
    stop_step = opts.step;
  }

  return (struct iter_opts) {
    .iter_min = iter_min,
    .iter_max = iter_max,
    .iter_step = iter_step,
    .start = start,
    .start_step = start_step,
    .stop = stop,
    .stop_step = stop_step,
    .size = size
  };
}

// -----------------------------------------------------------------------------
#endif

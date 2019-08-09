#include "slurrr.h"
#include "utils.h"
#include <vctrs.h>

// -----------------------------------------------------------------------------
// All defined below

static void update_before(SEXP x, struct slide_params* params);
static void update_after(SEXP x, struct slide_params* params);
static void update_step(SEXP x, struct slide_params* params);
static void update_complete(SEXP x, struct slide_params* params);
static void update_forward(SEXP x, struct slide_params* params);
static void update_offset(SEXP x, struct slide_params* params);

static void update_before_after_if_unbounded(struct slide_params* params);
static void validate_before_after_negativeness(const struct slide_params params);

// -----------------------------------------------------------------------------

struct slide_params init_params(SEXP x) {
  struct slide_params params;

  params.type = r_scalar_int_get(r_lst_get(x, 0));
  params.size = r_scalar_int_get(r_lst_get(x, 1));
  params.constrain = r_scalar_lgl_get(r_lst_get(x, 2));

  update_before(x, &params);
  update_after(x, &params);
  update_step(x, &params);
  update_complete(x, &params);
  update_forward(x, &params);

  // Must be done after all of: before/after/complete/forward
  update_offset(x, &params);

  update_before_after_if_unbounded(&params);
  validate_before_after_negativeness(params);

  return params;
}

// -----------------------------------------------------------------------------
// Checking for scalar ptypes

static bool is_scalar(SEXP x) {
  return vec_size(x) == 1;
}

static void stop_scalar(const char * arg, int size) {
  Rf_errorcall(R_NilValue, "`%s` must have size 1, not %i.", arg, size);
}

static void check_scalar(SEXP x, SEXP arg) {
  if (is_scalar(x)) {
    return;
  }

  stop_scalar(r_scalar_chr_get(arg), vec_size(x));
}

static SEXP check_scalar_ptype(SEXP x, SEXP ptype, SEXP x_arg) {
  x = PROTECT(vctrs_cast(
    x,
    ptype,
    x_arg,
    strings_empty
  ));

  check_scalar(x, x_arg);

  UNPROTECT(1);
  return x;
}

static SEXP check_scalar_int(SEXP x, SEXP x_arg) {
  return check_scalar_ptype(x, slurrr_shared_empty_int, x_arg);
}

static SEXP check_scalar_lgl(SEXP x, SEXP x_arg) {
  return check_scalar_ptype(x, slurrr_shared_empty_lgl, x_arg);
}

// -----------------------------------------------------------------------------
// Checking and updating parameters in the struct

static bool is_unbounded(SEXP x) {
  return OBJECT(x) && Rf_inherits(x, "slurrr_box_unbounded");
}

static void update_before(SEXP x, struct slide_params* params) {
  SEXP before = r_lst_get(x, 3);

  if (is_unbounded(before)) {
    params->before = 0;
    params->before_unbounded = true;
    return;
  }

  before = PROTECT(check_scalar_int(before, strings_dot_before));

  params->before = r_scalar_int_get(before);
  params->before_unbounded = false;

  UNPROTECT(1);
}

static void update_after(SEXP x, struct slide_params* params) {
  SEXP after = r_lst_get(x, 4);

  if (is_unbounded(after)) {
    params->after = 0;
    params->after_unbounded = true;
    return;
  }

  after = PROTECT(check_scalar_int(after, strings_dot_after));

  params->after = r_scalar_int_get(after);
  params->after_unbounded = false;

  UNPROTECT(1);
}

static void update_step(SEXP x, struct slide_params* params) {
  SEXP step = r_lst_get(x, 5);

  step = PROTECT(check_scalar_int(step, strings_dot_step));

  params->step = r_scalar_int_get(step);

  if (params->step < 1) {
    Rf_errorcall(R_NilValue, "`.step` must be at least 1, not %i.", params->step);
  }

  UNPROTECT(1);
}

static void update_complete(SEXP x, struct slide_params* params) {
  SEXP complete = r_lst_get(x, 6);

  complete = PROTECT(check_scalar_lgl(complete, strings_dot_complete));

  params->complete = r_scalar_lgl_get(complete);

  UNPROTECT(1);
}

static void update_forward(SEXP x, struct slide_params* params) {
  SEXP forward = r_lst_get(x, 7);

  forward = PROTECT(check_scalar_lgl(forward, strings_dot_forward));

  params->forward = r_scalar_lgl_get(forward);

  UNPROTECT(1);
}

static void update_offset(SEXP x, struct slide_params* params) {
  SEXP offset = r_lst_get(x, 8);

  bool null_offset = offset == R_NilValue;

  if (!null_offset) {
    offset = PROTECT(check_scalar_int(offset, strings_dot_offset));
    params->offset = r_scalar_int_get(offset);
    UNPROTECT(1);
  }

  // - Checking if the start of the frame is out of range when going forward
  // - Ensure `before <= offset` so we can create a full window
  if (params->complete &&
      params->forward &&
      !params->before_unbounded &&
      (null_offset || params->before > params->offset)) {
    params->offset = params->before;
    return;
  }

  // - Checking if the start of the frame is out of range when going backward
  // - Ensure `after <= offset` so we can create a full window
  if (params->complete &&
      !params->forward &&
      !params->after_unbounded &&
      (null_offset || params->after > params->offset)) {
    params->offset = params->after;
    return;
  }

  // - Checking if the end of the frame is out of range when going forward
  // - Ensure that if `after < 0`, then `abs(after) <= offset`
  //   so we have some data to partially compute on
  if (!params->complete && params->forward && !params->after_unbounded && params->after < 0) {
    if (null_offset || params->offset < -params->after) {
      params->offset = -params->after;
      return;
    }
  }

  // - Checking if the end of the frame is out of range when going backward
  // - Ensure that if `before < 0`, then `abs(before) <= offset` so we have
  //   some data to partially compute on
  if (!params->complete && !params->forward && !params->before_unbounded && params->before < 0L) {
    if (null_offset || params->offset < -params->before) {
      params->offset = -params->before;
      return;
    }
  }

  // If offset was NULL and none of these conditions were met,
  // meaning that we have a usable window, then set offset to 0
  if (null_offset) {
    params->offset = 0;
  }

  return;
}

// -----------------------------------------------------------------------------
// Extra checks on the validity of before/after with unbounded-ness

// The order of checks are important here and are done so they they work even
// if both .before/.after are unbounded(). The goal is to set unbounded()
// .before/.after values to values such that the width of the first
// iteration's window frame is correct

static void update_before_after_if_unbounded(struct slide_params* params) {
  if (params->forward) {
    if (params->before_unbounded) {
      params->before = params->offset;
    }
    if (params->after_unbounded) {
      params->after = params->size - 1 - params->offset;
    }
  } else {
    if (params->after_unbounded) {
      params->after = params->offset;
    }
    if (params->before_unbounded) {
      params->before = params->size - 1 - params->offset;
    }
  }
}

static void validate_before_after_negativeness(const struct slide_params params) {
  bool before_negative = params.before < 0;
  bool after_negative = params.after < 0;

  if (!before_negative && !after_negative) {
    return;
  }

  if (before_negative && after_negative) {
    Rf_errorcall(
      R_NilValue,
      "`.before` (%i) and `.after` (%i) cannot both be negative.",
      params.before,
      params.after
    );
  }

  if (before_negative && abs(params.before) > params.after) {
    int abs_before = abs(params.before);
    Rf_errorcall(
      R_NilValue,
      "When `.before` (%i) is negative, it's absolute value (%i) cannot be greater than `.after` (%i).",
      params.before,
      abs_before,
      params.after
    );
  }

  if (after_negative && abs(params.after) > params.before) {
    int abs_after = abs(params.after);
    Rf_errorcall(
      R_NilValue,
      "When `.after` (%i) is negative, it's absolute value (%i) cannot be greater than `.before` (%i).",
      params.after,
      abs_after,
      params.before
    );
  }
}

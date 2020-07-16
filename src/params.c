#include "slider.h"
#include "slider-vctrs.h"
#include "utils.h"
#include "params.h"

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

static SEXP check_ptype(SEXP x, SEXP ptype) {
  return vec_cast(x, ptype);
}

static SEXP check_int(SEXP x) {
  return check_ptype(x, slider_shared_empty_int);
}

static SEXP check_lgl(SEXP x) {
  return check_ptype(x, slider_shared_empty_lgl);
}

static SEXP check_scalar_int(SEXP x, SEXP x_arg) {
  check_scalar(x, x_arg);
  return check_int(x);
}

static SEXP check_scalar_lgl(SEXP x, SEXP x_arg) {
  check_scalar(x, x_arg);
  return check_lgl(x);
}

// -----------------------------------------------------------------------------

static bool is_unbounded(SEXP x) {
  return TYPEOF(x) == REALSXP && REAL(x)[0] == R_PosInf;
}

// -----------------------------------------------------------------------------

// [[ include("params.h") ]]
int pull_type(SEXP params) {
  return r_scalar_int_get(r_lst_get(params, 0));
}

// [[ include("params.h") ]]
bool pull_constrain(SEXP params) {
  return r_scalar_lgl_get(r_lst_get(params, 1));
}

// [[ include("params.h") ]]
bool pull_atomic(SEXP params) {
  return r_scalar_lgl_get(r_lst_get(params, 2));
}

// [[ include("params.h") ]]
int pull_before(SEXP params, bool* before_unbounded) {
  SEXP before = r_lst_get(params, 3);

  check_scalar(before, strings_dot_before);

  if (is_unbounded(before)) {
    *before_unbounded = true;
    return 0;
  }

  before = PROTECT(check_int(before));
  int out = r_scalar_int_get(before);

  if (out == NA_INTEGER) {
    Rf_errorcall(R_NilValue, "`.before` can't be missing.");
  }

  UNPROTECT(1);
  return out;
}

// [[ include("params.h") ]]
int pull_after(SEXP params, bool* after_unbounded) {
  SEXP after = r_lst_get(params, 4);

  check_scalar(after, strings_dot_after);

  if (is_unbounded(after)) {
    *after_unbounded = true;
    return 0;
  }

  after = PROTECT(check_int(after));
  int out = r_scalar_int_get(after);

  if (out == NA_INTEGER) {
    Rf_errorcall(R_NilValue, "`.after` can't be missing.");
  }

  UNPROTECT(1);
  return out;
}

// [[ include("params.h") ]]
int pull_step(SEXP params) {
  SEXP step_ = r_lst_get(params, 5);
  step_ = PROTECT(check_scalar_int(step_, strings_dot_step));

  int step = r_scalar_int_get(step_);

  if (step == NA_INTEGER) {
    Rf_errorcall(R_NilValue, "`.step` can't be missing.");
  }

  if (step < 1) {
    Rf_errorcall(R_NilValue, "`.step` must be at least 1, not %i.", step);
  }

  UNPROTECT(1);
  return step;
}

// [[ include("params.h") ]]
int pull_complete(SEXP params) {
  SEXP complete = r_lst_get(params, 6);
  complete = PROTECT(check_scalar_lgl(complete, strings_dot_complete));
  int out = r_scalar_lgl_get(complete);

  if (out == NA_LOGICAL) {
    Rf_errorcall(R_NilValue, "`.complete` can't be missing.");
  }

  UNPROTECT(1);
  return out;
}

// -----------------------------------------------------------------------------

// [[ include("params.h") ]]
void check_double_negativeness(int before, int after, bool before_positive, bool after_positive) {
  if (!before_positive && !after_positive) {
    Rf_errorcall(
      R_NilValue,
      "`.before` (%i) and `.after` (%i) cannot both be negative.",
      before,
      after
    );
  }
}

// [[ include("params.h") ]]
void check_after_negativeness(int after, int before, bool after_positive, bool before_unbounded) {
  if (after_positive) {
    return;
  }

  if (before_unbounded) {
    return;
  }

  int abs_after = abs(after);

  if (abs_after > before) {
    Rf_errorcall(
      R_NilValue,
      "When `.after` (%i) is negative, it's absolute value (%i) cannot be greater than `.before` (%i).",
      after,
      abs_after,
      before
    );
  }
}

// [[ include("params.h") ]]
void check_before_negativeness(int before, int after, bool before_positive, bool after_unbounded) {
  if (before_positive) {
    return;
  }

  if (after_unbounded) {
    return;
  }

  int abs_before = abs(before);

  if (abs_before > after) {
    Rf_errorcall(
      R_NilValue,
      "When `.before` (%i) is negative, it's absolute value (%i) cannot be greater than `.after` (%i).",
      before,
      abs_before,
      after
    );
  }
}

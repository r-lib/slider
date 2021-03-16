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
  return !OBJECT(x) && TYPEOF(x) == REALSXP && REAL(x)[0] == R_PosInf;
}

// -----------------------------------------------------------------------------

// [[ include("params.h") ]]
int validate_type(SEXP x) {
  return r_scalar_int_get(x);
}

// [[ include("params.h") ]]
bool validate_constrain(SEXP x) {
  return r_scalar_lgl_get(x);
}

// [[ include("params.h") ]]
bool validate_atomic(SEXP x) {
  return r_scalar_lgl_get(x);
}

// [[ include("params.h") ]]
int validate_before(SEXP x, bool* before_unbounded, bool dot) {
  check_scalar(x, dot ? strings_dot_before : strings_before);

  if (is_unbounded(x)) {
    *before_unbounded = true;
    return 0;
  }

  x = PROTECT(check_int(x));
  int out = r_scalar_int_get(x);

  if (out == NA_INTEGER) {
    if (dot) {
      Rf_errorcall(R_NilValue, "`.before` can't be missing.");
    } else {
      Rf_errorcall(R_NilValue, "`before` can't be missing.");
    }
  }

  UNPROTECT(1);
  return out;
}

// [[ include("params.h") ]]
int validate_after(SEXP x, bool* after_unbounded, bool dot) {
  check_scalar(x, dot ? strings_dot_after : strings_after);

  if (is_unbounded(x)) {
    *after_unbounded = true;
    return 0;
  }

  x = PROTECT(check_int(x));
  int out = r_scalar_int_get(x);

  if (out == NA_INTEGER) {
    if (dot) {
      Rf_errorcall(R_NilValue, "`.after` can't be missing.");
    } else {
      Rf_errorcall(R_NilValue, "`after` can't be missing.");
    }
  }

  UNPROTECT(1);
  return out;
}

// [[ include("params.h") ]]
int validate_step(SEXP x, bool dot) {
  x = PROTECT(check_scalar_int(x, dot ? strings_dot_step : strings_step));

  int step = r_scalar_int_get(x);

  if (step == NA_INTEGER) {
    if (dot) {
      Rf_errorcall(R_NilValue, "`.step` can't be missing.");
    } else {
      Rf_errorcall(R_NilValue, "`step` can't be missing.");
    }
  }

  if (step < 1) {
    if (dot) {
      Rf_errorcall(R_NilValue, "`.step` must be at least 1, not %i.", step);
    } else {
      Rf_errorcall(R_NilValue, "`step` must be at least 1, not %i.", step);
    }
  }

  UNPROTECT(1);
  return step;
}

// [[ include("params.h") ]]
int validate_complete(SEXP x, bool dot) {
  x = PROTECT(check_scalar_lgl(x, dot ? strings_dot_complete : strings_complete));
  int out = r_scalar_lgl_get(x);

  if (out == NA_LOGICAL) {
    if (dot) {
      Rf_errorcall(R_NilValue, "`.complete` can't be missing.");
    } else {
      Rf_errorcall(R_NilValue, "`complete` can't be missing.");
    }
  }

  UNPROTECT(1);
  return out;
}

// [[ include("params.h") ]]
int validate_na_rm(SEXP x, bool dot) {
  x = PROTECT(check_scalar_lgl(x, dot ? strings_dot_na_rm : strings_na_rm));
  int out = r_scalar_lgl_get(x);

  if (out == NA_LOGICAL) {
    if (dot) {
      Rf_errorcall(R_NilValue, "`.na_rm` can't be missing.");
    } else {
      Rf_errorcall(R_NilValue, "`na_rm` can't be missing.");
    }
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

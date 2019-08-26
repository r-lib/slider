// Ripped and altered from:
// https://github.com/r-lib/vctrs/blob/527e350ae74f245a464f9b35111e560acadc8515/src/compare.c

#include "compare.h"
#include "slide-vctrs.h"
#include <vctrs.h>
#include <strings.h>

static void stop_not_comparable(SEXP x, SEXP y, const char* message) {
  Rf_errorcall(R_NilValue, "`x` and `y` are not comparable: %s", message);
}

static bool is_data_frame(SEXP x) {
  return Rf_inherits(x, "data.frame");
}

// https://stackoverflow.com/questions/10996418
static int icmp(int x, int y) {
  return (x > y) - (x < y);
}
static int dcmp(double x, double y) {
  return (x > y) - (x < y);
}

static int scmp(SEXP x, SEXP y) {
  if (x == y)
    return 0;
  int cmp = strcmp(CHAR(x), CHAR(y));
  return cmp / abs(cmp);
}

static int compare_scalar(SEXP x, R_len_t i, SEXP y, R_len_t j) {
  if (TYPEOF(x) != TYPEOF(y))
    stop_not_comparable(x, y, "different types");

  switch(TYPEOF(x)) {
  case LGLSXP: {
    int xi = LOGICAL(x)[i], yj = LOGICAL(y)[j];
    return icmp(xi, yj);
  }
  case INTSXP: {
    int xi = INTEGER(x)[i], yj = INTEGER(y)[j];
    return icmp(xi, yj);
  }
  case REALSXP: {
    double xi = REAL(x)[i], yj = REAL(y)[j];
    return (isnan(xi) || isnan(yj)) ? NA_INTEGER : dcmp(xi, yj);
  }
  case STRSXP: {
    SEXP xi = STRING_ELT(x, i), yj = STRING_ELT(y, j);
    return (xi == NA_STRING || yj == NA_STRING) ? NA_INTEGER : scmp(xi, yj);

  }
  case VECSXP:
    if (is_data_frame(x)) {
      int p = Rf_length(x);
      if (p != Rf_length(y))
        stop_not_comparable(x, y, "different number of columns");
      // TODO - Check for equal_names
      //if (!equal_names(x, y))
      //  stop_not_comparable(x, y, "different column names");

      if (p == 0)
        stop_not_comparable(x, y, "data frame with zero columns");

      if (p > 1) {
        for (int k = 0; k < (p - 1); ++k) {
          SEXP col_x = VECTOR_ELT(x, k);
          SEXP col_y = VECTOR_ELT(y, k);
          int cmp = compare_scalar(col_x, i, col_y, j);

          if (cmp != 0)
            return cmp;
        }
      }

      SEXP col_x = VECTOR_ELT(x, p - 1);
      SEXP col_y = VECTOR_ELT(y, p - 1);
      return compare_scalar(col_x, i, col_y, j);
    } else {
      stop_not_comparable(x, y, "lists are not comparable");
    }
  default:
      Rf_errorcall(R_NilValue, "Unsupported type %s", Rf_type2char(TYPEOF(x)));
  }
}

// [[ include("compare.h") ]]
bool compare_gt(SEXP x, R_len_t i, SEXP y, R_len_t j) {
  return compare_scalar(x, i, y, j) == 1;
}

// [[ include("compare.h") ]]
bool compare_lt(SEXP x, R_len_t i, SEXP y, R_len_t j) {
  return compare_scalar(x, i, y, j) == -1;
}

// [[ include("compare.h") ]]
bool compare_lte(SEXP x, R_len_t i, SEXP y, R_len_t j) {
  return compare_scalar(x, i, y, j) <= 0;
}

// [[ include("compare.h") ]]
bool vec_any_gt(SEXP x, SEXP y) {
  R_len_t n = vec_size(x);

  for (R_len_t i = 0; i < n; ++i) {
    if (compare_scalar(x, i, y, i) == 1) {
      return(true);
    }
  }

  return false;
}

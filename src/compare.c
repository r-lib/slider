// - Modified version of `vctrs::vec_compare()`
// - We don't care about `NA`s because the index is not allowed to have any

#include "slider-vctrs.h"
#include "compare.h"
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

// UTF-8 translation is successful in these cases:
// - (utf8 + latin1), (unknown + utf8), (unknown + latin1)
// UTF-8 translation fails purposefully in these cases:
// - (bytes + utf8), (bytes + latin1), (bytes + unknown)
// UTF-8 translation is not attempted in these cases:
// - (utf8 + utf8), (latin1 + latin1), (unknown + unknown), (bytes + bytes)

static int scmp(SEXP x, SEXP y) {
  if (x == y) {
    return 0;
  }

  // Same encoding
  if (Rf_getCharCE(x) == Rf_getCharCE(y)) {
    int cmp = strcmp(CHAR(x), CHAR(y));
    return cmp / abs(cmp);
  }

  const void *vmax = vmaxget();
  int cmp = strcmp(Rf_translateCharUTF8(x), Rf_translateCharUTF8(y));
  vmaxset(vmax);

  if (cmp == 0) {
    return cmp;
  } else {
    return cmp / abs(cmp);
  }
}

// -----------------------------------------------------------------------------

static inline int lgl_compare_scalar(const int* x, const int* y) {
  return icmp(*x, *y);
}

static inline int int_compare_scalar(const int* x, const int* y) {
  return icmp(*x, *y);
}

static inline int dbl_compare_scalar(const double* x, const double* y) {
  return dcmp(*x, *y);
}

static inline int chr_compare_scalar(const SEXP* x, const SEXP* y) {
  return scmp(*x, *y);
}

static int compare_scalar(SEXP x, R_len_t i, SEXP y, R_len_t j);

static inline int df_compare_scalar(SEXP x, R_len_t i, SEXP y, R_len_t j, R_len_t n_col) {
  int cmp;

  for (int k = 0; k < n_col; ++k) {
    SEXP col_x = VECTOR_ELT(x, k);
    SEXP col_y = VECTOR_ELT(y, k);

    cmp = compare_scalar(col_x, i, col_y, j);

    if (cmp != 0) {
      return cmp;
    }
  }

  return cmp;
}

// -----------------------------------------------------------------------------

static int compare_scalar(SEXP x, R_len_t i, SEXP y, R_len_t j) {
  switch (TYPEOF(x)) {
  case LGLSXP: return lgl_compare_scalar(LOGICAL(x) + i, LOGICAL(y) + j);
  case INTSXP: return int_compare_scalar(INTEGER(x) + i, INTEGER(y) + j);
  case REALSXP: return dbl_compare_scalar(REAL(x) + i, REAL(y) + j);
  case STRSXP: return chr_compare_scalar(STRING_PTR(x) + i, STRING_PTR(y) + j);
  case VECSXP: {
    if (!is_data_frame(x)) {
      stop_not_comparable(x, y, "lists are not comparable");
    }

    int n_col = Rf_length(x);

    if (n_col != Rf_length(y)) {
      stop_not_comparable(x, y, "must have the same number of columns");
    }

    if (n_col == 0) {
      stop_not_comparable(x, y, "data frame with zero columns");
    }

    return df_compare_scalar(x, i, y, j, n_col);
  }
  default:
    Rf_errorcall(R_NilValue, "Unsupported type %s", Rf_type2char(TYPEOF(x)));
  }
}

// -----------------------------------------------------------------------------

#define SCALAR_COMPARE_DIRECTION(CONST_DEREF, SCALAR_COMPARE, RELATION, TO)  \
do {                                                                         \
  return SCALAR_COMPARE(CONST_DEREF(x) + i, CONST_DEREF(y) + j) RELATION TO; \
}                                                                            \
while (0)

static bool lgl_compare_lt(SEXP x, R_len_t i, SEXP y, R_len_t j) {
  SCALAR_COMPARE_DIRECTION(LOGICAL_RO, lgl_compare_scalar, ==, -1);
}
static bool int_compare_lt(SEXP x, R_len_t i, SEXP y, R_len_t j) {
  SCALAR_COMPARE_DIRECTION(INTEGER_RO, int_compare_scalar, ==, -1);
}
static bool dbl_compare_lt(SEXP x, R_len_t i, SEXP y, R_len_t j) {
  SCALAR_COMPARE_DIRECTION(REAL_RO, dbl_compare_scalar, ==, -1);
}
static bool chr_compare_lt(SEXP x, R_len_t i, SEXP y, R_len_t j) {
  SCALAR_COMPARE_DIRECTION(STRING_PTR_RO, chr_compare_scalar, ==, -1);
}
static bool df_compare_lt(SEXP x, R_len_t i, SEXP y, R_len_t j) {
  int n_col = Rf_length(x);

  if (n_col != Rf_length(y)) {
    stop_not_comparable(x, y, "must have the same number of columns");
  }

  if (n_col == 0) {
    stop_not_comparable(x, y, "data frame with zero columns");
  }

  return df_compare_scalar(x, i, y, j, n_col) == -1;
}

static bool lgl_compare_gt(SEXP x, R_len_t i, SEXP y, R_len_t j) {
  SCALAR_COMPARE_DIRECTION(LOGICAL_RO, lgl_compare_scalar, ==, 1);
}
static bool int_compare_gt(SEXP x, R_len_t i, SEXP y, R_len_t j) {
  SCALAR_COMPARE_DIRECTION(INTEGER_RO, int_compare_scalar, ==, 1);
}
static bool dbl_compare_gt(SEXP x, R_len_t i, SEXP y, R_len_t j) {
  SCALAR_COMPARE_DIRECTION(REAL_RO, dbl_compare_scalar, ==, 1);
}
static bool chr_compare_gt(SEXP x, R_len_t i, SEXP y, R_len_t j) {
  SCALAR_COMPARE_DIRECTION(STRING_PTR_RO, chr_compare_scalar, ==, 1);
}
static bool df_compare_gt(SEXP x, R_len_t i, SEXP y, R_len_t j) {
  int n_col = Rf_length(x);

  if (n_col != Rf_length(y)) {
    stop_not_comparable(x, y, "must have the same number of columns");
  }

  if (n_col == 0) {
    stop_not_comparable(x, y, "data frame with zero columns");
  }

  return df_compare_scalar(x, i, y, j, n_col) == 1;
}

static bool lgl_compare_lte(SEXP x, R_len_t i, SEXP y, R_len_t j) {
  SCALAR_COMPARE_DIRECTION(LOGICAL_RO, lgl_compare_scalar, <=, 0);
}
static bool int_compare_lte(SEXP x, R_len_t i, SEXP y, R_len_t j) {
  SCALAR_COMPARE_DIRECTION(INTEGER_RO, int_compare_scalar, <=, 0);
}
static bool dbl_compare_lte(SEXP x, R_len_t i, SEXP y, R_len_t j) {
  SCALAR_COMPARE_DIRECTION(REAL_RO, dbl_compare_scalar, <=, 0);
}
static bool chr_compare_lte(SEXP x, R_len_t i, SEXP y, R_len_t j) {
  SCALAR_COMPARE_DIRECTION(STRING_PTR_RO, chr_compare_scalar, <=, 0);
}
static bool df_compare_lte(SEXP x, R_len_t i, SEXP y, R_len_t j) {
  int n_col = Rf_length(x);

  if (n_col != Rf_length(y)) {
    stop_not_comparable(x, y, "must have the same number of columns");
  }

  if (n_col == 0) {
    stop_not_comparable(x, y, "data frame with zero columns");
  }

  return df_compare_scalar(x, i, y, j, n_col) <= 0;
}

// [[ include("compare.h") ]]
slider_compare_fn_t get_compare_fn_lt(SEXP x) {
  switch (TYPEOF(x)) {
  case LGLSXP: return lgl_compare_lt;
  case INTSXP: return int_compare_lt;
  case REALSXP: return dbl_compare_lt;
  case STRSXP: return chr_compare_lt;
  case VECSXP: {
    if (!is_data_frame(x)) {
      Rf_errorcall(R_NilValue, "`x` and `y` are not comparable, lists are not comparable.");
    }
    return df_compare_lt;
  }
  default:
    Rf_errorcall(R_NilValue, "Unsupported type %s", Rf_type2char(TYPEOF(x)));
  }
}

// [[ include("compare.h") ]]
slider_compare_fn_t get_compare_fn_gt(SEXP x) {
  switch (TYPEOF(x)) {
  case LGLSXP: return lgl_compare_gt;
  case INTSXP: return int_compare_gt;
  case REALSXP: return dbl_compare_gt;
  case STRSXP: return chr_compare_gt;
  case VECSXP: {
    if (!is_data_frame(x)) {
    Rf_errorcall(R_NilValue, "`x` and `y` are not comparable, lists are not comparable.");
  }
    return df_compare_gt;
  }
  default:
    Rf_errorcall(R_NilValue, "Unsupported type %s", Rf_type2char(TYPEOF(x)));
  }
}

// [[ include("compare.h") ]]
slider_compare_fn_t get_compare_fn_lte(SEXP x) {
  switch (TYPEOF(x)) {
  case LGLSXP: return lgl_compare_lte;
  case INTSXP: return int_compare_lte;
  case REALSXP: return dbl_compare_lte;
  case STRSXP: return chr_compare_lte;
  case VECSXP: {
    if (!is_data_frame(x)) {
    Rf_errorcall(R_NilValue, "`x` and `y` are not comparable, lists are not comparable.");
  }
    return df_compare_lte;
  }
  default:
    Rf_errorcall(R_NilValue, "Unsupported type %s", Rf_type2char(TYPEOF(x)));
  }
}

#undef SCALAR_COMPARE_DIRECTION

// -----------------------------------------------------------------------------

static bool df_any_gt(SEXP x, SEXP y, R_len_t n_row);

#define ANY_GT(CTYPE, CONST_DEREF, SCALAR_COMPARE)      \
do {                                                    \
  const CTYPE* p_x = CONST_DEREF(x);                    \
  const CTYPE* p_y = CONST_DEREF(y);                    \
                                                        \
  for (R_len_t i = 0; i < size; ++i, ++p_x, ++p_y) {    \
    if (SCALAR_COMPARE(p_x, p_y) == 1) {                \
      return true;                                      \
    }                                                   \
  }                                                     \
                                                        \
  return false;                                         \
}                                                       \
while (0)

// [[ include("compare.h") ]]
bool vec_any_gt(SEXP x, SEXP y) {
  R_len_t size = vec_size(x);

  switch (TYPEOF(x)) {
  case LGLSXP:  ANY_GT(int, LOGICAL_RO, lgl_compare_scalar);
  case INTSXP:  ANY_GT(int, INTEGER_RO, int_compare_scalar);
  case REALSXP: ANY_GT(double, REAL_RO, dbl_compare_scalar);
  case STRSXP:  ANY_GT(SEXP, STRING_PTR_RO, chr_compare_scalar);
  case VECSXP: {
    if (is_data_frame(x)) {
      return df_any_gt(x, y, size);
    } else {
      Rf_errorcall(R_NilValue, "Can't compare lists with `vec_any_gt()`");
    }
  }
  default:
    Rf_error("Unimplemented type in `vec_any_gt()`");
  }
}

#undef ANY_GT

// -----------------------------------------------------------------------------

static bool vec_any_gt_col(SEXP x, SEXP y, R_len_t n_row);

static bool df_any_gt(SEXP x, SEXP y, R_len_t n_row) {
  int n_col = Rf_length(x);

  if (n_col == 0) {
    stop_not_comparable(x, y, "data frame with zero columns");
  }

  if (n_col != Rf_length(y)) {
    stop_not_comparable(x, y, "must have the same number of columns");
  }

  for (R_len_t i = 0; i < n_col; ++i) {
    SEXP x_col = VECTOR_ELT(x, i);
    SEXP y_col = VECTOR_ELT(y, i);

    if (vec_any_gt_col(x_col, y_col, n_row)) {
      return true;
    }
  }

  return false;
}

// -----------------------------------------------------------------------------

#define ANY_GT_COL(CTYPE, CONST_DEREF, SCALAR_COMPARE) \
do {                                                   \
  const CTYPE* p_x = CONST_DEREF(x);                   \
  const CTYPE* p_y = CONST_DEREF(y);                   \
                                                       \
  for (R_len_t i = 0; i < n_row; ++i, ++p_x, ++p_y) {  \
    if (SCALAR_COMPARE(p_x, p_y) == 1) {               \
      return true;                                     \
    }                                                  \
  }                                                    \
                                                       \
  return false;                                        \
}                                                      \
while (0)

static bool vec_any_gt_col(SEXP x, SEXP y, R_len_t n_row) {
  switch (TYPEOF(x)) {
  case LGLSXP:  ANY_GT_COL(int, LOGICAL_RO, lgl_compare_scalar);
  case INTSXP:  ANY_GT_COL(int, INTEGER_RO, int_compare_scalar);
  case REALSXP: ANY_GT_COL(double, REAL_RO, dbl_compare_scalar);
  case STRSXP:  ANY_GT_COL(SEXP, STRING_PTR_RO, chr_compare_scalar);
  case VECSXP: {
    if (is_data_frame(x)) {
      return df_any_gt(x, y, n_row);
    } else {
      Rf_errorcall(R_NilValue, "Can't compare lists with `vec_any_gt()`");
    }
  }
  default:
    Rf_error("Unimplemented type in `vec_any_gt()`");
  }
}

#undef ANY_GT_COL

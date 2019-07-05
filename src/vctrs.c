#include "vctrs.h"

SEXP vec_slice(SEXP x, SEXP index) {
  static SEXP (*fn)(SEXP, SEXP) = NULL;

  if (fn == NULL) {
    fn = (SEXP (*)(SEXP, SEXP)) R_GetCCallable("vctrs", "vec_slice");
  }

  // Call the function
  return fn(x, index);
}

SEXP vec_init(SEXP x, R_len_t n) {
  static SEXP (*fn)(SEXP, R_len_t) = NULL;

  if (fn == NULL) {
    fn = (SEXP (*)(SEXP, R_len_t)) R_GetCCallable("vctrs", "vec_init");
  }

  // Call the function
  return fn(x, n);
}

SEXP vec_assign_impl(SEXP x, SEXP i, SEXP value, bool clone) {
  static SEXP (*fn)(SEXP, SEXP, SEXP, bool) = NULL;

  if (fn == NULL) {
    fn = (SEXP (*)(SEXP, SEXP, SEXP, bool)) R_GetCCallable("vctrs", "vec_assign_impl");
  }

  // Call the function
  return fn(x, i, value, clone);
}

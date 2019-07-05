#include "slurrr.h"
#include <vctrs.h>

// just to test if it works
SEXP slurrr_slide(SEXP x, SEXP index) {
  return vec_slice(x, index);
}

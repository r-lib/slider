#include "summary-core-align.h"

extern "C" {

/*
 * `alignof()` is C++11 specific, so this single compilation unit requires
 * C++11, and we call these helpers from C in `summary-core.h`.
 *
 * Technically `alignof()` is also in C11, but it is unclear how well R supports
 * that.
 */

size_t align_of_long_double(void) {
  return alignof(long double);
}

size_t align_of_mean_state_t(void) {
  return alignof(struct mean_state_t);
}

} // extern "C"

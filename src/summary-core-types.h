#ifndef SLIDER_SUMMARY_CORE_TYPES
#define SLIDER_SUMMARY_CORE_TYPES

#include <stdint.h> // uintptr_t

struct mean_state_t {
  long double sum;
  uint64_t count;
};

#endif

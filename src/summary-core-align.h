#ifndef SLIDER_SUMMARY_CORE_ALIGN
#define SLIDER_SUMMARY_CORE_ALIGN

#include <stddef.h> // size_t

extern "C" {

#include "summary-core-types.h"

size_t align_of_long_double(void);
size_t align_of_mean_state_t(void);

} // extern "C"

#endif

#ifndef SLIDER_ITERATION_H
#define SLIDER_ITERATION_H

#include "slider.h"

struct iteration_cleanup_info {
  int old_iteration;
};

void slider_iteration_cleanup(void* p_data);

#endif

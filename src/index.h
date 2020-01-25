#ifndef SLIDER_INDEX_H
#define SLIDER_INDEX_H

#include "slider.h"
#include "compare.h"

// -----------------------------------------------------------------------------

struct window_info {
  int* starts;
  int* stops;
  SEXP seq;
  int* p_seq_val;
};

#define PROTECT_WINDOW_INFO(window, n) do {  \
  PROTECT((window)->seq);                    \
  *n += 1;                                   \
} while (0)

// -----------------------------------------------------------------------------

struct index_info {
  SEXP data;
  int size;
  int last_pos;
  int current_start_pos;
  int current_stop_pos;
  slider_compare_fn_t compare_lt;
  slider_compare_fn_t compare_gt;
  slider_compare_fn_t compare_lte;
};

#define PROTECT_INDEX_INFO(index, n) do {  \
  PROTECT((index)->data);                  \
  *n += 1;                                 \
} while (0)

// -----------------------------------------------------------------------------

struct range_info {
  SEXP starts;
  SEXP stops;
  int size;
  bool start_unbounded;
  bool stop_unbounded;
};

#define PROTECT_RANGE_INFO(range, n) do { \
  PROTECT((range)->starts);               \
  PROTECT((range)->stops);                \
  *n += 2;                                \
} while (0)

// -----------------------------------------------------------------------------

#endif

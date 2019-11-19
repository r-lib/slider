#ifndef SLIDE_BETWEEN_H
#define SLIDE_BETWEEN_H

#include "slide.h"
#include "compare.h"

// -----------------------------------------------------------------------------

struct out_info {
  SEXP data;
  PROTECT_INDEX data_pidx;
  SEXP ptype;
  int size;
  SEXP indices;
  bool has_indices;
  SEXP index;
  int* p_index_val;
  int index_size;
};

#define PROTECT_OUT_INFO(out, n) do {                  \
  PROTECT_WITH_INDEX((out)->data, &(out)->data_pidx);  \
  PROTECT((out)->ptype);                               \
  PROTECT((out)->indices);                             \
  PROTECT((out)->index);                               \
  *n += 4;                                             \
} while (0)

// -----------------------------------------------------------------------------

struct window_info {
  int* starts;
  int* stops;
  int start;
  int stop;
  int start_idx;
  int stop_idx;
  SEXP seq;
  int* p_seq_val;
  int size;
};

#define PROTECT_WINDOW_INFO(window, n) do {  \
  PROTECT((window)->seq);                    \
  *n += 1;                                   \
} while (0)

// -----------------------------------------------------------------------------

struct index_info {
  SEXP data;
  int size;
  int current_start_pos;
  int current_stop_pos;
  slide_compare_fn_t compare_lt;
  slide_compare_fn_t compare_gt;
  slide_compare_fn_t compare_lte;
};

#define PROTECT_INDEX_INFO(index, n) do {  \
  PROTECT((index)->data);                  \
  *n += 1;                                 \
} while (0)

// -----------------------------------------------------------------------------

struct range_info {
  SEXP starts;
  SEXP stops;
  SEXP start;
  SEXP stop;
  PROTECT_INDEX start_pidx;
  PROTECT_INDEX stop_pidx;
  int size;
  bool start_unbounded;
  bool stop_unbounded;
};

#define PROTECT_RANGE_INFO(range, n) do {                     \
  PROTECT((range)->starts);                                   \
  PROTECT((range)->stops);                                    \
  PROTECT_WITH_INDEX((range)->start, &(range)->start_pidx);   \
  PROTECT_WITH_INDEX((range)->stop, &(range)->stop_pidx);     \
  *n += 4;                                                    \
} while (0)

// -----------------------------------------------------------------------------

struct iteration_info {
  SEXP data;
  int* p_data_val;
  int max;
};

#define PROTECT_ITERATION_INFO(iteration, n) do {  \
  PROTECT((iteration)->data);                      \
  *n += 1;                                         \
} while (0)

// -----------------------------------------------------------------------------

#endif

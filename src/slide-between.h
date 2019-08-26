#ifndef SLIDE_BETWEEN_H
#define SLIDE_BETWEEN_H

#include "slide.h"

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
  SEXP first;
  SEXP last;
  int size;
};

#define PROTECT_INDEX_INFO(index, n) do {  \
  PROTECT((index)->data);                  \
  PROTECT((index)->first);                 \
  PROTECT((index)->last);                  \
  *n += 3;                                 \
} while (0)

// -----------------------------------------------------------------------------

struct last_info {
  SEXP start_loc;
  SEXP stop_loc;
  int* p_start_loc_val;
  int* p_stop_loc_val;
  SEXP start_index;
  SEXP stop_index;
  SEXP* p_start_index;
  SEXP* p_stop_index;
};

#define PROTECT_LAST_INFO(last, n) do {                \
  PROTECT((last)->start_loc);                          \
  PROTECT((last)->stop_loc);                           \
  PROTECT((last)->start_index);                        \
  PROTECT((last)->stop_index);                         \
  /* SEXP* assignment must be done after protection */ \
  (last)->p_start_index = &(last)->start_index;        \
  (last)->p_stop_index = &(last)->stop_index;          \
  *n += 4;                                             \
} while (0)

// -----------------------------------------------------------------------------

struct range_info {
  SEXP starts;
  SEXP stops;
  SEXP start;
  SEXP stop;
  PROTECT_INDEX start_pidx;
  PROTECT_INDEX stop_pidx;
  int count;
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

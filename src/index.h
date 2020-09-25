#ifndef SLIDER_INDEX_H
#define SLIDER_INDEX_H

#include "slider.h"
#include "compare.h"

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

struct index_info new_index_info(SEXP);

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

struct range_info new_range_info(SEXP, SEXP, int);

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

void fill_window_info(int* window_sizes,
                      int* window_starts,
                      int* window_stops,
                      SEXP window_indices,
                      int size);

struct window_info new_window_info(int*, int*, int);

int locate_window_starts_pos(struct index_info* index, struct range_info range, int pos);
int locate_window_stops_pos(struct index_info* index, struct range_info range, int pos);

void increment_window(struct window_info window,
                      struct index_info* index,
                      struct range_info range,
                      int pos);

// -----------------------------------------------------------------------------

int compute_min_iteration(struct index_info index, struct range_info range, bool complete);
int compute_max_iteration(struct index_info index, struct range_info range, bool complete);

// -----------------------------------------------------------------------------
#endif

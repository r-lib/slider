#ifndef SLIDER_INDEX_H
#define SLIDER_INDEX_H

#include "slider.h"

// -----------------------------------------------------------------------------

struct index_info {
  SEXP data;
  const int* p_data;
  int size;
  int last_pos;
  int current_start_pos;
  int current_stop_pos;
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
  const int* p_starts;
  const int* p_stops;
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
  const int* p_peer_sizes;
  const int* p_peer_starts;
  const int* p_peer_stops;
  SEXP seq;
  int* p_seq_val;
};

#define PROTECT_WINDOW_INFO(window, n) do {  \
  PROTECT((window)->seq);                    \
  *n += 1;                                   \
} while (0)

void fill_peer_info(const int* p_peer_sizes,
                    int size,
                    int* p_peer_starts,
                    int* p_peer_stops);

struct window_info new_window_info(const int* p_peer_sizes,
                                   const int* p_peer_starts,
                                   const int* p_peer_stops);

int locate_peer_starts_pos(struct index_info* index, struct range_info range, int pos);
int locate_peer_stops_pos(struct index_info* index, struct range_info range, int pos);

void increment_window(struct window_info window,
                      struct index_info* index,
                      struct range_info range,
                      int pos);

// -----------------------------------------------------------------------------

int compute_min_iteration(struct index_info index, struct range_info range, bool complete);
int compute_max_iteration(struct index_info index, struct range_info range, bool complete);

// -----------------------------------------------------------------------------
#endif
